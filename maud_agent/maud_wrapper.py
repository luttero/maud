"""
maud_wrapper.py - Python wrapper for MAUD batch processing interface

This module provides a programmatic Python interface to control MAUD
(XRD structural refinement software) via its batch instruction file system.

MAUD accepts .ins (instruction) files with CIF-like tokens that define
the entire refinement workflow: loading data, setting parameters, running
refinement, and exporting results.

Usage:
    from maud_agent.maud_wrapper import MaudBatch

    m = MaudBatch(maud_jar="/path/to/Maud.jar")
    m.load_analysis("my_sample.par")
    m.set_iterations(20)
    m.add_data_file("my_data.xy")
    m.run()
    m.save_analysis("refined.par")
"""

import os
import subprocess
import tempfile
from pathlib import Path
from typing import Optional, List, Dict, Union


class MaudBatch:
    """
    Programmatic interface to MAUD's batch processing mode.

    MAUD is launched as: java com.radiographema.MaudText -f instruction.ins
    The .ins file contains CIF-like tokens that describe the entire workflow.
    """

    # Mapping from MAUD CIF tokens to their purpose
    CMD_MAP = {
        "analysis_file": "_riet_analysis_file",
        "iterations": "_riet_analysis_iteration_number",
        "wizard_index": "_riet_analysis_wizard_index",
        "save_file": "_riet_analysis_fileToSave",
        "data_file": "_riet_meas_datafile_name",
        "simple_result": "_riet_append_simple_result_to",
        "full_result": "_riet_append_result_to",
        "replace_data": "_riet_meas_datafile_replace",
        "auto_background": "_maud_background_add_automatic",
        "plot_output": "_maud_output_plot_filename",
        "remove_all_data": "_maud_remove_all_datafiles",
        "remove_all_phases": "_maud_remove_all_phases",
        "import_phase": "_maud_import_phase",
        "pole_figures_file": "_maud_export_pole_figures_filename",
        "pole_figures_options": "_maud_export_pole_figures_options",
        "pole_figures_export": "_maud_export_pole_figures",
        "plot2d_output": "_maud_output_plot2D_filename",
        "title": "_publ_section_title",
        "stress_file": "_maud_output_stress_filename",
        "stress_options": "_maud_output_stress_options",
        "instrument_script": "_riet_meas_datains_name",
        "diff_data_output": "_maud_output_diff_data_filename",
        "dataset_number": "_pd_meas_dataset_number",
        "working_dir": "_maud_working_directory",
    }

    # Wizard modes
    WIZARD_COMPUTE_ONLY = -1       # Only compute, no refinement
    WIZARD_REFINE = 0              # Full refinement (default)
    WIZARD_REFINE_AND_SAVE = -2    # Refine then save
    WIZARD_REFINE_NO_SAVE = 999    # Refine, output to stdout only
    WIZARD_WIZARD = 1              # Use refinement wizard

    def __init__(
        self,
        maud_home: Optional[str] = None,
        java_home: Optional[str] = None,
        java_opts: Optional[List[str]] = None,
        workdir: Optional[str] = None,
    ):
        """
        Initialize MAUD batch wrapper.

        Args:
            maud_home: Path to MAUD installation directory.
                       If None, checks MAUD_HOME env var, then common paths.
            java_home: Path to Java installation. If None, uses JAVA_HOME or 'java'.
            java_opts: Additional JVM options (e.g., ["-Xmx4g"])
            workdir: Working directory for MAUD
        """
        self.maud_home = maud_home or os.environ.get("MAUD_HOME")
        self.java_home = java_home
        self.java_opts = java_opts or ["-Xmx2g"]
        self._workdir = workdir or os.getcwd()

        self._java = self._find_java()
        self._classpath = self._build_classpath()

        self._cif_items: List[tuple] = []
        self._loop_items: List[List[tuple]] = []

    @staticmethod
    def _find_java() -> str:
        java = os.environ.get("JAVA_HOME")
        if java:
            return os.path.join(java, "bin", "java")
        java = os.environ.get("JAVA_HOME_22") or os.environ.get("JAVA_HOME_19")
        if java:
            return os.path.join(java, "bin", "java")
        return "java"

    def _build_classpath(self) -> str:
        """Build MAUD classpath from installation directory."""
        cp_parts = []

        if self.maud_home:
            # Check for Maud.jar in various locations
            for jar_candidate in [
                os.path.join(self.maud_home, "Maud.jar"),
                os.path.join(self.maud_home, "lib", "Maud.jar"),
                os.path.join(self.maud_home, "dist", "Maud.jar"),
            ]:
                if os.path.isfile(jar_candidate):
                    cp_parts.append(jar_candidate)
                    break

            # Add all jars from lib/
            lib_dir = os.path.join(self.maud_home, "lib")
            if os.path.isdir(lib_dir):
                for f in sorted(os.listdir(lib_dir)):
                    if f.endswith(".jar"):
                        cp_parts.append(os.path.join(lib_dir, f))

        # Also check build/ directory (from compiling source)
        build_dir = os.path.join(os.getcwd(), "build")
        if os.path.isdir(build_dir):
            cp_parts.append(os.path.join(build_dir, "classes"))
            for root, dirs, files in os.walk(build_dir):
                for f in files:
                    if f.endswith(".jar"):
                        cp_parts.append(os.path.join(root, f))

        # Add CWD/src so MAUD can find resources
        src_dir = os.path.join(os.getcwd(), "src")
        if os.path.isdir(src_dir):
            cp_parts.append(src_dir)

        return os.pathsep.join(cp_parts) if cp_parts else os.getcwd()

    def _generate_ins(self) -> str:
        """
        Generate the .ins instruction file content from the queued commands.
        """
        lines = ["data_MAUD_batch"]
        for tag, value in self._cif_items:
            lines.append(f"{tag} {value}")

        if self._loop_items:
            lines.append("loop_")
            # First pass: emit all tag names
            for block in self._loop_items:
                for tag, _ in block:
                    lines.append(tag)
                break  # tags only once

            # Second pass: emit values row by row
            for row_idx, block in enumerate(self._loop_items):
                for _, value in block:
                    lines.append(value)

        lines.append("")
        return "\n".join(lines)

    def _quote(self, value: str) -> str:
        """Quote a value for CIF format if it contains spaces."""
        if " " in value or "'" in value:
            return f"'{value}'" if '"' not in value else f'"{value}"'
        return value

    def _check_maud_home(self):
        """Raise if MAUD_HOME is not set and Maud.jar cannot be found."""
        if not self._classpath or "Maud.jar" not in self._classpath:
            if not self.maud_home:
                raise RuntimeError(
                    "MAUD_HOME not set. Set MAUD_HOME environment variable "
                    "or pass maud_home= to MaudBatch().\n"
                    "Expected to find Maud.jar in $MAUD_HOME/ or $MAUD_HOME/lib/"
                )

    def _build_cmd(self, ins_file: str) -> List[str]:
        """Build the command to run MAUD in batch mode."""
        cmd = [self._java] + self.java_opts
        if self._classpath:
            cmd.extend(["-cp", self._classpath])
        cmd.append("com.radiographema.MaudText")
        cmd.extend(["-f", ins_file])
        return cmd

    # ====================================================================
    # Public API - CIF item commands
    # ====================================================================

    def set_working_directory(self, path: str):
        """Set the working directory for file resolution."""
        self._cif_items.append(
            (self.CMD_MAP["working_dir"], self._quote(path))
        )

    def load_analysis(self, filename: str):
        """
        Load a .par analysis file (the main MAUD project file).

        This is the first thing you must do. The .par file contains
        the full analysis configuration (phases, instruments, etc.).
        """
        self._cif_items.append(
            (self.CMD_MAP["analysis_file"], self._quote(filename))
        )

    def set_iterations(self, n: int):
        """Set the number of refinement iterations."""
        self._cif_items.append(
            (self.CMD_MAP["iterations"], str(n))
        )

    def save_to(self, filename: str):
        """Set the output filename for the refined analysis."""
        self._cif_items.append(
            (self.CMD_MAP["save_file"], self._quote(filename))
        )

    def add_data_file(self, filename: str, replace: bool = False):
        """
        Add a diffraction data file to the current dataset.

        Args:
            filename: Path to the data file (.xy, .raw, .dat, .PRN, etc.)
            replace: If True, replaces existing data files
        """
        if replace:
            self._cif_items.append(
                (self.CMD_MAP["replace_data"], "true")
            )
        self._cif_items.append(
            (self.CMD_MAP["data_file"], self._quote(filename))
        )

    def import_phase(self, filename: str):
        """
        Import a phase from a .cif or .par file.

        The phase file should contain the crystal structure definition.
        """
        self._cif_items.append(
            (self.CMD_MAP["import_phase"], self._quote(filename))
        )

    def remove_all_data_files(self):
        """Remove all data files from the analysis."""
        self._cif_items.append(
            (self.CMD_MAP["remove_all_data"], "true")
        )

    def remove_all_phases(self):
        """Remove all phases from the analysis."""
        self._cif_items.append(
            (self.CMD_MAP["remove_all_phases"], "true")
        )

    def set_auto_background(self, enabled: bool = True):
        """Enable or disable automatic polynomial background."""
        self._cif_items.append(
            (self.CMD_MAP["auto_background"], "true" if enabled else "false")
        )

    def set_wizard_index(self, index: int):
        """
        Set refinement wizard mode.

        -1 = compute only (no refinement)
         0 = refine (default)
         1-99 = wizard modes (complex multi-step strategies)
        """
        self._cif_items.append(
            (self.CMD_MAP["wizard_index"], str(index))
        )

    def set_title(self, title: str):
        """Set the title/description for this analysis."""
        self._cif_items.append(
            (self.CMD_MAP["title"], self._quote(title))
        )

    def set_dataset_number(self, n: int):
        """Select which dataset to operate on (0-indexed)."""
        self._cif_items.append(
            (self.CMD_MAP["dataset_number"], str(n))
        )

    # ====================================================================
    # Export commands
    # ====================================================================

    def export_plot(self, filename: str):
        """
        Export the diffraction fit plot as PNG.

        One PNG per dataset is generated with filename + dataset index.
        """
        self._cif_items.append(
            (self.CMD_MAP["plot_output"], self._quote(filename))
        )

    def export_plot_2d(self, filename: str):
        """Export 2D diffraction plot as PNG."""
        self._cif_items.append(
            (self.CMD_MAP["plot2d_output"], self._quote(filename))
        )

    def export_pole_figures(self, filename: str, phase_hkl_pairs: Optional[Dict[int, List[tuple]]] = None):
        """
        Export pole figures for one or more phases.

        Args:
            filename: Output filename for pole figures (.xpc or .png)
            phase_hkl_pairs: Dict mapping phase_number -> list of (h,k,l) tuples
                             e.g., {0: [(1,1,1), (2,0,0), (2,2,0)]}

        If phase_hkl_pairs is None, only sets the output filename.
        """
        self._cif_items.append(
            (self.CMD_MAP["pole_figures_file"], self._quote(filename))
        )
        if phase_hkl_pairs:
            xpc_parts = []
            for phase_num, hkl_list in phase_hkl_pairs.items():
                hkl_str = " ".join(
                    f"{h} {k} {l}" for h, k, l in hkl_list
                )
                xpc_parts.append(f"P{phase_num} {hkl_str}")
            self._cif_items.append(
                (self.CMD_MAP["pole_figures_export"],
                 self._quote(" ".join(xpc_parts)))
            )

    def export_pole_figures_options(self, options: str):
        """Set pole figures export options."""
        self._cif_items.append(
            (self.CMD_MAP["pole_figures_options"], self._quote(options))
        )

    def export_stress(self, filename: str, phase_hkl_pairs: Optional[Dict[int, List[tuple]]] = None):
        """
        Export stress data using sin2psi method.

        Args:
            filename: Output stress data filename
            phase_hkl_pairs: Dict mapping phase_number -> list of (h,k,l) tuples
        """
        self._cif_items.append(
            (self.CMD_MAP["stress_file"], self._quote(filename))
        )
        if phase_hkl_pairs:
            options_parts = []
            for phase_num, hkl_list in phase_hkl_pairs.items():
                hkl_str = " ".join(
                    f"{h} {k} {l}" for h, k, l in hkl_list
                )
                options_parts.append(f"P{phase_num} {hkl_str}")
            self._cif_items.append(
                (self.CMD_MAP["stress_options"],
                 self._quote(" ".join(options_parts)))
            )

    def export_diffraction_data(self, filename: str):
        """Export experimental and computed diffraction data as CIF."""
        self._cif_items.append(
            (self.CMD_MAP["diff_data_output"], self._quote(filename))
        )

    def append_simple_results(self, filename: str):
        """Append simple summary results to a text file."""
        self._cif_items.append(
            (self.CMD_MAP["simple_result"], self._quote(filename))
        )

    def append_full_results(self, filename: str):
        """Append full results to a text file."""
        self._cif_items.append(
            (self.CMD_MAP["full_result"], self._quote(filename))
        )

    def add_instrument_script(self, filename: str):
        """Load instrument configuration from a script file."""
        self._cif_items.append(
            (self.CMD_MAP["instrument_script"], self._quote(filename))
        )

    # ====================================================================
    # Batch loop commands (for processing multiple files in one run)
    # ====================================================================

    def begin_batch_loop(self):
        """Start a batch loop to process multiple files in one MAUD run."""
        self._current_loop_block = []

    def add_to_batch_loop(self, analysis_file: str, **extra_items):
        """
        Add an entry to the batch loop.

        Each entry should at minimum have an analysis file.
        """
        block = [(self.CMD_MAP["analysis_file"], self._quote(analysis_file))]
        for tag, value in extra_items.items():
            token = self.CMD_MAP.get(tag, tag)
            block.append((token, self._quote(value)))
        self._loop_items.append(block)

    def end_batch_loop(self):
        """End the batch loop."""
        self._current_loop_block = None

    # ====================================================================
    # Run
    # ====================================================================

    def generate_script(self, output_file: str) -> str:
        """
        Generate the .ins instruction file and return the path.
        Use this to inspect the generated script before running.
        """
        content = self._generate_ins()
        Path(output_file).write_text(content, encoding="utf-8")
        return output_file

    def run(
        self,
        ins_file: Optional[str] = None,
        timeout: int = 600,
        capture_output: bool = True,
        dry_run: bool = False,
    ) -> Dict:
        """
        Execute the MAUD batch process.

        Args:
            ins_file: Path for the .ins file. If None, creates a temp file.
            timeout: Max execution time in seconds.
            capture_output: If True, returns stdout/stderr.
            dry_run: If True, only generate the .ins file without running.

        Returns:
            Dict with keys: success, ins_file, command, output, error, exit_code
        """
        self._check_maud_home()

        if ins_file is None:
            tmp = tempfile.NamedTemporaryFile(
                suffix=".ins", delete=False, mode="w"
            )
            ins_file = tmp.name
            tmp.write(self._generate_ins())
            tmp.close()
        else:
            self.generate_script(ins_file)

        cmd = self._build_cmd(ins_file)

        result = {
            "success": False,
            "ins_file": ins_file,
            "command": " ".join(cmd),
            "ins_content": Path(ins_file).read_text(encoding="utf-8"),
        }

        if dry_run:
            result["success"] = True
            return result

        try:
            proc = subprocess.run(
                cmd,
                cwd=self._workdir,
                capture_output=capture_output,
                text=True,
                timeout=timeout,
            )
            result["exit_code"] = proc.returncode
            result["output"] = proc.stdout if capture_output else ""
            result["error"] = proc.stderr if capture_output else ""
            result["success"] = proc.returncode == 0

            # Parse key results from output
            result["rw"] = self._parse_rw(proc.stdout if capture_output else "")
            result["rexp"] = self._parse_rexp(proc.stdout if capture_output else "")
            result["gof"] = self._parse_gof(proc.stdout if capture_output else "")

        except subprocess.TimeoutExpired:
            result["exit_code"] = -1
            result["error"] = f"Process timed out after {timeout}s"
        except FileNotFoundError:
            result["error"] = (
                f"Java not found. Tried: {self._java}\n"
                "Set JAVA_HOME or install JDK."
            )

        return result

    @staticmethod
    def _parse_rw(output: str) -> Optional[float]:
        """Parse Rwp from MAUD output."""
        import re
        m = re.search(r"Rwp\s*[:=]\s*([\d.]+)", output, re.IGNORECASE)
        if m:
            return float(m.group(1))
        m = re.search(r"_refine_ls_R_factor_all\s+([\d.]+)", output)
        if m:
            return float(m.group(1))
        return None

    @staticmethod
    def _parse_rexp(output: str) -> Optional[float]:
        """Parse Rexp from MAUD output."""
        import re
        m = re.search(r"Rexp\s*[:=]\s*([\d.]+)", output, re.IGNORECASE)
        if m:
            return float(m.group(1))
        return None

    @staticmethod
    def _parse_gof(output: str) -> Optional[float]:
        """Parse goodness of fit from MAUD output."""
        import re
        m = re.search(
            r"_refine_ls_goodness_of_fit_all\s+([\d.]+)", output
        )
        if m:
            return float(m.group(1))
        return None
