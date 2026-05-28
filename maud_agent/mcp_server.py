"""
mcp_server.py - MCP (Model Context Protocol) Server for MAUD

This MCP server exposes MAUD's XRD structural refinement capabilities as
standardized tools that any MCP-compatible AI agent can call.

Usage:
    # Direct execution
    python maud_agent/mcp_server.py
    
    # Via MCP host (Claude Desktop, Cursor, etc.)
    mcp-dev run maud_agent/mcp_server.py

    # Configuration via environment variables:
    export MAUD_HOME=/path/to/maud     # MAUD installation directory
    export JAVA_HOME=/path/to/java      # Java JDK location

Tools exposed:
    - load_analysis          Load a .par analysis file
    - run_refinement         Run Rietveld refinement
    - run_compute            Compute without refinement
    - add_data_file          Add diffraction data
    - import_phase           Import crystal structure
    - save_analysis          Save refined results
    - export_plot            Export fit plot as PNG
    - export_pole_figures    Export pole figures
    - export_diff_data       Export diffraction data (CIF)
    - export_stress          Export stress analysis
    - batch_workflow         Run a complete multi-step workflow
    - generate_ins_script    Generate .ins instruction file only
"""

import json
import os
import sys
from typing import Any, Optional

try:
    from mcp.server import Server, stdio_server
except ImportError:
    print(
        "MCP SDK not found. Install with: pip install mcp",
        file=sys.stderr,
    )
    sys.exit(1)

from maud_agent.maud_wrapper import MaudBatch


def create_server() -> Server:
    """Create and configure the MAUD MCP server."""
    server = Server("maud-xrd-refinement")

    # ====================================================================
    # Tool: load_analysis
    # ====================================================================
    @server.tool()
    def load_analysis(
        filename: str,
        workdir: Optional[str] = None,
        title: Optional[str] = None,
    ) -> str:
        """
        Load a MAUD .par analysis file.

        The .par file is the main MAUD project file containing the
        complete analysis configuration (samples, phases, instruments,
        diffraction data, refinement parameters, etc.).

        Args:
            filename: Path to the .par analysis file
            workdir: Working directory (default: current)
            title: Optional title for the analysis
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(filename)
        if title:
            m.set_title(title)
        ins_path = m.generate_script("_load_analysis.ins")
        return json.dumps({
            "status": "queued",
            "analysis_file": filename,
            "ins_file": ins_path,
            "note": "Use run_refinement or run_compute to execute",
        })

    # ====================================================================
    # Tool: run_refinement
    # ====================================================================
    @server.tool()
    def run_refinement(
        analysis_file: str,
        iterations: int = 10,
        output_file: Optional[str] = None,
        data_file: Optional[str] = None,
        phase_file: Optional[str] = None,
        wizard_mode: int = 0,
        workdir: Optional[str] = None,
        timeout: int = 600,
    ) -> str:
        """
        Run Rietveld refinement on an analysis file.

        This is the main refinement tool. It loads an analysis,
        optionally adds data and phases, runs N iterations of
        least-squares refinement, and saves the results.

        Args:
            analysis_file: Path to the .par analysis file to refine
            iterations: Number of refinement iterations (default: 10)
            output_file: Path to save the refined .par file.
                         If None, overwrites the input.
            data_file: Optional .xy/.dat/.raw data file to add
            phase_file: Optional .cif/.par phase file to import
            wizard_mode: Refinement wizard mode:
                         0 = standard refinement
                        -1 = compute only (no refinement)
                         1-99 = wizard-guided refinement
            workdir: Working directory
            timeout: Max execution time in seconds
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        m.set_iterations(iterations)

        if data_file:
            m.add_data_file(data_file)
        if phase_file:
            m.import_phase(phase_file)
        if output_file:
            m.save_to(output_file)
        else:
            m.save_to(analysis_file)

        if wizard_mode != 0:
            m.set_wizard_index(wizard_mode)

        result = m.run(timeout=timeout)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "analysis_file": analysis_file,
            "output_file": result["ins_file"],
            "iterations": iterations,
            "rw": result.get("rw"),
            "rexp": result.get("rexp"),
            "gof": result.get("gof"),
            "exit_code": result.get("exit_code"),
            "error": result.get("error", ""),
            "output_summary": result.get("output", "")[:2000],
        })

    # ====================================================================
    # Tool: run_compute
    # ====================================================================
    @server.tool()
    def run_compute(
        analysis_file: str,
        output_file: Optional[str] = None,
        workdir: Optional[str] = None,
        timeout: int = 300,
    ) -> str:
        """
        Compute diffraction pattern without refinement.

        Use this to evaluate a model without optimizing parameters.
        Useful for initial model checking before refinement.

        Args:
            analysis_file: Path to .par analysis file
            output_file: Path to save computed result
            workdir: Working directory
            timeout: Max execution time in seconds
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        m.set_wizard_index(-1)  # Compute only
        if output_file:
            m.save_to(output_file)

        result = m.run(timeout=timeout)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "analysis_file": analysis_file,
            "compute_only": True,
            "exit_code": result.get("exit_code"),
            "error": result.get("error", "")[:500],
        })

    # ====================================================================
    # Tool: add_data_file
    # ====================================================================
    @server.tool()
    def add_data_file(
        analysis_file: str,
        data_file: str,
        output_file: str,
        replace: bool = False,
        workdir: Optional[str] = None,
    ) -> str:
        """
        Add a diffraction data file to an analysis.

        MAUD supports many formats:
        - .xy, .dat (two-column angle vs. intensity)
        - .raw (Philips/raw format)
        - .PRN (various column formats)
        - .F1B, .ddq (synchrotron formats)
        - .tif, .jpg (2D detector images)
        - .hdf (HDF5 format)
        - Various instrument-specific formats

        Args:
            analysis_file: Path to .par analysis file
            data_file: Path to the diffraction data file
            output_file: Path to save the updated analysis
            replace: If True, replace existing data files
            workdir: Working directory
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        m.add_data_file(data_file, replace=replace)
        m.save_to(output_file)

        result = m.run(timeout=120)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "analysis_file": analysis_file,
            "data_file": data_file,
            "output_file": output_file,
            "exit_code": result.get("exit_code"),
        })

    # ====================================================================
    # Tool: import_phase
    # ====================================================================
    @server.tool()
    def import_phase(
        analysis_file: str,
        phase_file: str,
        output_file: str,
        clear_existing: bool = False,
        workdir: Optional[str] = None,
    ) -> str:
        """
        Import a crystal structure phase into an analysis.

        Phase files can be:
        - .cif (Crystallographic Information File)
        - .par (MAUD phase definition)
        - .apf (MAUD atom position file)

        Args:
            analysis_file: Path to .par analysis file
            phase_file: Path to the phase/structure file (.cif, .par, .apf)
            output_file: Path to save the updated analysis
            clear_existing: If True, remove all existing phases first
            workdir: Working directory
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        if clear_existing:
            m.remove_all_phases()
        m.import_phase(phase_file)
        m.save_to(output_file)

        result = m.run(timeout=120)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "analysis_file": analysis_file,
            "phase_file": phase_file,
            "phases_cleared": clear_existing,
            "output_file": output_file,
        })

    # ====================================================================
    # Tool: save_analysis
    # ====================================================================
    @server.tool()
    def save_analysis(
        analysis_file: str,
        output_file: str,
        workdir: Optional[str] = None,
    ) -> str:
        """
        Save the current analysis state to a file.

        This loads an analysis and immediately saves it,
        useful for converting between formats or making copies.

        Args:
            analysis_file: Source .par file
            output_file: Destination path
            workdir: Working directory
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        m.save_to(output_file)

        result = m.run(timeout=60)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "source": analysis_file,
            "destination": output_file,
        })

    # ====================================================================
    # Tool: export_plot
    # ====================================================================
    @server.tool()
    def export_plot(
        analysis_file: str,
        plot_output: str,
        iterations: int = 0,
        workdir: Optional[str] = None,
    ) -> str:
        """
        Export the diffraction fit plot as PNG image(s).

        Generates one PNG per dataset showing observed vs. calculated
        diffraction patterns with difference curve.

        Args:
            analysis_file: Path to refined .par file
            plot_output: Base filename for PNG output (extensions added)
            iterations: If > 0, also run refinement before plotting
            workdir: Working directory
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        if iterations > 0:
            m.set_iterations(iterations)
        m.export_plot(plot_output)

        result = m.run(timeout=300)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "analysis_file": analysis_file,
            "plot_output": plot_output,
            "plot_count": "one per dataset",
        })

    # ====================================================================
    # Tool: export_pole_figures
    # ====================================================================
    @server.tool()
    def export_pole_figures(
        analysis_file: str,
        output_file: str,
        phases: str,
        workdir: Optional[str] = None,
    ) -> str:
        """
        Export pole figures for texture analysis.

        Pole figures describe the crystallographic orientation distribution
        of polycrystalline materials.

        Args:
            analysis_file: Path to refined .par file
            output_file: Output filename for pole figures
            phases: Phase and reflections specification in format
                    "phase_number h k l [h k l ...]"
                    Example: "0 1 1 1 2 0 0 2 2 0"
                    For multiple phases: "P0 1 1 1 2 0 0 P1 1 1 1"
            workdir: Working directory
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        m.export_pole_figures(output_file)
        m.export_pole_figures_options(phases)

        result = m.run(timeout=300)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "analysis_file": analysis_file,
            "pole_figure_file": output_file,
            "phases_spec": phases,
        })

    # ====================================================================
    # Tool: export_diff_data
    # ====================================================================
    @server.tool()
    def export_diff_data(
        analysis_file: str,
        output_file: str,
        workdir: Optional[str] = None,
    ) -> str:
        """
        Export experimental and computed diffraction data as CIF.

        The output CIF file contains observed and calculated intensities
        for all datasets, useful for archiving or further analysis.

        Args:
            analysis_file: Path to refined .par file
            output_file: Output .cif filename
            workdir: Working directory
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        m.export_diffraction_data(output_file)

        result = m.run(timeout=120)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "source": analysis_file,
            "output_cif": output_file,
        })

    # ====================================================================
    # Tool: export_stress
    # ====================================================================
    @server.tool()
    def export_stress(
        analysis_file: str,
        output_file: str,
        phases_hkl: str,
        workdir: Optional[str] = None,
    ) -> str:
        """
        Export residual stress analysis results.

        Uses the sin2psi method to compute stresses from diffraction data.

        Args:
            analysis_file: Path to refined .par file
            output_file: Output stress data file
            phases_hkl: Phase and reflection specification
                        Format: "phase_num h k l [h k l ...]"
                        Example: "0 2 1 1 2 2 0"
            workdir: Working directory
        """
        m = MaudBatch(workdir=workdir)
        m.load_analysis(analysis_file)
        m.export_stress(output_file)

        result = m.run(timeout=300)

        return json.dumps({
            "status": "completed" if result["success"] else "failed",
            "source": analysis_file,
            "stress_output": output_file,
        })

    # ====================================================================
    # Tool: batch_workflow
    # ====================================================================
    @server.tool()
    def batch_workflow(
        workflow_json: str,
        workdir: Optional[str] = None,
        timeout: int = 600,
    ) -> str:
        """
        Execute a complete multi-step refinement workflow.

        The workflow is a JSON array of steps. Each step can be:
        - {"action": "load", "file": "...", "out": "..."}
        - {"action": "refine", "file": "...", "iterations": 10, "out": "..."}
        - {"action": "compute", "file": "...", "out": "..."}
        - {"action": "add_data", "file": "...", "data": "...", "out": "..."}
        - {"action": "import_phase", "file": "...", "phase": "...", "out": "..."}
        - {"action": "plot", "file": "...", "output": "...", "png": "..."}
        - {"action": "export_diff", "file": "...", "output": "..."}
        - {"action": "export_pf", "file": "...", "output": "...", "phases_hkl": "..."}

        Example workflow:
        [
            {"action": "load", "file": "sample.par", "out": "step1.par"},
            {"action": "add_data", "file": "step1.par", "data": "my_data.xy", "out": "step2.par"},
            {"action": "import_phase", "file": "step2.par", "phase": "my_phase.cif", "out": "step3.par"},
            {"action": "refine", "file": "step3.par", "iterations": 20, "out": "final.par"},
            {"action": "plot", "file": "final.par", "png": "final_plot"},
            {"action": "export_diff", "file": "final.par", "output": "diffraction.cif"}
        ]

        Args:
            workflow_json: JSON string describing the workflow steps
            workdir: Working directory
            timeout: Max total execution time in seconds
        """
        try:
            steps = json.loads(workflow_json)
        except json.JSONDecodeError as e:
            return json.dumps({"status": "error", "error": f"Invalid JSON: {e}"})

        results = []
        for i, step in enumerate(steps):
            action = step.get("action", "")
            try:
                if action == "load":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.save_to(step.get("out", step["file"]))
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"], "exit_code": r.get("exit_code"),
                    })

                elif action == "refine":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.set_iterations(step.get("iterations", 10))
                    m.save_to(step.get("out", step["file"]))
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"],
                        "rw": r.get("rw"), "gof": r.get("gof"),
                    })

                elif action == "compute":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.set_wizard_index(-1)
                    m.save_to(step.get("out", step["file"]))
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"],
                    })

                elif action == "add_data":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.add_data_file(step["data"])
                    m.save_to(step.get("out", step["file"]))
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"],
                    })

                elif action == "import_phase":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.import_phase(step["phase"])
                    m.save_to(step.get("out", step["file"]))
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"],
                    })

                elif action == "plot":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.export_plot(step["png"])
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"],
                    })

                elif action == "export_diff":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.export_diffraction_data(step["output"])
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"],
                    })

                elif action == "export_pf":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.export_pole_figures(step["output"])
                    if step.get("phases_hkl"):
                        m.export_pole_figures_options(step["phases_hkl"])
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"],
                    })

                elif action == "export_stress":
                    m = MaudBatch(workdir=workdir)
                    m.load_analysis(step["file"])
                    m.export_stress(step["output"])
                    r = m.run(timeout=timeout)
                    results.append({
                        "step": i, "action": action,
                        "success": r["success"],
                    })

                else:
                    results.append({
                        "step": i, "action": action,
                        "success": False,
                        "error": f"Unknown action: {action}",
                    })

            except Exception as e:
                results.append({
                    "step": i, "action": action,
                    "success": False,
                    "error": str(e),
                })

        all_success = all(r.get("success") for r in results)
        return json.dumps({
            "status": "completed" if all_success else "partial",
            "all_success": all_success,
            "steps_completed": len(results),
            "steps": results,
        })

    # ====================================================================
    # Tool: generate_ins_script
    # ====================================================================
    @server.tool()
    def generate_ins_script(
        commands_json: str,
        output_file: str = "maud_instructions.ins",
        workdir: Optional[str] = None,
    ) -> str:
        """
        Generate a MAUD .ins instruction file without executing it.

        This is useful for inspecting the instruction file that will be
        generated, or for manual submission.

        The commands_json should be a list of {"command": "...", "value": "..."}
        objects, where command is one of:
            "analysis_file", "iterations", "save_file", "data_file",
            "import_phase", "plot_output", "pole_figures_file",
            "diff_data_output", "stress_file", etc.

        Args:
            commands_json: JSON array of command objects
            output_file: Path for the generated .ins file
            workdir: Working directory
        """
        try:
            commands = json.loads(commands_json)
        except json.JSONDecodeError as e:
            return json.dumps({"status": "error", "error": f"Invalid JSON: {e}"})

        m = MaudBatch(workdir=workdir)
        for cmd in commands:
            name = cmd.get("command", cmd.get("name", ""))
            value = str(cmd.get("value", cmd.get("val", "")))
            if not name:
                continue

            method_map = {
                "analysis_file": lambda v: m.load_analysis(v),
                "iterations": lambda v: m.set_iterations(int(v)),
                "save_file": lambda v: m.save_to(v),
                "data_file": lambda v: m.add_data_file(v),
                "import_phase": lambda v: m.import_phase(v),
                "plot_output": lambda v: m.export_plot(v),
                "pole_figures_file": lambda v: m.export_pole_figures(v),
                "diff_data_output": lambda v: m.export_diffraction_data(v),
                "stress_file": lambda v: m.export_stress(v),
                "remove_all_data": lambda v: m.remove_all_data_files(),
                "remove_all_phases": lambda v: m.remove_all_phases(),
                "auto_background": lambda v: m.set_auto_background(v.lower() == "true"),
                "title": lambda v: m.set_title(v),
                "working_dir": lambda v: m.set_working_directory(v),
            }

            method = method_map.get(name)
            if method:
                method(value)

        ins_path = m.generate_script(output_file)

        return json.dumps({
            "status": "generated",
            "ins_file": ins_path,
            "command_count": len(commands),
            "content": Path(ins_path).read_text(encoding="utf-8"),
        })

    from pathlib import Path

    return server


def main():
    """Run the MCP server over stdio."""
    server = create_server()
    server.run(stdio_server())


if __name__ == "__main__":
    main()
