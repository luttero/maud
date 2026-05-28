"""
maud_pipeline.py - High-level pipeline for automated XRD refinement

Provides convenience pipelines for common XRD analysis workflows.
Each pipeline combines multiple MAUD steps into a single call.
"""

from typing import Optional, Dict, Any
from .maud_wrapper import MaudBatch


class MaudPipeline:
    """
    High-level XRD refinement pipeline.

    Wraps common multi-step workflows into single method calls.
    """

    def __init__(
        self,
        maud_home: Optional[str] = None,
        workdir: Optional[str] = None,
    ):
        self.maud_home = maud_home
        self.workdir = workdir

    def _batch(self) -> MaudBatch:
        return MaudBatch(
            maud_home=self.maud_home,
            workdir=self.workdir,
        )

    def full_refinement(
        self,
        par_file: str,
        data_file: str,
        phase_file: Optional[str] = None,
        iterations: int = 20,
        output_par: Optional[str] = None,
        export_plot: Optional[str] = None,
        export_diff_cif: Optional[str] = None,
        timeout: int = 600,
    ) -> Dict[str, Any]:
        """
        Full Rietveld refinement pipeline.

        Loads analysis, adds data, optionally imports phase,
        runs refinement, and exports results.

        Args:
            par_file: Input .par analysis file
            data_file: Diffraction data file
            phase_file: Optional phase/crystal structure file
            iterations: Number of refinement cycles
            output_par: Output refined .par file (default: input + "_refined.par")
            export_plot: Optional PNG plot export path
            export_diff_cif: Optional CIF export path
            timeout: Max execution time (seconds)
        """
        if output_par is None:
            output_par = par_file.replace(".par", "_refined.par")

        # Build instruction sequence
        m = self._batch()
        m.load_analysis(par_file)
        m.remove_all_data_files()
        m.add_data_file(data_file)
        if phase_file:
            m.import_phase(phase_file)
        m.set_iterations(iterations)
        m.save_to(output_par)

        if export_plot:
            m.export_plot(export_plot)
        if export_diff_cif:
            m.export_diffraction_data(export_diff_cif)

        result = m.run(timeout=timeout)

        return {
            "success": result["success"],
            "input": par_file,
            "data": data_file,
            "phase": phase_file,
            "output": output_par,
            "iterations": iterations,
            "rw": result.get("rw"),
            "gof": result.get("gof"),
            "exit_code": result.get("exit_code"),
            "error": result.get("error", ""),
        }

    def batch_sequential(
        self,
        par_files: list,
        output_dir: str,
        iterations: int = 10,
        timeout_per: int = 300,
    ) -> Dict[str, Any]:
        """
        Run sequential refinement on multiple sample files.

        Args:
            par_files: List of .par file paths
            output_dir: Directory for output files
            iterations: Number of refinement cycles per sample
            timeout_per: Timeout per refinement (seconds)
        """
        results = []
        for i, par_file in enumerate(par_files):
            import os
            base = os.path.basename(par_file).replace(".par", "")
            out_par = os.path.join(output_dir, f"{base}_refined.par")
            r = self.full_refinement(
                par_file=par_file,
                data_file="",
                iterations=iterations,
                output_par=out_par,
                timeout=timeout_per,
            )
            r["index"] = i
            results.append(r)

        return {
            "total": len(results),
            "successful": sum(1 for r in results if r["success"]),
            "failed": sum(1 for r in results if not r["success"]),
            "results": results,
        }

    def texture_analysis(
        self,
        par_file: str,
        output_pole_figures: str,
        phase_number: int = 0,
        hkl_reflections: Optional[list] = None,
        iterations: int = 10,
        timeout: int = 600,
    ) -> Dict[str, Any]:
        """
        Run refinement and extract pole figures for texture analysis.

        Args:
            par_file: Input .par file
            output_pole_figures: Output path for pole figure data
            phase_number: Phase index (0 = first phase)
            hkl_reflections: List of (h,k,l) tuples for pole figure calculation
                             Default: [(1,1,1), (2,0,0), (2,2,0)]
            iterations: Number of refinement cycles
            timeout: Max execution time (seconds)
        """
        if hkl_reflections is None:
            hkl_reflections = [(1, 1, 1), (2, 0, 0), (2, 2, 0)]

        m = self._batch()
        m.load_analysis(par_file)
        if iterations > 0:
            m.set_iterations(iterations)
        m.export_pole_figures(
            output_pole_figures,
            {phase_number: hkl_reflections},
        )

        result = m.run(timeout=timeout)

        return {
            "success": result["success"],
            "input": par_file,
            "pole_figures": output_pole_figures,
            "phase": phase_number,
            "hkl": hkl_reflections,
            "rw": result.get("rw"),
            "exit_code": result.get("exit_code"),
        }

    def stress_analysis(
        self,
        par_file: str,
        output_stress: str,
        phase_number: int = 0,
        hkl_reflections: Optional[list] = None,
        iterations: int = 10,
        timeout: int = 600,
    ) -> Dict[str, Any]:
        """
        Run refinement and extract residual stress data.

        Args:
            par_file: Input .par file
            output_stress: Output path for stress data
            phase_number: Phase index (0 = first phase)
            hkl_reflections: List of (h,k,l) for sin2psi analysis
            iterations: Number of refinement cycles
            timeout: Max execution time (seconds)
        """
        if hkl_reflections is None:
            hkl_reflections = [(2, 1, 1), (2, 2, 0), (3, 1, 0)]

        m = self._batch()
        m.load_analysis(par_file)
        if iterations > 0:
            m.set_iterations(iterations)
        m.export_stress(
            output_stress,
            {phase_number: hkl_reflections},
        )

        result = m.run(timeout=timeout)

        return {
            "success": result["success"],
            "input": par_file,
            "stress_output": output_stress,
            "phase": phase_number,
            "hkl": hkl_reflections,
            "rw": result.get("rw"),
            "exit_code": result.get("exit_code"),
        }

    def compare_methods(
        self,
        par_file: str,
        data_file: str,
        iterations_list: list,
        output_dir: str,
        timeout_per: int = 300,
    ) -> Dict[str, Any]:
        """
        Run refinement with different iteration counts and compare results.

        Useful for convergence studies.

        Args:
            par_file: Template .par file
            data_file: Diffraction data file
            iterations_list: E.g., [5, 10, 20, 50]
            output_dir: Output directory
            timeout_per: Timeout per run
        """
        import os
        results = []
        for iters in iterations_list:
            out = os.path.join(
                output_dir,
                f"refined_{iters}it.par",
            )
            r = self.full_refinement(
                par_file=par_file,
                data_file=data_file,
                iterations=iters,
                output_par=out,
                timeout=timeout_per,
            )
            r["iterations"] = iters
            results.append(r)

        return {
            "total_runs": len(results),
            "iterations_tested": iterations_list,
            "convergence": [
                {"iterations": r["iterations"], "rw": r.get("rw"), "gof": r.get("gof")}
                for r in results if r["success"]
            ],
            "results": results,
        }
