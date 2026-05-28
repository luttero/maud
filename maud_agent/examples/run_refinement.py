"""
Example: Run a full Rietveld refinement workflow using the Python API.

Run: python maud_agent/examples/run_refinement.py
"""

import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from maud_agent.maud_pipeline import MaudPipeline


def main():
    # Initialize the pipeline
    p = MaudPipeline(
        # maud_home="/path/to/maud",  # Set MAUD_HOME or pass here
        workdir=".",
    )

    # Full refinement workflow
    result = p.full_refinement(
        par_file="sample.par",
        data_file="sample.xy",
        phase_file="structure.cif",  # Optional
        iterations=20,
        output_par="sample_refined.par",
        export_plot="fit_plot",
        export_diff_cif="diffraction.cif",
        timeout=600,
    )

    print(f"Success: {result['success']}")
    if result.get("rw"):
        print(f"Rwp: {result['rw']:.5f}")
    if result.get("gof"):
        print(f"GoF: {result['gof']:.5f}")

    # Compare different iteration counts
    convergence = p.compare_methods(
        par_file="sample.par",
        data_file="sample.xy",
        iterations_list=[5, 10, 20, 50],
        output_dir="./convergence_test",
    )

    print(f"\nConvergence test ({convergence['total_runs']} runs):")
    for c in convergence["convergence"]:
        print(f"  {c['iterations']} iters: Rwp={c['rw']}, GoF={c['gof']}")


if __name__ == "__main__":
    main()
