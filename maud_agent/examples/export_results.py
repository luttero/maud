"""
Example: Export pole figures and stress data from a refined analysis.

Run: python maud_agent/examples/export_results.py
"""

import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from maud_agent.maud_pipeline import MaudPipeline


def main():
    p = MaudPipeline(workdir=".")

    # Export pole figures for texture analysis
    pf_result = p.texture_analysis(
        par_file="refined.par",
        output_pole_figures="pole_figures.xpc",
        phase_number=0,
        hkl_reflections=[
            (1, 1, 1),
            (2, 0, 0),
            (2, 2, 0),
            (3, 1, 1),
        ],
        iterations=0,  # Don't refine again, just export
    )
    print(f"Pole figures exported: {pf_result['success']}")

    # Export stress analysis
    stress_result = p.stress_analysis(
        par_file="refined.par",
        output_stress="stress_data.txt",
        phase_number=0,
        hkl_reflections=[
            (2, 1, 1),
            (2, 2, 0),
        ],
    )
    print(f"Stress data exported: {stress_result['success']}")


if __name__ == "__main__":
    main()
