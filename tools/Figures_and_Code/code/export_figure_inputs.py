"""Prepare private plotting inputs from the BrainNormR analysis workbooks."""

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd


PLOT_ROOT = Path(__file__).resolve().parents[1]
RESULTS = PLOT_ROOT.parent / "BrainNormR" / "Results"
ANATOMICAL_LEVELS = ("WholeBrain", "Hemisphere", "Lobe", "ROI")
AGE_BREAKS = (19, 30, 40, 50, 60, 78)
PROFILE_SHEETS = (
    "Participant_Summary", "Profile_Matrix", "Null_Outcome_Samples", "Null_Block_Samples",
    "Null_Summary", "Stability_Iterations", "Threshold_Pairwise", "Threshold_TopDecile",
)


def bh_adjust(p_values):
    """Return Benjamini–Hochberg adjusted p values in their original order."""
    p = np.asarray(p_values, dtype=float)
    if not np.all(np.isfinite(p) & (0 <= p) & (p <= 1)):
        raise ValueError("The reported test sets require finite p values in [0, 1].")
    order = np.argsort(p, kind="stable")
    ranked = p[order] * len(p) / np.arange(1, len(p) + 1)
    q = np.empty_like(p)
    q[order] = np.minimum(1.0, np.minimum.accumulate(ranked[::-1])[::-1])
    return q


def structural_only(frame):
    return frame.loc[frame.Level.isin(ANATOMICAL_LEVELS)].copy()


def export_figure_inputs(summary_file, curves_file, periods_file, profile_file, master_file, target):
    summary = structural_only(pd.read_excel(summary_file, sheet_name="Summary"))
    curves = structural_only(pd.read_excel(curves_file, sheet_name="CentileCurves"))
    with pd.ExcelFile(periods_file) as book:
        periods = structural_only(book.parse("Structural_PeriodChange"))
        omnibus = structural_only(book.parse("ANCOVA_Omnibus"))
        adjacent = structural_only(book.parse("ANCOVA_AdjacentContrasts"))

    by_outcome = curves.groupby("Outcome").Age.agg(["count", "min", "max"])
    if (len(summary) != 445 or not summary.Outcome.is_unique
            or set(by_outcome.index) != set(summary.Outcome)
            or not by_outcome.eq([400, 19, 77]).to_numpy().all()
            or curves.duplicated(["Outcome", "Age"]).any()
            or len(periods) != 2225 or periods.duplicated(["Outcome", "PeriodLabel"]).any()
            or len(omnibus) != 445 or not omnibus.Column.is_unique
            or len(adjacent) != 1780 or adjacent.duplicated(["Column", "contrast"]).any()
            or any(set(ids) != set(summary.Outcome) for ids in (periods.Outcome, omnibus.Column, adjacent.Column))):
        raise ValueError("Expected 445 complete reference outcomes, 2,225 periods, 445 omnibus tests, and 1,780 contrasts.")

    summary["AgeSex_q_BH"] = summary.groupby(["OutcomeType", "Level"]).AgeSex_P.transform(bh_adjust)
    omnibus["q_BH_LevelMeasure"] = omnibus.groupby(["Level", "Measure"]).p_raw.transform(bh_adjust)
    adjacent["q_BH_LevelMeasureContrast"] = adjacent.groupby(["Level", "Measure", "contrast"]).p_raw.transform(bh_adjust)
    for table, family_column in ((omnibus, "q_BH_LevelMeasure"), (adjacent, "q_BH_LevelMeasureContrast")):
        table["q_BH_Global"] = bh_adjust(table.p_raw)
        table["Significant_FDR"] = table[family_column].lt(0.05)

    endpoints = curves.loc[curves.Age.isin([19, 77])].pivot(index="Outcome", columns="Age", values="P50")
    medians = endpoints.reindex(summary.Outcome)
    summary["P50_Age19"] = medians[19].to_numpy()
    summary["P50_Age77"] = medians[77].to_numpy()
    summary["P50Change_19to77_perSampleSD"] = (summary.P50_Age77 - summary.P50_Age19) / summary.SampleSD
    summary["P50Change_19to77_Percent"] = 100 * (summary.P50_Age77 / summary.P50_Age19 - 1)

    with pd.ExcelFile(profile_file) as book:
        profile_data = {name: book.parse(name) for name in PROFILE_SHEETS}
    participants = pd.read_excel(master_file, sheet_name="NormativeScores_All", usecols=["SubjectID", "Age"])
    participants = participants.drop_duplicates()
    if (len(participants) != 293 or participants.SubjectID.isna().any()
            or not participants.SubjectID.is_unique or not participants.Age.between(19, 77).all()):
        raise ValueError("Expected consistent ages for all 293 participants.")
    group_counts = np.histogram(participants.Age, bins=AGE_BREAKS)[0].tolist()

    target.mkdir(parents=True, exist_ok=True)
    tables = {"summary": summary, "periods": periods, "omnibus": omnibus, "adjacent": adjacent, **profile_data}
    for name, frame in tables.items():
        frame.drop(columns=["SourceFile"], errors="ignore").to_csv(target / f"{name}.csv", index=False)
    curves.drop(columns=["SourceFile"], errors="ignore").to_csv(target / "centiles.csv.gz", index=False)
    (target / "age_histogram.json").write_text(json.dumps({"group_counts": group_counts}) + "\n", encoding="utf-8")
    print(f"Prepared local plotting inputs for {len(summary)} outcomes in {target}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--summary", type=Path, default=RESULTS / "Thesis_Outputs" / "MASTER_RESULTS.xlsx")
    parser.add_argument("--curves", type=Path, default=RESULTS / "Thesis_Outputs" / "MASTER_CURVES.xlsx")
    parser.add_argument("--periods", type=Path, default=RESULTS / "AgePeriod_Analysis" / "AGE_PERIOD_ANALYSIS_RESULTS.xlsx")
    parser.add_argument("--profiles", type=Path, default=RESULTS / "Deviation_Profiles" / "NORMATIVE_DEVIATION_PROFILE_ANALYSIS.xlsx")
    parser.add_argument("--master", type=Path, default=RESULTS / "Biomarker" / "MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx")
    parser.add_argument("--output-dir", type=Path, default=PLOT_ROOT / "data")
    args = parser.parse_args()
    export_figure_inputs(args.summary, args.curves, args.periods, args.profiles, args.master, args.output_dir)


if __name__ == "__main__":
    main()
