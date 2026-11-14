"""Reconstruct balanced burden from held-out scores and link it to age by ID."""

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import spearmanr


ROOT = Path(__file__).resolve().parent
RESULTS = ROOT.parent / "BrainNormR" / "Results"
LEVEL_COUNTS = {"WholeBrain": 1, "Hemisphere": 2, "Lobe": 6, "ROI": 68}
MEASURES = ("Volume", "Area", "Thickness", "Curvature", "FD")
SCORE_COLUMNS = (
    "Outcome", "OutcomeType", "Level", "Hemisphere", "SubjectID",
    "SubjectID_IsFallback", "Age", "Sex", "edu", "CF_Norm_Z",
)
BURDEN_DENOMINATOR = 4080


def read_profiles(master_file):
    scores = pd.read_excel(
        master_file, sheet_name="NormativeScores_All", usecols=list(SCORE_COLUMNS),
        dtype={"SubjectID": str},
    )
    scores = scores.loc[
        scores.Level.isin(LEVEL_COUNTS)
        & scores.OutcomeType.isin(MEASURES)
        & (scores.Level.ne("Lobe") | scores.Hemisphere.eq("Bilateral"))
    ].copy()
    if (len(scores) != 112805 or scores.Outcome.nunique() != 385
            or scores.SubjectID.isna().any()
            or scores.SubjectID_IsFallback.astype(str).str.upper().isin({"TRUE", "1"}).any()
            or scores.duplicated(["SubjectID", "Outcome"]).any()
            or not np.isfinite(scores[["Age", "CF_Norm_Z"]].to_numpy()).all()):
        raise ValueError("Expected 112,805 finite, uniquely identified held-out cortical scores.")
    metadata = scores[["SubjectID", "Age", "Sex", "edu"]].drop_duplicates().set_index("SubjectID")
    if len(metadata) != 293 or not metadata.index.is_unique:
        raise ValueError("Participant metadata must be consistent across the 385 included outcomes.")

    scores["Extreme"] = scores.CF_Norm_Z.abs().ge(2)
    blocks = scores.groupby(["SubjectID", "Level", "OutcomeType"], sort=False).Extreme.agg(["size", "sum"])
    expected = blocks.index.get_level_values("Level").map(LEVEL_COUNTS)
    if len(blocks) != 293 * 20 or not np.array_equal(blocks["size"], expected):
        raise ValueError("Expected 20 complete anatomical level-by-measure blocks per participant.")
    numerators = blocks["sum"].mul(204 // blocks["size"]).groupby(level="SubjectID").sum()
    metadata["Burden_Numerator"] = numerators
    return metadata


def audit_age_link(master_file, profile_file, output_dir):
    metadata = read_profiles(master_file)
    participants = pd.read_excel(profile_file, sheet_name="Participant_Summary", dtype={"SubjectID": str})
    participants = participants.loc[np.isclose(participants.Threshold, 2)].set_index("SubjectID")
    if not participants.index.is_unique or set(participants.index) != set(metadata.index):
        raise ValueError("The profile workbook and the score workbook must contain the same 293 participants.")
    participants = participants.join(metadata)
    stored = participants.Balanced_LevelMeasure_Burden
    reconstructed = participants.Burden_Numerator / BURDEN_DENOMINATOR
    error = (reconstructed - stored).abs().max()
    if not np.isfinite(stored).all() or error > 1e-12:
        raise ValueError("The reconstructed balanced burden differs from the stored profile results.")
    participants["Balanced_Burden_Percent"] = 100 * stored

    rho, p = spearmanr(participants.Age, participants.Burden_Numerator)
    old = pd.read_excel(profile_file, sheet_name="Demographic_Audit")
    old_age = old.loc[old.Audit.str.startswith("Age")].iloc[0]
    result = {
        "N": len(participants),
        "profile_outcomes": 385,
        "score_records_verified": 112805,
        "blocks_per_participant": 20,
        "maximum_absolute_burden_reconstruction_error": float(error),
        "Spearman_rho": float(rho),
        "Spearman_p": float(p),
        "tie_convention": "Average ranks of exact numerators over 4,080",
        "published_Spearman_rho": float(old_age.Effect),
        "published_Spearman_p": float(old_age.P_Value),
        "published_demographic_q": float(old_age.Q_Value),
    }
    output_dir.mkdir(parents=True, exist_ok=True)
    participants[["Age", "Balanced_Burden_Percent"]].to_csv(
        output_dir / "Age_and_Balanced_Burden.csv", index=True,
    )
    (output_dir / "Age_Link_Audit.json").write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(f"Age audit: n={result['N']}, Spearman rho={rho:.6f}, p={p:.6f}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--master", type=Path, default=RESULTS / "Biomarker" / "MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx")
    parser.add_argument("--profiles", type=Path, default=RESULTS / "Deviation_Profiles" / "NORMATIVE_DEVIATION_PROFILE_ANALYSIS.xlsx")
    parser.add_argument("--output-dir", type=Path, default=ROOT / "analysis")
    args = parser.parse_args()
    audit_age_link(args.master, args.profiles, args.output_dir)


if __name__ == "__main__":
    main()
