"""Rebuild Figure 6 from the existing participant and block-burden exports.

Rank 1 is the smallest balanced burden and rank 293 the largest. The profile
heatmap uses those same ranks, increasing from bottom to top. No models are fit.
Run: python code/rebuild_figure6.py
"""
from pathlib import Path
from io import BytesIO
import json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, Normalize
from matplotlib.transforms import Bbox

BASE = Path(__file__).resolve().parents[1]
MEASURES = ["Volume", "Area", "Thickness", "Curvature", "FD"]
LEVELS = ["WholeBrain", "Hemisphere", "Lobe", "ROI"]
LEVEL_LABELS = ["Whole cortex", "Hemisphere", "Bilateral lobe", "Region"]
SHORT = ["V", "A", "T", "C", "FD"]
STYLE = {
    "font.family": "DejaVu Sans", "font.size": 8,
    "axes.titlesize": 9, "axes.labelsize": 8,
    "xtick.labelsize": 7, "ytick.labelsize": 7,
    "pdf.fonttype": 42, "svg.fonttype": "none",
    "axes.spines.top": False, "axes.spines.right": False,
}


def build_figure(data_dir=None, output_dir=None):
    data_dir = Path(data_dir) if data_dir is not None else BASE / "data"
    output_dir = Path(output_dir) if output_dir is not None else BASE / "figures/main"
    output_dir.mkdir(parents=True, exist_ok=True)
    participants = pd.read_csv(data_dir / "Participant_Summary.csv")
    participants = participants[np.isclose(participants.Threshold, 2)].copy()
    profiles = pd.read_csv(data_dir / "Profile_Matrix.csv").set_index("SubjectID")
    if len(participants) != 293 or participants.SubjectID.duplicated().any():
        raise ValueError("Expected 293 unique participants at the |Z| >= 2 threshold.")
    if profiles.index.duplicated().any():
        raise ValueError("Duplicate participant identifiers in the block profiles.")
    participants = participants.sort_values(
        ["Balanced_LevelMeasure_Burden", "SubjectID"],
        ascending=[True, True], kind="stable",
    )
    blocks = [level + "__" + measure for level in LEVELS for measure in MEASURES]
    profile_pct = profiles.loc[participants.SubjectID, blocks].to_numpy(float) * 100
    burden_pct = participants.Balanced_LevelMeasure_Burden.to_numpy(float) * 100
    if not np.isfinite(profile_pct).all():
        raise ValueError("The figure requires complete finite block profiles.")
    if not np.allclose(profile_pct.mean(axis=1), burden_pct, atol=1e-10, rtol=0):
        raise ValueError("Balanced burden does not equal the mean of the 20 blocks.")
    means = profile_pct.mean(axis=0).reshape(4, 5)
    ranks = np.arange(1, len(participants) + 1)
    palette = plt.get_cmap("plasma")

    with plt.rc_context(STYLE):
        fig = plt.figure(figsize=(7.2, 7.5))
        grid = fig.add_gridspec(
            2, 2, height_ratios=[1, 1.55], left=.105, right=.90,
            bottom=.235, top=.95, hspace=.50, wspace=.48,
        )
        ax = fig.add_subplot(grid[0, 0])
        ax.plot(ranks, burden_pct, color="#26734d", linewidth=1.6)
        ax.axhline(burden_pct.mean(), color="#cb6a20", linestyle="--", linewidth=1)
        ax.set(xlim=(1, 293), ylim=(0, 52), xlabel="Participant rank", ylabel="Balanced burden (%)")
        ax.set_xticks([50, 100, 150, 200, 250])
        ax.set_yticks([0, 10, 20, 30, 40, 50])
        ax.grid(axis="y", alpha=.16, linewidth=.5)
        ax.set_title("a  Ranked balanced burden", loc="left", fontweight="bold", pad=8)

        ax = fig.add_subplot(grid[0, 1])
        ax.pcolormesh(
            np.arange(6) - .5, np.arange(5) - .5, means,
            cmap="YlGnBu", norm=Normalize(0, 10), shading="flat",
            edgecolors="white", linewidth=.35, rasterized=False,
        )
        ax.set(xlim=(-.5, 4.5), ylim=(3.5, -.5))
        ax.set_xticks(range(5), SHORT)
        ax.set_yticks(range(4), LEVEL_LABELS, fontsize=6.8)
        ax.tick_params(length=0, pad=3)
        for spine in ax.spines.values():
            spine.set_visible(False)
        for row in range(4):
            for col in range(5):
                ax.text(col, row, f"{means[row, col]:.2f}", ha="center", va="center",
                        fontsize=6.8, color="white" if means[row, col] > 6.2 else "#111111")
        ax.set_title("b  Mean block burden (%)", loc="left", fontweight="bold", pad=8)

        ax = fig.add_subplot(grid[1, :])
        display_blocks = blocks
        display_pct = profiles.loc[participants.SubjectID, display_blocks].to_numpy(float) * 100
        mesh = ax.pcolormesh(
            np.arange(21), np.arange(294) + .5, display_pct,
            cmap=palette, vmin=0, vmax=100, shading="flat",
            edgecolors="none", rasterized=False, antialiased=False,
        )
        ax.set(xlim=(0, 20), ylim=(.5, 293.5), ylabel="Participant rank")
        ax.set_yticks([1, 75, 150, 225, 293])
        ax.set_xticks(np.arange(20) + .5, SHORT * 4, fontsize=7)
        for x in [5, 10, 15]:
            ax.axvline(x, color='#f6f6f6', linewidth=.45)
        for x, label in zip([2.5, 7.5, 12.5, 17.5], LEVEL_LABELS):
            ax.text(x, -.13, label, transform=ax.get_xaxis_transform(),
                    ha='center', va='top', fontsize=7, clip_on=False)
        ax.set_title("c  Individual block profiles", loc="left", fontweight="bold", pad=8)
        position = ax.get_position()
        cb = fig.colorbar(mesh, cax=fig.add_axes([.925, position.y0, .016, position.height]))
        cb.set_label("Block burden (%)", fontsize=7)
        cb.set_ticks([0, 25, 50, 75, 100])
        cb.ax.tick_params(labelsize=6.5, length=2)
        if cb.solids is not None:
            cb.solids.set_rasterized(False)
            cb.solids.set_edgecolor("face")
        for ext in ["png", "pdf", "svg"]:
            buffer = BytesIO()
            fig.savefig(buffer, format=ext, dpi=600, facecolor="white",
                        bbox_inches=Bbox.from_bounds(0, 1.0, 7.2, 6.5))
            target = output_dir / ("Figure_6_Individual_Profiles." + ext)
            pending = target.with_suffix(".pending")
            pending.write_bytes(buffer.getvalue())
            pending.replace(target)
        plt.close(fig)

    ranked = pd.DataFrame({"Rank": ranks, "SubjectID": participants.SubjectID.to_numpy(),
                           "BalancedBurden_Percent": burden_pct})
    ranked = pd.concat([ranked, pd.DataFrame(profile_pct, columns=blocks)], axis=1)
    ranked.to_csv(output_dir / "Figure_6_Ranked_Source_Data.csv", index=False)
    audit = {"N": 293, "Threshold": 2, "rank_1": "lowest balanced burden",
             "rank_293": "highest balanced burden", "mean_percent": float(burden_pct.mean()),
             "minimum_percent": float(burden_pct.min()), "maximum_percent": float(burden_pct.max()),
             "block_means_percent": means.tolist(), "panel_c_palette": "plasma: dark purple to yellow, 0-100 percent", "panel_c_column_order": display_blocks,
             "model_refitting": False, "underlying_burdens_changed": False}
    (output_dir / "Figure_6_Display_Audit.json").write_text(json.dumps(audit, indent=2) + "\n")
    return audit


if __name__ == "__main__":
    print(json.dumps(build_figure(), indent=2))
