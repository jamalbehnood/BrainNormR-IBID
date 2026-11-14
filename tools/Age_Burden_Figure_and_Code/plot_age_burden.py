"""Plot chronological age against balanced cortical deviation burden.

Uses verified participant identifiers and existing cross-fitted scores. No
normative model is refitted. Run after audit_age_link.py, or distribute this
script beside the locally generated age and burden inputs.
"""
from pathlib import Path
import json
import numpy as np
import pandas as pd
from scipy.stats import spearmanr
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parent
OUT = ROOT / 'figures'
OUT.mkdir(exist_ok=True)


def local_linear(x, y, grid, span=2 / 3):
    """Deterministic local linear tricube smoother, without robust iterations.

    The displayed curve is descriptive, with no confidence band or nonlinear
    significance claim. The same two-thirds span is used across the age range.
    """
    result = []
    k = int(np.ceil(len(x) * span))
    for point in grid:
        d = np.abs(x - point)
        bandwidth = np.partition(d, k - 1)[k - 1]
        u = d / max(bandwidth, np.finfo(float).eps)
        w = np.where(u < 1, (1 - np.minimum(u, 1) ** 3) ** 3, 0)
        design = np.column_stack([np.ones(len(x)), x - point])
        coef = np.linalg.lstsq(design * np.sqrt(w[:, None]), y * np.sqrt(w), rcond=None)[0]
        result.append(coef[0])
    return np.asarray(result)


def main():
    data = pd.read_csv(ROOT / 'analysis/Age_and_Balanced_Burden.csv')
    audit = json.loads((ROOT / 'analysis/Age_Link_Audit.json').read_text())
    assert len(data) == 293 and data.SubjectID.is_unique
    assert data.Age.min() == 19 and data.Age.max() == 77
    x = data.Age.to_numpy(float)
    y = data.Balanced_Burden_Percent.to_numpy(float)

    # Complete profiles comprise levels with 1, 2, 6 and 68 outcomes per
    # measure. Equal weighting across 20 blocks gives an exact common
    # denominator of 20*lcm(1,2,6,68) = 4,080. Ranking this integer numerator
    # treats mathematically identical burdens as ties, irrespective of
    # sub-machine-precision differences in Excel/R floating point summation.
    units = np.rint(y * 4080 / 100).astype(int)
    assert np.max(np.abs(y - units * 100 / 4080)) < 1e-10
    rho, p = spearmanr(x, units)
    smooth_x = np.linspace(x.min(), x.max(), 240)
    smooth_y = local_linear(x, y, smooth_x)
    style = {
        'font.family': 'DejaVu Sans', 'font.size': 9,
        'axes.labelsize': 9, 'xtick.labelsize': 8, 'ytick.labelsize': 8,
        'axes.spines.top': False, 'axes.spines.right': False,
        'pdf.fonttype': 42, 'svg.fonttype': 'none',
    }
    with plt.rc_context(style):
        fig, ax = plt.subplots(figsize=(7.2, 4.25))
        ax.scatter(x, y, s=18, color='#285f82', alpha=.58, edgecolors='none', label='Individual participants')
        ax.plot(smooth_x, smooth_y, color='#bf7929', linewidth=1.9, label='Descriptive local linear smooth')
        ax.axhline(y.mean(), color='#7e8993', linestyle=(0, (3, 3)), linewidth=.8, label='Cohort mean')
        ax.set(xlabel='Age (years)', ylabel='Balanced deviation burden (%)',
               xlim=(17, 79), ylim=(-1, 52))
        ax.set_xticks([20, 30, 40, 50, 60, 70, 77])
        ax.set_yticks([0, 10, 20, 30, 40, 50])
        ax.grid(axis='y', color='#e0e4e8', linewidth=.5)
        ax.set_axisbelow(True)
        ax.text(.02, .97, f'n = 293\nSpearman $\\rho$ = {rho:.3f}, p = {p:.3f}',
                transform=ax.transAxes, va='top', fontsize=8.5, linespacing=1.6)
        ax.legend(loc='upper right', frameon=False, fontsize=8, handlelength=2.4)
        fig.subplots_adjust(left=.12, right=.98, bottom=.16, top=.97)
        for ext in ['png', 'pdf', 'svg']:
            fig.savefig(OUT / ('Age_vs_Balanced_Deviation_Burden.' + ext), dpi=600, facecolor='white')
        plt.close(fig)
    pd.DataFrame({'Age': smooth_x, 'Descriptive_Smooth_Burden_Percent': smooth_y}).to_csv(
        ROOT / 'analysis/Descriptive_Age_Smoother.csv', index=False)
    note = {
        'N': len(data), 'age_min': float(x.min()), 'age_max': float(x.max()),
        'primary_threshold': 2.0, 'mean_burden_percent': float(y.mean()),
        'Spearman_rho_tie_aware': float(rho), 'Spearman_p_tie_aware': float(p),
        'tie_convention': 'Average ranks of exact integer numerators over 4,080',
        'unique_burden_values': int(len(np.unique(units))),
        'participants_in_tied_burden_groups': int(pd.Series(units).duplicated(keep=False).sum()),
        'stored_pipeline_age_rho': audit['published_Spearman_rho'],
        'stored_pipeline_age_p': audit['published_Spearman_p'],
        'smooth': 'Local linear tricube, span=2/3; no robust iterations; descriptive only',
        'inference': 'No detected monotonic age association. No test of nonlinear association was performed.',
        'limitations': 'Cross-sectional internal audit; held-out scores share training data. Nominal p-value is descriptive, not independent external validation.',
        'model_refitting': False, 'figure6_modified': False,
    }
    (ROOT / 'analysis/Age_Figure_Analysis.json').write_text(json.dumps(note, indent=2) + '\n')
    (OUT / 'Age_Figure_Caption.txt').write_text(
        'Chronological age and balanced cortical deviation burden. Each point represents one of 293 participants aged '
        '19–77 years. The ordinate is the mean percentage of extreme cross-fitted normative scores '
        '(|Z| ≥ 2) across 20 equally weighted anatomical level-by-measure blocks. Points use the recorded ages '
        'without jitter. The orange line is a descriptive local linear tricube smoother (span two-thirds, no robust '
        'iterations); the dashed line is the cohort mean. Spearman correlation uses average ranks for '
        f'mathematically tied burdens (ρ = {rho:.3f}, nominal two-sided p = {p:.3f}). '
        'No nonlinear association test or confidence band is implied. The absence of a detected monotonic '
        'association does not establish complete age calibration. Cross-sectional differences do not describe '
        'within-person aging. The original Figure 6 rank ordering is unchanged.\n', encoding='utf-8')
    print(json.dumps(note, indent=2))


if __name__ == '__main__':
    main()
