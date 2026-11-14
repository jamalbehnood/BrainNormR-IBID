from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
BASE=Path(__file__).resolve().parents[1]
WORK=BASE/'figures/supplementary'
WORK.mkdir(exist_ok=True,parents=True)
S=pd.read_csv(BASE/'data/summary.csv')
C=pd.read_csv(BASE/'data/centiles.csv.gz')
M=['Volume','Area','Thickness','Curvature','FD']
LOBES=['Frontal','Parietal','Temporal','Occipital','Cingulate','Insula']
def extra_figures():
    colors = ['#2c7fb8', '#d95f0e', '#238b45', '#756bb1', '#bc9319', '#1b9e77']
    style = {'font.family': 'DejaVu Sans', 'font.size': 8, 'axes.labelsize': 8,
             'xtick.labelsize': 7, 'ytick.labelsize': 7, 'pdf.fonttype': 42,
             'svg.fonttype': 'none', 'axes.spines.top': False, 'axes.spines.right': False}
    with plt.rc_context(style):
        fig, axes = plt.subplots(3, 2, figsize=(7.2, 7.0))
        for j, m in enumerate(M):
            ax = axes.flat[j]
            for lob, color in zip(LOBES, colors):
                row = S[(S.Level == 'Lobe') & (S.Hemisphere == 'Bilateral') &
                        (S.Region == lob) & (S.OutcomeType == m)].iloc[0]
                g = C[C.Outcome == row.Outcome].sort_values('Age')
                ax.plot(g.Age, (g.P50 - g.P50.iloc[0]) / row.SampleSD,
                        color=color, linewidth=1.3, label=lob)
            ax.axhline(0, color='#c6cbd1', linewidth=.6)
            ax.set(xlim=(19, 77), xticks=[20, 40, 60, 77], xlabel='Age (years)',
                   ylabel='Median difference / observed SD')
            ax.set_title(f'{chr(97+j)}  {m}', loc='left', fontweight='bold', fontsize=9)
            ax.grid(alpha=.15)
        axes.flat[5].axis('off')
        handles, labels = axes.flat[0].get_legend_handles_labels()
        axes.flat[5].legend(handles, labels, loc='center left', frameon=False, fontsize=8)
        fig.subplots_adjust(left=.13, right=.98, bottom=.085, top=.95, hspace=.55, wspace=.43)
        for ext in ['pdf', 'svg', 'png']:
            fig.savefig(WORK / ('Figure_S7_Bilateral_Lobar_Reference_Patterns.' + ext), dpi=600, facecolor='white')
        plt.close(fig)

        fig, axes = plt.subplots(1, 2, figsize=(7.2, 3.65))
        levels = ['WholeBrain', 'Hemisphere', 'Lobe', 'ROI']
        names = ['Whole\ncortex', 'Hemi-\nsphere', 'Lobe', 'DK68']
        for j, col, ylabel, ref in [(0, 'Coverage95', 'Coverage of 95% reference intervals (%)', 95),
                                  (1, 'CrossFit_Z_SD', 'SD of cross-fitted normative scores', 1)]:
            ax = axes[j]
            series = [S.loc[S.Level == lev, col].to_numpy() * (100 if j == 0 else 1) for lev in levels]
            boxes = ax.boxplot(series, tick_labels=names, patch_artist=True,
                               widths=.55, medianprops={'color': '#1a3851', 'linewidth': 1.2},
                               flierprops={'marker': 'o', 'markersize': 2.5, 'markerfacecolor': '#6d8a9e',
                                           'markeredgecolor': 'none', 'alpha': .6})
            for box in boxes['boxes']:
                box.set(facecolor='#d9e5ed', edgecolor='#6d8a9e', linewidth=.8)
            ax.axhline(ref, color='#b07937', linestyle='--', linewidth=.9)
            ax.set_ylabel(ylabel)
            ax.set_title(('a  Reference interval coverage' if j == 0 else 'b  Normative score spread'),
                         loc='left', fontweight='bold', fontsize=9)
            ax.grid(axis='y', alpha=.15)
        fig.subplots_adjust(left=.10, right=.98, top=.90, bottom=.19, wspace=.50)
        for ext in ['pdf', 'svg', 'png']:
            fig.savefig(WORK / ('Figure_S8_Internal_Calibration.' + ext), dpi=600, facecolor='white')
        plt.close(fig)

if __name__=='__main__':
    extra_figures()
