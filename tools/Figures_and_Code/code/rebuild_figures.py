"""Plot Paper 1 from exported results. No model fitting or permutation reruns.

Run from any working directory: python code/rebuild_figures.py
The constant-gradient audit is applied before assigning maximum-gradient ages.
"""
from pathlib import Path
from io import BytesIO
import json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
from matplotlib.lines import Line2D
from matplotlib.colors import TwoSlopeNorm

ROOT=Path(__file__).resolve().parents[1]
DATA=ROOT/'data'; OUT=ROOT/'figures'
M=['Volume','Area','Thickness','Curvature','FD']
PERIODS=['19–29','30–39','40–49','50–59','60–77']
LOBES=['Frontal','Parietal','Temporal','Occipital','Cingulate','Insula']
PALETTE=['#2c7fb8','#d95f0e','#238b45','#756bb1','#e6ab02','#1b9e77','#a6761d']
GREEN='#26734d'; BLUE='#2878a1'; ORANGE='#cb6a20'
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':8,'axes.titlesize':9,
 'axes.labelsize':8,'xtick.labelsize':7,'ytick.labelsize':7,'pdf.fonttype':42,
 'svg.fonttype':'none','axes.spines.top':False,'axes.spines.right':False})
S=pd.read_csv(DATA/'summary.csv'); C=pd.read_csv(DATA/'centiles.csv.gz')
P=pd.read_csv(DATA/'periods.csv'); A=pd.read_csv(DATA/'adjacent.csv')
PS=pd.read_csv(DATA/'Participant_Summary.csv')
PR=pd.read_csv(DATA/'Profile_Matrix.csv').set_index('SubjectID')
ST=pd.read_csv(DATA/'Stability_Iterations.csv')
CONTRASTS=list(A[A.Level.eq('WholeBrain')].contrast.unique())
CAPTIONS={}

def audit_gradients():
    rows=[]
    for outcome,g in C.groupby('Outcome',sort=False):
        g=g.sort_values('Age');x=g.Age.to_numpy();y=g.P50.to_numpy()
        sd=float(S.loc[S.Outcome.eq(outcome),'SampleSD'].iloc[0])
        residual=float(np.max(np.abs(y-np.polyval(np.polyfit(x,y,1),x)))/sd)
        variation=float(np.ptp(np.gradient(y,x))/sd)
        rows.append({'Outcome':outcome,'LinearityResidual_perSD':residual,
                     'DerivativeRange_perSD':variation,'ConstantGradient':residual<1e-8 and variation<1e-8})
    audit=pd.DataFrame(rows)
    constant=set(audit.loc[audit.ConstantGradient,'Outcome'])
    summary=S.copy();summary['ConstantGradient']=summary.Outcome.isin(constant)
    summary['UniqueInteriorMaximum']=~summary.ConstantGradient & ~summary.MaxChangeAtInteriorBoundary.astype(bool) & summary.MaxChangeStable.astype(bool)
    classifications=[]
    for _,r in summary.iterrows():
        g=P[P.Outcome.eq(r.Outcome)]
        label='Constant' if r.ConstantGradient else g.loc[g.MeanDerivative_per_SD.abs().idxmax(),'PeriodLabel']
        classifications.append({'Outcome':r.Outcome,'Measure':r.OutcomeType,'Period':label})
    return summary,audit,pd.DataFrame(classifications)

S_AUDIT,GRADIENT_AUDIT,CLASS=audit_gradients()

def save(fig,name,folder='main'):
    target=OUT/folder;target.mkdir(parents=True,exist_ok=True)
    for ext in ['png','pdf','svg']:
        stream=BytesIO();fig.savefig(stream,format=ext,dpi=600,facecolor='white')
        payload=stream.getvalue()
        if ext=='png':assert payload[-8:-4]==b'IEND'
        p=target/(name+'.'+ext);pending=p.with_suffix('.pending')
        pending.write_bytes(payload);pending.replace(p)
    plt.close(fig)

def panel(ax,title):
    ax.set_title(title,loc='left',fontweight='bold',pad=7)

def colorbar(fig,im,rect,label):
    cb=fig.colorbar(im,cax=fig.add_axes(rect),orientation='horizontal')
    cb.set_label(label,fontsize=7.5,labelpad=3)
    cb.ax.tick_params(labelsize=7,length=2)
    if cb.solids is not None:
        cb.solids.set_rasterized(False);cb.solids.set_edgecolor('face')
    return cb

def heat(ax,values,xlabels,ylabels,limit=1.2,annot=None,q=None,fontsize=7):
    values=np.asarray(values,float);nr,nc=values.shape
    im=ax.pcolormesh(np.arange(nc+1)-.5,np.arange(nr+1)-.5,values,
        cmap='RdBu_r',norm=TwoSlopeNorm(vmin=-limit,vcenter=0,vmax=limit),
        shading='flat',linewidth=.25,edgecolors='white',antialiased=False,rasterized=False)
    ax.set_xlim(-.5,nc-.5);ax.set_ylim(nr-.5,-.5)
    ax.set_xticks(range(nc),xlabels,fontsize=fontsize)
    ax.set_yticks(range(nr),ylabels,fontsize=fontsize)
    ax.tick_params(length=0,pad=3)
    for spine in ax.spines.values():spine.set_visible(False)
    if annot is not None:
        for r in range(nr):
            for c in range(nc):
                label=format(values[r,c],annot)
                if q is not None:
                    prob=q[r,c];label+='***' if prob<.001 else '**' if prob<.01 else '*' if prob<.05 else ''
                ax.text(c,r,label.replace('-','−'),ha='center',va='center',fontsize=fontsize,
                    color='white' if abs(values[r,c])>limit*.62 else '#111111')
    return im

def curve(outcome):return C[C.Outcome.eq(outcome)].sort_values('Age')
def selected(level,measure,region=None,hemi=None):
    g=S[S.Level.eq(level)&S.OutcomeType.eq(measure)]
    if region is not None:g=g[g.Region.eq(region)]
    if hemi is not None:g=g[g.Hemisphere.eq(hemi)]
    assert len(g)==1,(level,measure,region,hemi)
    return g.iloc[0]

def display_region(row):
    if row.Level=='WholeBrain':return 'Whole cortex'
    if row.Level=='Hemisphere':return row.Hemisphere+' hemisphere'
    if row.Level=='Lobe':return {'Left':'L ','Right':'R ','Bilateral':'B '}[row.Hemisphere]+row.Region.lower()+' lobe'
    names={'caudalanteriorcingulate':'caudal ant. cingulate','rostralanteriorcingulate':'rostral ant. cingulate',
      'caudalmiddlefrontal':'caudal middle frontal','rostralmiddlefrontal':'rostral middle frontal',
      'lateralorbitofrontal':'lateral orbitofrontal','medialorbitofrontal':'medial orbitofrontal',
      'superiortemporal':'superior temporal','inferiortemporal':'inferior temporal','middletemporal':'middle temporal',
      'superiorfrontal':'superior frontal','inferiorparietal':'inferior parietal','superiorparietal':'superior parietal',
      'lateraloccipital':'lateral occipital','frontalpole':'frontal pole','temporalpole':'temporal pole',
      'parsopercularis':'pars opercularis','parsorbitalis':'pars orbitalis','parstriangularis':'pars triangularis',
      'transversetemporal':'transverse temporal','bankssts':'banks STS','isthmuscingulate':'isthmus cingulate'}
    reg=str(row.Region).removeprefix('lh_').removeprefix('rh_')
    return ('L ' if row.Hemisphere=='Left' else 'R ')+names.get(reg,reg)

def figure1():
    fig,ax=plt.subplots(figsize=(7.2,8.3));ax.set_axis_off();ax.set_xlim(0,1);ax.set_ylim(0,1)
    def box(x,y,w,h,text,color):
        ax.add_patch(FancyBboxPatch((x,y),w,h,boxstyle='round,pad=.012',facecolor=color,edgecolor='#536476',linewidth=1))
        ax.text(x+w/2,y+h/2,text,ha='center',va='center',fontsize=8.2,linespacing=1.3)
    def arrow(start,end):ax.annotate('',xy=end,xytext=start,arrowprops={'arrowstyle':'-|>','color':'#536476','lw':1.0})
    box(.13,.90,.74,.075,'293 healthy adults, aged 19–77 years\nT1 MRI, sex and eTIV','#dceaf3')
    box(.13,.78,.74,.075,'FreeSurfer and CAT12 measurements\nVolume, area, thickness, curvature and FD; DK68 atlas','#dceee5');arrow((.5,.90),(.5,.865))
    box(.13,.66,.74,.075,'445 structural outcomes across four representations\nWhole cortex, hemispheres, lobes and DK68 regions','#e7e3f0');arrow((.5,.78),(.5,.745))
    box(.02,.50,.43,.10,'Full-sample reference models\nGAMLSS family and age functions\nConditional participant bootstrap','#f3ead8')
    box(.55,.50,.43,.10,'Fivefold held-out scoring\nModel selection within training folds\nConditional CDF to normative Z','#ddecf0')
    arrow((.4,.66),(.235,.61));arrow((.6,.66),(.765,.61))
    box(.02,.32,.43,.12,'Reference centiles and age patterns\nPeaks and troughs; gradient audit\nAge-period summaries and ANCOVA\nConstant slopes have no unique maximum','#f5e3dc')
    box(.55,.32,.43,.12,'385 outcomes in 20 profile blocks\nWhole cortex, hemispheres,\nbilateral lobes and DK68 regions\nBalanced and outcome-weighted burden','#dceee5')
    arrow((.235,.50),(.235,.45));arrow((.765,.50),(.765,.45))
    box(.55,.15,.43,.10,'Stored permutation benchmarks\nOutcome resampling and split halves\nThreshold sensitivity and audits','#ece7f3');arrow((.765,.32),(.765,.26))
    box(.06,.015,.88,.075,'Cross-sectional age patterns and individual deviation profiles\nInternal validation; conditional uncertainty; no diagnostic subtype assignment','#e7ecf0')
    arrow((.235,.32),(.235,.10));arrow((.765,.15),(.765,.10))
    fig.subplots_adjust(left=.03,right=.97,bottom=.01,top=.99);save(fig,'Figure_1_Workflow')

def figure2():
    fig,axs=plt.subplots(3,2,figsize=(7.2,7.0));hist=json.loads((DATA/'age_histogram.json').read_text())
    for i,m in enumerate(M):
        ax=axs.flat[i];row=selected('WholeBrain',m);g=curve(row.Outcome);factor=1000 if m in ['Volume','Area'] else 1
        ax.fill_between(g.Age,g.P2_5/factor,g.P97_5/factor,color=GREEN,alpha=.13)
        ax.fill_between(g.Age,g.P25/factor,g.P75/factor,color=GREEN,alpha=.23)
        ax.plot(g.Age,g.P50/factor,color=GREEN,lw=1.8)
        panel(ax,f'{chr(97+i)}  {m}');ax.set_xlim(19,77);ax.set_xticks([20,40,60,77]);ax.set_xlabel('Age (years)')
        ax.set_ylabel({'Volume':'Volume (10³ mm³)','Area':'Area (10³ mm²)','Thickness':'Thickness (mm)','Curvature':'Curvature (input units)','FD':'Fractal dimension'}[m]);ax.grid(alpha=.15)
    ax=axs.flat[5];counts=np.asarray(hist['group_counts'])
    ax.bar(range(5),counts,color='#6699ad',edgecolor='white',linewidth=.5)
    for j,n in enumerate(counts):ax.text(j,n+1,str(n),ha='center',va='bottom',fontsize=7)
    panel(ax,'f  Participant age groups');ax.set_xlabel('Age group (years)');ax.set_ylabel('Participants');ax.set_xticks(range(5),PERIODS,rotation=22,ha='right');ax.set_ylim(0,82);ax.text(.97,.95,'n = 293',transform=ax.transAxes,ha='right',va='top')
    fig.legend([Line2D([0],[0],color=GREEN,lw=2),Line2D([0],[0],color=GREEN,lw=7,alpha=.35),Line2D([0],[0],color=GREEN,lw=7,alpha=.13)],['Median','Interquartile range','2.5th–97.5th centiles'],loc='lower center',ncol=3,frameon=False,fontsize=7)
    fig.subplots_adjust(left=.115,right=.985,bottom=.13,top=.95,hspace=.5,wspace=.4);save(fig,'Figure_2_Reference_Curves')

def endpoints():
    sub=S.copy();sub['Key']=sub.apply(display_region,axis=1)
    units=[('WholeBrain',None,'Bilateral')]+[('Hemisphere',None,h) for h in ['Left','Right']]+[('Lobe',l,'Bilateral') for l in LOBES]
    a=np.array([[selected(level,m,reg,hemi).P50Change_19to77_perSampleSD for m in M] for level,reg,hemi in units])
    labels=['Whole cortex','Left hemisphere','Right hemisphere']+LOBES
    regions=sorted({x.removeprefix('lh_').removeprefix('rh_') for x in S[S.Level.eq('ROI')].Region})
    matrices=[]
    for hemi,prefix in [('Left','lh_'),('Right','rh_')]:
        matrices.append(np.array([[selected('ROI',m,prefix+r,hemi).P50Change_19to77_perSampleSD for m in M] for r in regions]))
    return a,labels,regions,matrices

def figure3():
    a,labels,regions,matrices=endpoints()
    fig=plt.figure(figsize=(7.2,8.8))
    ax=fig.add_axes([.31,.735,.58,.205])
    im=heat(ax,a,['Vol.','Area','Thick.','Curv.','FD'],labels,limit=2.75,fontsize=7.2)
    panel(ax,'a  Aggregate morphology')
    for i,(pos,values,title) in enumerate(zip([[.245,.125,.265,.525],[.715,.125,.265,.525]],matrices,['b  Left DK34 regions','c  Right DK34 regions'])):
        ax=fig.add_axes(pos)
        im=heat(ax,values,['Vol.','Area','Thick.','Curv.','FD'],regions,limit=2.75,fontsize=7.2)
        panel(ax,title)
    colorbar(fig,im,[.32,.048,.52,.017],'[P50(77) − P50(19)] / observed outcome SD')
    save(fig,'Figure_3_Endpoint_Patterns')

def extrema_data():
    rows=[]
    for _,r in S.iterrows():
        for kind in ['Peak','Trough']:
            if bool(r[kind+'Stable']):rows.append({'Outcome':r.Outcome,'Measure':r.OutcomeType,'Kind':kind,'Age':r[kind+'Age'],'Lower':r[kind+'Age_L95'],'Upper':r[kind+'Age_U95'],'Label':display_region(r)})
    frame=pd.DataFrame(rows);assert len(frame)==65;return frame

def figure4():
    ex=extrema_data();order=['Volume','Area','Curvature','FD','Thickness']
    fig=plt.figure(figsize=(7.2,8.2));gs=fig.add_gridspec(3,2,height_ratios=[.85,1.7,2.0],left=.22,right=.985,top=.96,bottom=.065,hspace=.45,wspace=1.35)
    for i,m in enumerate(order):
        ax=fig.add_subplot(gs[i//2,i%2]);g=ex[ex.Measure.eq(m)].sort_values('Age').reset_index(drop=True)
        for j,r in g.iterrows():
            ax.plot([r.Lower,r.Upper],[j,j],color='#a3aeb8',lw=.7)
            ax.scatter(r.Age,j,marker='o' if r.Kind=='Peak' else 's',color=ORANGE if r.Kind=='Peak' else BLUE,s=13,zorder=4)
        ax.set_yticks(range(len(g)),g.Label,fontsize=7.2);ax.set_ylim(len(g)-.2,-.8);ax.set_xlim(19,77);ax.set_xticks([20,40,60,77]);ax.grid(axis='x',alpha=.15);panel(ax,f'{chr(97+i)}  {m}')
        if i>=3:ax.set_xlabel('Age (years)')
    ax=fig.add_subplot(gs[2,1]);ax.axis('off')
    ax.legend([Line2D([0],[0],marker='o',color=ORANGE,linestyle='none'),Line2D([0],[0],marker='s',color=BLUE,linestyle='none'),Line2D([0],[0],color='#a3aeb8')],['Peak','Trough','Conditional 95% interval'],loc='upper left',frameon=False,fontsize=7)
    ax.text(0,.55,'L / R: left / right\nB: bilateral lobe\n\n18 peaks and 47 troughs\nNo whole-cortex extrema\n\nIntervals condition on the\nselected model specification',fontsize=7,va='top',linespacing=1.45)
    save(fig,'Figure_4_Internal_Extrema')

def figure5():
    fig=plt.figure(figsize=(7.2,7.55));gs=fig.add_gridspec(3,1,height_ratios=[1.05,1.2,1.2],left=.18,right=.96,bottom=.14,top=.95,hspace=.65)
    ax=fig.add_subplot(gs[0]);base=np.zeros(5)
    lower=[];upper=[];constant=[];unique=[]
    for m in M:
        g=S_AUDIT[S_AUDIT.OutcomeType.eq(m)]
        lower.append(int((~g.ConstantGradient & g.MaxChangeAtInteriorBoundary.astype(bool)&g.MaxChangeAge.lt(40)).sum()))
        upper.append(int((~g.ConstantGradient & g.MaxChangeAtInteriorBoundary.astype(bool)&g.MaxChangeAge.gt(40)).sum()))
        constant.append(int(g.ConstantGradient.sum()));unique.append(int(g.UniqueInteriorMaximum.sum()))
    for label,values,color in [('Lower boundary',lower,'#5b8db8'),('Upper boundary',upper,'#d59b52'),('Constant gradient',constant,'#bcbcbc')]:
        ax.barh(range(5),values,left=base,height=.68,color=color,label=label)
        for j,v in enumerate(values):
            if v>=4:ax.text(base[j]+v/2,j,str(v),ha='center',va='center',fontsize=7)
        base+=np.asarray(values)
    ax.set_yticks(range(5),M);ax.invert_yaxis();ax.set_xlim(0,90);ax.set_xlabel('Outcomes (89 per measure)');panel(ax,'a  Location of the maximum absolute median gradient')
    legend_handles,legend_labels=ax.get_legend_handles_labels()
    ax=fig.add_subplot(gs[1]);counts=pd.crosstab(CLASS.Measure,CLASS.Period).reindex(index=M,columns=PERIODS+['Constant'],fill_value=0)
    vals=counts.to_numpy();ax.pcolormesh(np.arange(7)-.5,np.arange(6)-.5,vals,cmap='Blues',vmin=0,vmax=89,edgecolors='white',linewidth=.4)
    ax.set_xlim(-.5,5.5);ax.set_ylim(4.5,-.5);ax.set_yticks(range(5),M);ax.set_xticks(range(6),PERIODS+['Constant']);ax.tick_params(length=0)
    for y in range(5):
        for x in range(6):ax.text(x,y,str(vals[y,x]),ha='center',va='center',fontsize=8,color='white' if vals[y,x]>50 else 'black')
    panel(ax,'b  Period with the largest absolute average gradient')
    gap_y=(fig.axes[0].get_position().y0+ax.get_position().y1)/2
    fig.legend(legend_handles,legend_labels,loc='center',bbox_to_anchor=(.57,gap_y),ncol=4,frameon=False,fontsize=6.0)
    ax=fig.add_subplot(gs[2]);whole=A[A.Level.eq('WholeBrain')]
    v=np.array([[whole[whole.Measure.eq(m)&whole.contrast.eq(c)].Std_Adjusted_Difference.iloc[0] for c in CONTRASTS] for m in M])
    q=np.array([[whole[whole.Measure.eq(m)&whole.contrast.eq(c)].q_BH_LevelMeasureContrast.iloc[0] for c in CONTRASTS] for m in M])
    im=heat(ax,v,['30s − 20s','40s − 30s','50s − 40s','60–77 − 50s'],M,limit=1.2,annot='.2f',q=q,fontsize=7.5)
    panel(ax,'c  Whole-cortex adjacent-group adjusted contrasts')
    colorbar(fig,im,[.31,.073,.48,.018],'Adjusted older-minus-younger difference / residual SD')
    fig.text(.18,.012,'Stars: within-family BH q < .05 / .01 / .001; no stars on descriptive gradients.',fontsize=6.5)
    save(fig,'Figure_5_Gradient_and_Period_Audit')

def figure6():
    # The standalone renderer fixes one rank convention for both panels a and c.
    import importlib.util
    spec=importlib.util.spec_from_file_location('paper1_figure6',ROOT/'code/rebuild_figure6.py')
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
    module.build_figure(DATA,OUT/'main')

def figure7():
    no=pd.read_csv(DATA/'Null_Outcome_Samples.csv');nb=pd.read_csv(DATA/'Null_Block_Samples.csv');ns=pd.read_csv(DATA/'Null_Summary.csv')
    fig,axs=plt.subplots(2,2,figsize=(7.2,5.2))
    for i,(ax,frame,target,stat) in enumerate([(axs[0,0],no,'Outcome-weighted structural burden','SD'),(axs[0,1],nb,'Equal-weighted multiscale structural burden','SD'),(axs[1,0],no,'Outcome-weighted structural burden','P95'),(axs[1,1],nb,'Equal-weighted multiscale structural burden','P95')]):
        row=ns[ns.Target.eq(target)&ns.Statistic.eq(stat)]
        if len(row)!=1:
            row=ns[ns.Statistic.eq(stat)&ns.Null_Model.str.contains('outcome' if frame is no else 'block',case=False)]
        assert len(row)==1,(target,stat)
        observed=float(row.Observed.iloc[0])*100
        ax.hist(frame[stat]*100,bins=32,color='#7298af',edgecolor='white',linewidth=.25)
        ax.axvline(observed,color=ORANGE,lw=1.5,label=f'Observed = {observed:.2f}')
        panel(ax,f'{chr(97+i)}  '+('Outcome-wise' if frame is no else 'Block-wise')+' permutation')
        ax.set_ylabel('Iterations');ax.set_xlabel('Burden SD (percentage points)' if stat=='SD' else '95th-percentile burden (%)');ax.legend(frameon=False,fontsize=6.6)
    fig.subplots_adjust(left=.10,right=.98,bottom=.12,top=.94,hspace=.5,wspace=.35);save(fig,'Figure_7_Permutation_Benchmarks')

def figure8():
    fig,axs=plt.subplots(2,2,figsize=(7.2,5.4));methods=['Outcome bootstrap within block','Stratified split-half within block']
    present=list(ST.Method.unique());methods=[next(x for x in present if 'bootstrap' in x.lower()),next(x for x in present if 'split' in x.lower())]
    for ax,metric,tag,title in [(axs[0,0],'Balanced_Burden_Rho','a','Burden rank agreement'),(axs[0,1],'Top_Decile_Jaccard','b','Top-decile overlap')]:
        arrays=[ST[ST.Method.eq(x)][metric].dropna().to_numpy() for x in methods]
        ax.boxplot(arrays,positions=[1,2],widths=.5,showfliers=False,medianprops={'color':'black'})
        ax.scatter([1,2],[np.mean(x) for x in arrays],color=ORANGE,marker='D',s=15,zorder=4)
        ax.set_xticks([1,2],['Outcome\nresampling','Split half']);ax.set_ylim(0,1.02);panel(ax,f'{tag}  {title}');ax.set_ylabel('Spearman rho' if metric.endswith('Rho') else 'Jaccard overlap')
    ax=axs[1,0];thresholds=[1.96,2,2.58];arrays=[PS[np.isclose(PS.Threshold,t)].Balanced_LevelMeasure_Burden.to_numpy()*100 for t in thresholds]
    ax.boxplot(arrays,positions=[1,2,3],showfliers=False,medianprops={'color':'black'});ax.scatter([1,2,3],[np.mean(x) for x in arrays],color=ORANGE,marker='D',s=15)
    ax.set_xticks([1,2,3],['1.96','2.00','2.58']);ax.set_xlabel('Absolute Z threshold');ax.set_ylabel('Balanced burden (%)');panel(ax,'c  Threshold sensitivity')
    ax=axs[1,1];pair=pd.read_csv(DATA/'Threshold_Pairwise.csv');top=pd.read_csv(DATA/'Threshold_TopDecile.csv')
    pair=pair[pair.Metric.eq('Balanced_LevelMeasure_Burden')].copy().sort_values(['Threshold_1','Threshold_2']);top=top[top.Metric.eq('Balanced_LevelMeasure_Burden')]
    vals=[];labels=[]
    for _,r in pair.iterrows():
        q=top[np.isclose(top.Threshold_1,r.Threshold_1)&np.isclose(top.Threshold_2,r.Threshold_2)].iloc[0]
        vals.append([r.Spearman_rho,q.Top_Decile_Jaccard]);labels.append(f'{r.Threshold_1:.2f} / {r.Threshold_2:.2f}')
    v=np.asarray(vals);ax.pcolormesh(np.arange(3)-.5,np.arange(4)-.5,v,cmap='Blues',vmin=0,vmax=1,edgecolors='white',linewidth=.5)
    ax.set_xticks([0,1],['Rank rho','Jaccard']);ax.set_yticks(range(3),labels);ax.set_ylim(2.5,-.5)
    for y in range(3):
        for x in range(2):ax.text(x,y,f'{v[y,x]:.3f}',ha='center',va='center',color='white' if v[y,x]>.65 else 'black',fontsize=8)
    panel(ax,'d  Pairwise threshold agreement')
    fig.subplots_adjust(left=.115,right=.985,top=.95,bottom=.12,hspace=.62,wspace=.5);save(fig,'Figure_8_Stability_and_Sensitivity')

def aggregate_curves(level,name,groups):
    fig,axs=plt.subplots(3,2,figsize=(7.2,7.1))
    for i,m in enumerate(M):
        ax=axs.flat[i]
        for j,(label,region,hemi) in enumerate(groups):
            row=selected(level,m,region,hemi);g=curve(row.Outcome)
            ax.plot(g.Age,(g.P50-g.P50.iloc[0])/row.SampleSD,color=PALETTE[j],label=label,lw=1.5)
        panel(ax,f'{chr(97+i)}  {m}');ax.axhline(0,color='.7',lw=.5);ax.set_xlim(19,77);ax.set_xticks([20,40,60,77]);ax.set_xlabel('Age (years)');ax.set_ylabel('Median difference from age 19\n(observed SD units)');ax.grid(alpha=.12)
    axs.flat[5].axis('off');handles,labels=axs.flat[0].get_legend_handles_labels();axs.flat[5].legend(handles,labels,loc='center left',frameon=False,fontsize=7)
    fig.subplots_adjust(left=.12,right=.985,top=.95,bottom=.08,hspace=.5,wspace=.43);save(fig,name,'supplementary')

def supplements():
    aggregate_curves('Hemisphere','Figure_S1_Hemispheric_Reference_Patterns',[(h,None,h) for h in ['Left','Right']])
    _,_,regions,matrices=endpoints();fig,axs=plt.subplots(1,2,figsize=(7.2,8.0))
    for i,(ax,vals) in enumerate(zip(axs,matrices)):
        im=heat(ax,vals,['Vol.','Area','Thick.','Curv.','FD'],regions,limit=2.75,fontsize=6.6);panel(ax,('a  Left' if i==0 else 'b  Right')+' hemisphere')
    fig.subplots_adjust(left=.25,right=.98,top=.95,bottom=.13,wspace=.95);colorbar(fig,im,[.33,.045,.50,.016],'[P50(77) − P50(19)] / observed outcome SD');save(fig,'Figure_S2_DK68_Endpoint_Patterns','supplementary')
    ex=extrema_data();fig,axs=plt.subplots(2,1,figsize=(7.2,5.8),sharex=True)
    for i,(ax,kind) in enumerate(zip(axs,['Peak','Trough'])):
        for j,m in enumerate(M):
            g=ex[ex.Kind.eq(kind)&ex.Measure.eq(m)].sort_values('Age')
            for k,(_,r) in enumerate(g.iterrows()):
                y=j+(k-(len(g)-1)/2)*min(.035,.55/max(1,len(g)))
                ax.plot([r.Lower,r.Upper],[y,y],color=PALETTE[j],alpha=.4,lw=.7);ax.scatter(r.Age,y,color=PALETTE[j],s=12)
        ax.set_yticks(range(5),M);ax.set_xlim(19,77);ax.set_ylim(-.7,4.7);ax.grid(axis='x',alpha=.15);panel(ax,f'{chr(97+i)}  Supported internal {kind.lower()}s')
    axs[1].set_xlabel('Age (years)');fig.subplots_adjust(left=.15,right=.98,bottom=.10,top=.95,hspace=.35);save(fig,'Figure_S3_Extremum_Timing_Summary','supplementary')
    fig,axs=plt.subplots(3,2,figsize=(7.2,7.1))
    for i,m in enumerate(M):
        ax=axs.flat[i]
        for j,reg in enumerate([None]+LOBES):
            r=selected('WholeBrain' if reg is None else 'Lobe',m,reg,'Bilateral');g=curve(r.Outcome);iq=g.P75-g.P25
            ax.plot(g.Age,iq/np.interp(20,g.Age,iq),color='black' if reg is None else PALETTE[j-1],ls='--' if reg is None else '-',lw=1.7 if reg is None else 1.2,label='Whole cortex' if reg is None else reg)
        panel(ax,f'{chr(97+i)}  {m}');ax.set_xlim(19,77);ax.set_xlabel('Age (years)');ax.set_ylabel('Conditional IQR / IQR at age 20');ax.grid(alpha=.12)
    axs.flat[5].axis('off');h,l=axs.flat[0].get_legend_handles_labels();axs.flat[5].legend(h,l,loc='center left',frameon=False,fontsize=7)
    fig.subplots_adjust(left=.12,right=.985,top=.95,bottom=.08,hspace=.5,wspace=.43);save(fig,'Figure_S4_Conditional_Spread','supplementary')
    period_summary()
    adjacent_lobes()

def period_summary():
    fig,axs=plt.subplots(3,2,figsize=(7.2,8.0));units=[('WholeBrain','WholeBrain')]+[('Lobe',l) for l in LOBES]
    for i,m in enumerate(M):
        ax=axs.flat[i];values=[]
        for lev,reg in units:
            g=P[P.Level.eq(lev)&P.Region.eq(reg)&P.Hemisphere.eq('Bilateral')&P.Measure.eq(m)]
            values.append([g[g.PeriodLabel.eq(p)].MeanDerivative_per_SD.iloc[0] for p in PERIODS])
        im=heat(ax,values,PERIODS,['Whole cortex']+LOBES,limit=.12,annot='.3f',fontsize=6.0);panel(ax,f'{chr(97+i)}  {m}')
    axs.flat[5].axis('off');fig.subplots_adjust(left=.14,right=.985,bottom=.08,top=.95,hspace=.35,wspace=.64)
    colorbar(fig,im,[.66,.25,.28,.017],'Mean median gradient (SD/year)')
    fig.text(.65,.17,'Descriptive model summaries\nNo between-period significance test\nContinuous cut points:\n19, 30, 40, 50, 60, 77 years',fontsize=7,linespacing=1.4,va='top')
    save(fig,'Figure_S5_AgePeriod_Gradient_Heatmaps','supplementary')

def adjacent_lobes():
    fig,axs=plt.subplots(3,2,figsize=(7.2,8.0))
    labels=['19–29\nto 30–39','30–39\nto 40–49','40–49\nto 50–59','50–59\nto 60–77']
    for i,m in enumerate(M):
        ax=axs.flat[i];g=A[A.Level.eq('Lobe')&A.Hemisphere.eq('Bilateral')&A.Measure.eq(m)]
        vals=np.array([[g[g.Region.eq(r)&g.contrast.eq(c)].Std_Adjusted_Difference.iloc[0] for c in CONTRASTS] for r in LOBES])
        q=np.array([[g[g.Region.eq(r)&g.contrast.eq(c)].q_BH_LevelMeasureContrast.iloc[0] for c in CONTRASTS] for r in LOBES])
        im=heat(ax,vals,labels,LOBES,limit=1.2,annot='.2f',q=q,fontsize=6.0);panel(ax,f'{chr(97+i)}  {m}')
    axs.flat[5].axis('off');fig.subplots_adjust(left=.14,right=.985,bottom=.08,top=.95,hspace=.43,wspace=.64)
    colorbar(fig,im,[.66,.25,.28,.017],'Adjusted difference / residual SD')
    fig.text(.65,.17,'Older minus younger\n* q < .05, ** q < .01\n*** q < .001\nBH within measure × level × contrast',fontsize=7,linespacing=1.4,va='top')
    save(fig,'Figure_S6_Lobar_Adjacent_ANCOVA','supplementary')

def extended_periods():
    # The retained anatomical age-period panels remain reproducible.
    configurations=[('WholeBrain',None,'WholeBrain'),('Hemisphere',None,'Hemisphere'),('Lobe',None,'Lobe_All'),('Lobe','Bilateral','Lobe_Bilateral')]
    for level,hemi,label in configurations:
        g=P[P.Level.eq(level)].copy()
        if hemi:g=g[g.Hemisphere.eq(hemi)]
        g['Row']=g.apply(lambda r:r.Region if r.Level!='Hemisphere' else r.Hemisphere,axis=1)
        if level=='Lobe' and hemi is None:g['Row']=g.Region+' ('+g.Hemisphere+')'
        rows=sorted(g.Row.unique());fig,axs=plt.subplots(3,2,figsize=(7.2,8.0 if len(rows)<10 else 9.5))
        for i,m in enumerate(M):
            z=g[g.Measure.eq(m)];values=np.array([[z[z.Row.eq(r)&z.PeriodLabel.eq(p)].MeanDerivative_per_SD.iloc[0] for p in PERIODS] for r in rows])
            im=heat(axs.flat[i],values,PERIODS,rows,limit=max(.12,float(g.MeanDerivative_per_SD.abs().max())),annot='.3f' if len(rows)<10 else None,fontsize=6 if len(rows)<10 else 5.4);panel(axs.flat[i],f'{chr(97+i)}  {m}')
        axs.flat[5].axis('off');fig.subplots_adjust(left=.20,right=.985,top=.96,bottom=.08,hspace=.4,wspace=.75);colorbar(fig,im,[.67,.16,.27,.016],'Mean gradient (SD/year)');save(fig,'ModelDerived_AgePeriod_'+label,'additional_periods')
    for level,label in [('WholeBrain','WholeBrain'),('Lobe','BilateralLobes')]:
        if level=='Lobe':
            continue  # Identical data are already exported in supplementary Fig. S7.
        g=A[A.Level.eq('WholeBrain')];fig,ax=plt.subplots(figsize=(7.2,3.9))
        v=np.array([[g[g.Measure.eq(m)&g.contrast.eq(c)].Std_Adjusted_Difference.iloc[0] for c in CONTRASTS] for m in M]);q=np.array([[g[g.Measure.eq(m)&g.contrast.eq(c)].q_BH_LevelMeasureContrast.iloc[0] for c in CONTRASTS] for m in M])
        im=heat(ax,v,['30s − 20s','40s − 30s','50s − 40s','60–77 − 50s'],M,limit=1.2,annot='.2f',q=q,fontsize=8)
        panel(ax,'Whole-cortex adjusted adjacent-group contrasts');fig.subplots_adjust(left=.16,right=.98,top=.89,bottom=.23);colorbar(fig,im,[.32,.105,.45,.023],'Adjusted difference / residual SD');save(fig,'ANCOVA_Adjacent_WholeBrain','additional_periods')

if __name__=='__main__':
    OUT.mkdir(exist_ok=True)
    GRADIENT_AUDIT.to_csv(OUT/'constant_gradient_audit.csv',index=False)
    CLASS.to_csv(OUT/'corrected_period_classification.csv',index=False)
    for function in [figure1,figure2,figure3,figure4,figure5,figure6,figure7,figure8]:
        function();print(function.__name__+' complete')
    supplements();extended_periods()
    print('Finished: 8 main figures and 6 supplementary panels; run the supplemental renderer for the remaining figures.')
