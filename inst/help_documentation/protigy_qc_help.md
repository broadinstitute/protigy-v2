# QC Plots

Exploratory quality-control plots for each ome (dataset) after setup. Most tabs let you choose an annotation column for coloring or grouping; the default from setup is pre-selected where applicable. Exports match what you see on each tab.

## Boxplots

**Before normalization** and **after normalization** — distribution of feature values per sample (medians and quartiles). Use to spot outlier samples, global shifts, or normalization effects.

## Profile plots

**Before** and **after normalization** — sample intensity profiles (all features per sample, ordered and colored by annotation). Use to see whether samples cluster by group and whether normalization aligns profiles.

## Correlation

- **Heatmap** — pairwise Pearson correlation between samples (features as variables). Samples are ordered by annotation; color bar shows group membership.
- **Boxplot** — distribution of within-group vs between-group sample correlations. Use to check that replicates correlate more strongly than unrelated samples.

Groups with only one sample cannot be used for the correlation boxplot and are excluded (with a console message).

## PCA

PCA is run on the processed matrix (features × samples, centered and scaled). The subtitle shows how many features were used vs total after filtering.

### Scores plot

Samples projected onto two PCs you select. Points are colored (and optionally shaped) by annotation. Axis labels include % variance explained by each PC. Use to see batch effects, outliers, and whether groups separate in unsupervised space.

### Regression plot

For each PC (up to 10), shows how much of that PC’s variance is associated with the chosen annotation (R² from regressing PC scores on group). The title gives cumulative variance explained across PCs. Use to ask whether known metadata tracks major axes of variation.

### Cumulative loadings plot

For the **top 10 features** (ranked by cumulative squared loading through PC1–PC10, or through all PCs if fewer than 10 exist):

- **X-axis** — PC1 through that same last PC (up to PC10).
- **Y-axis** — cumulative fraction of each feature’s **total squared loading across all PCs** captured through that PC.
- **Legend** — rank and `geneSymbol` (or `id` if no symbol).

This summarizes how concentrated each feature’s PCA loading profile is in the leading components. It is **not** the fraction of dataset variance a protein explains, and it is **not** a supervised importance score. Use for QC: which features are mostly represented on major PCs vs spread across minor ones.

### Loadings export (CSV)

Full loadings table: `rank`, `cumulative_loading_PC1_10` (or `cumulative_loading_PC1_5`, etc., matching how many PCs were used), `id`, `geneSymbol`, and raw loadings for all PCs. Rows are sorted by `rank` (same order as the plot legend).

## Coefficient of variation (CV)

CV is computed **per feature, per group**: SD / mean within each group’s samples. By default, CV is computed from the **raw (non-normalized) data**; use the "Compute CV on normalized data" toggle to switch to the processed matrix instead.

CV is only meaningful on linear-scale intensities. If your data were log-transformed before upload, enter the log base in the **Log base** field (e.g. 2 or 10) so the app can back-transform to linear scale before computing CV. Leave the field blank if your data are already on a linear scale. The log base is detected automatically from your setup parameters where possible.

You can group by one or more metadata columns (combined with `_`). A preview table below the grouping selector shows sample counts per group before you run the analysis.

### Violin plot

Distribution of per-feature CVs across groups. Linear or log10 y-axis. Use to compare reproducibility or dispersion between conditions.

### CV filter (optional)

When enabled, features are kept if CV is **below** the cutoff according to:

- **At least one group** — reproducible in any condition, or  
- **All groups** — reproducible everywhere.

Filtered tables, plots, and a filtered GCT are exported separately. **This filter only affects the CV tab**; it does not change the main processed GCT used elsewhere.

### CV interpretation

- Lower CV within a group usually means more stable quantification across replicate samples.
- For meaningful CV distributions and filtering, use groups with at least three replicates.
- CV across groups answers different questions; compare violins side by side rather than over-interpreting a single cutoff.
- Cutoffs (e.g. 20%) are study-specific; treat the filter as a practical QC threshold, not a universal standard.
