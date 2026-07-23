# Linear Model

The Linear Model module fits a linear model to every feature in your dataset using the `limma` package (the same engine behind the Statistics module). Where the **Statistics** module runs group-based comparisons, the Linear Model module lets you build a custom model from your sample metadata: multiple variables at once, interactions, continuous covariates, and repeated-measures (blocking) designs. You then read out any coefficient or custom contrast you defined.

The module has three tabs, used in order:

1. **Setup** — choose your data, build the model, define contrasts, and run the fit.
2. **Results** — browse the per-feature results table, adjust the significance cutoff, and inspect p-value histograms.
3. **Volcano Plot** — visualize each coefficient or contrast and label features of interest.

> **Your data should be log-transformed before using this module.** Linear models assume the values are on a log scale. The Setup tab shows a reminder about this.

---

## Setup tab

The Setup tab is arranged in three columns: your **dataset and run controls** on the left, the **model builder** in the middle, and a live **design preview** on the right.

### 1. Choose a dataset

Use **Select dataset** to pick which ome (dataset) to work on. Just below it, **Annotation column** shows the default annotation from setup. If you have more than one dataset, an **Apply to all datasets** checkbox lets you run the same model on every ome at once.

### 2. Select model variables

Under **Select model variables**, pick the sample-metadata columns you want in the model. Every column of your sample metadata is offered here. Each column you pick becomes a term in the model formula, shown live under **Model Formula** (for example `~ treatment + genotype`).

> Avoid columns that uniquely identify each sample (like a sample ID). If every sample has a different value, the model can't be fit.

### 3. Set each variable as Factor or Continuous

Under **Variable Types**, each selected variable gets a **Factor / Continuous** toggle:

- **Factor** — a categorical grouping (e.g. treatment, genotype, batch). The app compares its levels against a reference level.
- **Continuous** — a numeric variable used as a slope (e.g. age, dose, tumor purity). The model estimates how the outcome changes per unit.

The app guesses a sensible default: text columns become factors, and so do numeric columns with only a handful of distinct values (a numeric grouping like "dose" with a few levels is treated as a factor). A numeric column with many distinct values defaults to continuous. You can always override the guess.

### 4. Reference levels

For each **Factor**, the **Reference levels** section lets you choose the baseline level. All other levels are reported *relative to* the reference. For example, with `treatment` and reference `Vehicle`, the coefficient `treatmentDrug` reads as **Drug minus Vehicle**. Pick the biologically meaningful baseline (usually the control).

### 5. Include intercept

The **Include intercept** checkbox is on by default and should stay on in most cases.

- **On (default)** — standard reference coding: `~ variables`. Each factor is measured against its reference level.
- **Off** — cell-means coding: `~ 0 + variables`. The model estimates each group's mean directly rather than differences from a reference. This is useful when you want to build contrasts between *all* levels without a reference — for example, to construct a difference-of-differences (interaction) contrast where every cell exists as its own coefficient.

When you turn the intercept off, a note appears explaining how it changes the coding. With one factor, its reference level no longer matters. With two or more factors, only the first factor becomes group-means coded; the others are still measured against their reference, so those reference choices still matter.

### 6. Interaction terms (optional)

If you selected two or more variables, the **Interaction terms** picker offers every pairwise combination (e.g. `treatment : genotype`). Add an interaction when you expect the effect of one variable to depend on the level of another — for example, "the drug response differs across genotypes." Leave it empty if you only need additive adjustment for covariates.

### 7. Blocking variable (optional) — repeated measures

Use **Blocking variable** for repeated-measures designs: pick the subject identifier (patient, donor, animal) when the same subject was measured more than once. The app then estimates the within-subject correlation and accounts for it in the fit (via `limma`'s `duplicateCorrelation`). A **Clear** button resets the selection.

> A blocking variable is for repeated subjects, not batch effects. Batch or plate variables are usually better added as ordinary factor covariates in **Select model variables**.

### 8. Custom Contrasts

A **contrast** is a specific comparison you want tested *after* the model is fit. Each contrast card builds one comparison and adds its own column to the results, keyed by an editable label. Empty cards are ignored when you run the model.

Each card has two modes:

**Single coef (1x1)** — a one-vs-one comparison. Pick a **Numerator** and a **Denominator** from the coefficient dropdowns; a swap button flips them. This tests numerator minus denominator (e.g. `treatmentDrug − treatmentVehicle`).

**Multi coef (2x2)** — a difference-of-differences (interaction) contrast, laid out as:

```
( A − B ) − ( C − D )
```

Pick four coefficients from the A, B, C, and D dropdowns. This asks whether the `A − B` difference differs from the `C − D` difference — for example, "is the drug-vs-vehicle effect different in one genotype versus another?"

A few things to know about both modes:

- **The dropdowns hold model coefficients, not raw metadata values.** They list the design-matrix coefficient names produced by your formula (the intercept is excluded, since comparing a group against the grand mean isn't a meaningful between-group contrast).
- **Four-cell interaction contrasts need all four cells to exist as coefficients.** Under the default intercept-on coding, reference cells are folded into the intercept and don't appear as separate coefficients. If you want the four literal cell handles for a Multi contrast, turn **Include intercept** off so all cells become coefficients.
- **The label is auto-generated but editable.** The app strips the shared variable prefix so a contrast's label reads as `Drug-Vehicle` rather than `treatmentDrug-treatmentVehicle`; a Multi contrast reads like `(WT-KO)-(Drug-Vehicle)`. Type your own label to override it. This label names the contrast's columns in the results and exports (with special characters replaced by dots — see the Results table below).
- **Live validation.** Each card shows the assembled expression (`expr:`) and turns green when valid. A card is invalid if any slot is empty, if a comparison cancels to zero (numerator equals denominator, or the two Multi pairs cancel), or if a coefficient is unrecognized. A summary below all cards confirms how many contrasts are ready to run.

Use **+ Add contrast** to add another card, and **Clear all** to reset back to a single blank card.

### 9. Design preview and Model Coefficients

The right column shows a live **Design Matrix Preview** (the first 10 rows of the actual design that will be fit) and a caption reporting how many samples are used (some may be dropped if a chosen variable is missing for them). What you preview here is exactly what gets fit.

Below the preview, **Model Coefficients** is a checklist of every coefficient the model will estimate. Unchecking a coefficient **hides it** from the Results and Volcano tabs (and from the per-coefficient PDF exports — the volcano and p-value-histogram PDFs) — but it does **not** remove it from the model, and the full results CSV, the JSON model summary, and the ssGSEA GCT still contain every coefficient. To actually drop a term from the model, uncheck the variable up in **Select model variables**.

### 10. Run the model

Click **Run Linear Model**. The app fits the model for the selected dataset (or all datasets if **Apply to all datasets** is checked) and automatically switches you to the **Results** tab. If a fit fails, a message explains why rather than crashing.

---

## Results tab

Each dataset gets its own tab. For the selected dataset you can browse the results table, tune the significance cutoff, and inspect diagnostic histograms.

### Cutoff Selection

The **Cutoff Selection** box controls what counts as significant. Its choices are also applied to the Volcano Plot tab.

- **Choose stat** — use adjusted p-values (`adj.p.val`, the default) or nominal/raw p-values (`nom.p.val`).
- **Choose cutoff** — the significance threshold (default `0.05`).
- **Apply cutoff to all datasets** — copy the current cutoff to every dataset. Unchecking restores *this* dataset's previous cutoff; other datasets keep the value that was applied.

> This cutoff is specific to the Linear Model module. It starts at `0.05` and is independent of the cutoff used in the Statistics module, even though the idea is the same.

### Results table

**Select coefficient to display** chooses which coefficient or contrast to view. The table then shows, for every feature:

- **id** — the feature identifier (protein/gene/site).
- **geneSymbol** — the human-readable gene symbol, when available.
- **logFC** — the effect size (log2 fold change) for that coefficient or contrast: how large and in which direction the effect is.
- **P.Value** — the nominal (unadjusted) p-value.
- **adj.P.Val** — the adjusted p-value (Benjamini-Hochberg FDR). See the note below on how it's computed.
- **logSignP** — a signed significance score, `−sign(logFC) × log10(p)`. It combines direction and significance into one number and is mainly used for the ssGSEA export.

Each column name carries the coefficient/contrast it belongs to, so results for different coefficients never mix. Note that the suffix is a "safe" version of the name — special characters become dots — so a contrast you labeled `Drug-Vehicle` shows up as `logFC.Drug.Vehicle` (the hyphen becomes a dot). The table is sortable and searchable.

For a multi-level factor or interaction that you did not turn into a contrast, the model also runs an overall F-test (`F`, `P.Value`, `adj.P.Val` for the whole factor — no `logFC`, since an F-test has no single direction), which asks whether *any* of its levels differ. These F-test columns appear in the exported results **CSV**, but the overall factor is not selectable in the on-screen "Select coefficient to display" dropdown (that dropdown lists only coefficients and contrasts that have a `logFC`).

### How adjusted p-values are computed

Adjusted p-values are Benjamini-Hochberg (FDR) corrected **separately for each coefficient or contrast, within each dataset**. In other words, each coefficient forms its own correction family across all features. A practical consequence: switching the coefficient dropdown changes the entire adjusted-p landscape, because you are looking at a different correction family. Nominal (raw) p-values are unaffected by this.

### P-value histograms

The histogram box shows the **adjusted** and **nominal** p-value distributions side by side for a coefficient you pick in the sidebar. A red line marks the active cutoff (translated consistently onto whichever scale is shown). A healthy distribution is roughly flat with a spike near zero; a strong peak at high p-values can signal a modeling issue.

### Alpha-level analysis (advisory)

The **Alpha-level Analysis** box runs a quick check on the tail of the nominal p-value distribution and may suggest an alpha level. This is **advisory only** — a rough heuristic, not a rigorous false-discovery estimate. If you click **Apply**, it sets the stat to nominal and the cutoff to the suggested value (because the suggestion is derived on the nominal scale). If the check is inconclusive, the box tells you to inspect the histogram manually.

### Dataset and Workflow information

Read-only boxes recap the fitted model: the formula, variables and their types, intercept setting, blocking variable, active cutoff and stat, plus any interactions and contrasts (shown as `label: expression`). A "Features tested" count and per-coefficient "Significant" counts summarize the outcome.

---

## Volcano Plot tab

The Volcano Plot visualizes one coefficient or contrast at a time.

- **Axes** — the x-axis is log2 fold change / effect size; the y-axis is `−log10` of the **nominal** p-value.
- **Significance** — significant points are dark red, everything else gray, with a horizontal line at the cutoff. Note that the y-axis is always the nominal p-value, but *which points are colored significant* (and where the line sits) follows your **Choose stat** and **Choose cutoff** settings from the Results tab. If you switch from adjusted to nominal, the line moves and the coloring updates accordingly.
- **Select Coefficient** — the sidebar radio buttons pick which coefficient/contrast to plot.

### Labeling features

Under **Label Proteins** you can turn on any combination of:

- **Proteins of interest** — your own curated list (see below).
- **Top 20 significant** — the 20 strongest significant features.
- **All significant** — labels every significant point.

Labels show the gene symbol (or the feature id if no symbol exists) and are drawn in magenta. If too many labels overlap, some are hidden and a note reports how many were dropped.

### Building a proteins-of-interest list

- **Search Proteins** — choose a metadata column to search, paste one or more IDs (separated by spaces, commas, or semicolons), and click Search. Matches are added to your list; anything not found is reported.
- **Click a point** on the plot to add that feature to the list.
- Each selected feature appears with a **×** to remove it, plus a **Clear all** button.

Adding proteins of interest automatically enables the "Proteins of interest" label mode.

---

## Exports

When Linear Model outputs are selected on the **Export** tab, the module writes, per dataset:

- **`lm_results_<ome>.csv`** — the full results table (all coefficients and contrasts).
- **`lm_volcano_plots_<ome>.pdf`** — one volcano page per visible coefficient/contrast, using your current labels and proteins of interest.
- **`lm_adj_pval_hist_<ome>.pdf`** and **`lm_nom_pval_hist_<ome>.pdf`** — p-value histograms, one page per coefficient.
- **`lm_proteins_of_interest_<ome>.csv`** — the rows matching your proteins-of-interest list (skipped if the list is empty).
- **`lm_parameters_<ome>.txt`** and **`lm_model_summary_<ome>.json`** — the model configuration (formula, variables, types, reference levels, intercept, interactions, blocking, contrasts, cutoff, stat), as text and structured JSON.
- **`lm_stat_results_for_ssGSEA_<ome>.gct`** — a signed-significance GCT for downstream ssGSEA (skipped for F-test-only models, which have no signed effect).

---

## Linear Model vs. Statistics — which to use?

- Use **Statistics** for straightforward group comparisons (one-sample, two-sample, or F-tests across the levels of a single annotation).
- Use **Linear Model** when you need to model several variables together, adjust for continuous covariates, test interactions, handle repeated measures, or build custom difference-of-differences contrasts.

Both modules use `limma` under the hood and share the same volcano and significance conventions, so results are directly comparable in spirit.
