# Statistical Analysis

This section explains the statistical testing options available in ProTIGY for analyzing your normalized and filtered dataset.

## Annotation Selection

**The default annotation chosen during setup is used for all statistical analysis.** If you want to use a different annotation, you need to go back to the setup menu and change that setting before running the analysis.

### Annotation Requirements for Statistical Testing

Statistical testing requires an annotation column that meets the following criteria:

- **At least 2 categories**: The annotation column must have at least 2 distinct groups/categories for comparison
- **Multiple samples per group**: The annotation column cannot be an ID column where every value is unique (1 sample per group)
  - ID columns (like sample IDs or patient IDs) are not suitable for statistical testing because each group would have only 1 sample, which is insufficient for statistical comparison
  - You need multiple samples in each group to perform meaningful statistical tests

**What happens if your annotation is not suitable?**
- A prominent warning message will appear at the top of the Statistics tab explaining why testing is not available
- The test selection dropdown will only show "None" as an option
- The "Run Test" button will be disabled
- To fix this, return to the **Setup** tab using the sidebar button and select a different annotation column that has multiple samples per group

## Statistical Tests

ProTIGY offers several statistical tests based on the `limma` package, which provides moderated t-tests and F-tests for omics data analysis.

### One-Sample Moderated T-Test
- **Purpose**: Test whether the group mean is significantly different from zero
- **Use case**: Only meaningful for **ratio data** (e.g., log2 fold changes)
- **When to use**: When comparing each group against a reference value of zero

### Two-Sample Moderated T-Test
- **Purpose**: Test whether group means are significantly different from each other
- **Use case**: Pairwise comparisons between experimental groups
- **When to use**: When comparing two groups directly (e.g., treatment vs. control)

#### Selecting Contrasts

ProTIGY provides multiple ways to select pairwise contrasts for two-sample t-tests:

**Quick Select Buttons:**
- **All Pairwise**: Select all possible pairwise contrasts (both directions: A/B and B/A)
- **All vs Control**: Selects all contrasts vs manually specified control group(s). First check "Manually specify control group(s)" and select your control groups, then click this button.
- **Sequential Pairs**: Select sequential comparisons comparing later to earlier groups (e.g., for Time_1, Time_2, Time_3, generates Time_2/Time_1, Time_3/Time_2) - useful for time-series or ordered experimental designs
- **Clear All**: Deselect all contrasts

**Manual Control Group Selection:**
- Check the "Manually specify control group(s)" checkbox to enable control group selection
- Click on group badges to select which groups should be used as controls
- Once control groups are selected, use the "All vs Control" button to generate all contrasts vs the selected control groups

**Matrix View Selection:**
- Interactive grid where you can click cells to select contrasts
  - Columns represent numerator groups (what you're testing)
  - Rows represent denominator groups (what you're comparing against)
  - Click any cell to toggle contrast selection
  - Blue checkmark (✓) indicates selected contrasts

**Selected Contrasts Panel:**
- Shows a summary of all selected contrasts
- Displays count (e.g., "42 of 380 contrasts selected")
- Click the × button next to any contrast to remove it

### Moderated F-Test
- **Purpose**: Test whether there is a significant difference between any of the defined groups
- **Use case**: Multiple group comparisons
- **When to use**: When you have more than 2 groups and want to identify any significant differences

### No Testing
- **Purpose**: Skip statistical testing for a specific dataset
- **Use case**: When you want to exclude a particular dataset from statistical analysis
- **When to use**: If you only want to analyze certain datasets and skip others in your multi-omics analysis

## P-Value Filtering

You can filter results based on p-values (both raw and adjusted) and adjust the p-value cutoff after running the analysis.

### P-Value Adjustment Methods
- **Raw p-values**: Unadjusted p-values from the statistical test
- **Adjusted p-values**: Multiple testing correction (e.g., Benjamini-Hochberg FDR)

### Setting P-Value Cutoffs
- **Default**: Often 0.05 for raw p-values, 0.05-0.1 for adjusted p-values
- **Adjustment**: You can change the cutoff after running the analysis
- **Interpretation**: Lower cutoffs are more stringent, higher cutoffs are more permissive

## Results Interpretation

### Volcano Plots
- **Available for**: One-sample and Two-sample t-tests only
- **Not available for**: F-tests (multiple group comparisons)
- **X-axis**: Log2 fold change (effect size)
- **Y-axis**: -log10(nominal p-value) (statistical significance)
- **Significant features**: Points above the horizontal line (p-value cutoff)
- **Cutoff controls**: The same **stat** (nominal vs adjusted *p*-value) and **cutoff** value as **Statistics → Summary** apply to the volcano plot and to the horizontal line
- **Label proteins** (optional checkboxes): **Proteins of interest** (search by ID/metadata column or click a point to toggle it in the list), **Top 20 significant** (among points above the line: ranked by *y*-axis significance, then by absolute log2 fold change; if many features tie at the cutoff band, more than 20 may be labeled), or **All significant** (can be crowded)
- **Label color**: Magenta highlights for labeled points and text (distinct from dark red significant scatter points)

### Summary Statistics
- **Differential expression summary**: Table columns depend on the test (**Group** for one-sample, **Numerator** / **Denominator** for two-sample contrasts, or feature/DE counts only for the moderated F-test). The significance **cutoff** is set in **Cutoff Selection** on the same tab (it is not duplicated as a table column).
- **Total features tested**: Number of features included in the analysis
- **Significant features**: Number of features passing the p-value and fold change cutoffs
- **Effect size distribution**: Range and distribution of p-values
