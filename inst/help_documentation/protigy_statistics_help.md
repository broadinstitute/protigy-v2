# Statistical Analysis

This section explains the statistical testing options available in ProTIGY for analyzing your normalized and filtered dataset.

## Annotation Selection

**The default annotation chosen during setup is used for all statistical analysis.** If you want to use a different annotation, you need to go back to the setup menu and change that setting before running the analysis.

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
- **Y-axis**: -log10(p-value) (statistical significance)
- **Significant features**: Points above the horizontal line (p-value cutoff)

### Summary Statistics
- **Total features tested**: Number of features included in the analysis
- **Significant features**: Number of features passing the p-value and fold change cutoffs
- **Effect size distribution**: Range and distribution of p-values
