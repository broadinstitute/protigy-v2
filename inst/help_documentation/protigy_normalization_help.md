# Dataset Setup

This section explains the data preprocessing options available in ProTIGY for setting up your dataset before analysis.

## Analysis Annotation Column

This is the default annotation column used for all analysis in ProTIGY. This column contains sample metadata that will be used for grouping and analysis throughout the workflow.

- **Purpose**: Provides consistent sample grouping for visualization and statistical analysis
- **Usage**: Used for sample grouping in plots, statistical tests, and analysis results
- **Content**: Contains sample metadata such as tumor type (tumor vs NAT), treatment group (drug A, B, C), time point, etc.
- **Important**: This column should contain **discrete/categorical data** (e.g., treatment groups, cell lines, conditions) for best results.

### Visualization vs. Statistical Testing

**You can select an analysis annotation column for visualization even if it is not compatible with statistical testing.** This allows you to explore your data visually before deciding on the appropriate annotation for statistical analysis.

- **Visualization**: Any discrete/categorical annotation column can be used for visualization in QC plots, PCA, and other exploratory analyses
- **Statistical Testing**: To perform statistical tests, the annotation column must meet additional requirements:
  - **At least 2 categories**: The column must have at least 2 distinct groups for comparison
  - **Multiple samples per group**: The column cannot be an ID column where every value is unique (1 sample per group)
    - ID columns (like sample IDs or patient IDs) are not suitable for statistical testing because each group would have only 1 sample
    - You need multiple samples in each group to perform meaningful statistical tests

**What happens if your annotation is not suitable for statistics?**
- You can still use it for **visualization** in all QC plots and exploratory analyses
- A warning message will appear in the Statistics tab explaining why testing is not available
- The test selection dropdown will only show "None" as an option
- The "Run Test" button will be disabled
- To perform statistical testing, return to the **Setup** tab and select a different annotation column that has multiple samples per group

## Gene Symbol Column

If your dataset contains gene symbol information, you can specify which column contains the gene symbols:

- **Default**: If a column named "geneSymbol" exists, it will be automatically selected
- **Custom**: You can select any other column from your dataset's row metadata (rdesc) to use as gene symbols
- **None**: If no gene symbol column is available, select "None"

**Note**: 
- If `geneSymbol` does not exist and you select another column, that column's values will be copied to create a `geneSymbol` column, and the original column will be preserved in the dataset.
- If `geneSymbol` already exists and you select a different column, the original `geneSymbol` column will be preserved as `geneSymbol_original`, and the selected column will become the new `geneSymbol` column. The selected column will also be preserved in the dataset.
- If `geneSymbol` already exists and you select "None" or "geneSymbol" itself, the existing `geneSymbol` column will be kept unchanged.
- **Blank gene symbols**: Blank or empty gene symbol values are converted to `NA` and all rows are preserved. Features without gene symbols are kept in the analysis.

## Data Preprocessing Options

The following sections explain the data preprocessing options available for normalizing and filtering your dataset before statistical analysis.

## Intensity Data

Set this dropdown to "Yes" if you are using raw or log-transformed intensity data. **By default, ProTIGY assumes your data are ratio data unless this is set to "Yes".** This setting will show only the relevant normalization methods and statistical tests appropriate for intensity data.

## Log Transformation

**Log transformation is REQUIRED for all normalization and statistical analysis in ProTIGY.** If this is set to "none", it is assumed your data are already log-transformed.

Apply log transformation to your data. This is necessary for most omics data to stabilize variance and make the data suitable for statistical analysis.

## Data Normalization Methods

You can apply different normalization methods to your data prior to statistical testing. Most methods are applied to each sample (column) separately, except for 'Quantile' and 'VSN' normalization which consider the entire matrix.

### Normalize Per Group
When enabled, normalization will be performed within each experimental group separately. Any normalization method can be performed group-wise. For Median and Median-MAD normalization, the group-level median of sample medians is added back to each normalized data value.

### For Log-Transformed Ratios:
- **Median**: Subtract the sample median from each value (centering). All samples will have a median of zero after normalization.
- **Median-MAD**: Subtract the sample median and divide by sample MAD (centering plus scaling). All samples will have a median of zero after normalization.
- **2-component**: Use a mixture-model approach to separate non-changing from changing features. Features are z-scored using the mean and standard deviation of non-changing features. **Note: This method is disabled for datasets with more than 20 samples due to processing time constraints.**

### For Log-Transformed Intensities:
- **Median (non-zero)**: Subtract the sample median and add back the median of all sample medians. This preserves the overall intensity scale.
- **Median-MAD (non-zero)**: Subtract the sample median, divide by sample MAD, and add back the median of all sample medians.
- **Upper quartile**: Subtract the sample's 75th percentile from each value.

### For Raw Intensity Values:
- **VSN**: Variance stabilizing normalization. This method is specifically designed for raw intensity data.

### Global Normalization:
- **Quantile**: Transform the data so that the quantiles of all sample distributions are equal. **Use with caution** as this can remove potentially meaningful outliers.

### Data Already Normalized:
- **None**: Use this option if your data has already been normalized elsewhere.

## Data Filtering Options

### Missing Data Filter
Remove features that are not quantified in a specified percentage of samples. For intensity data, the missing data rate is capped at 99% to prevent statistical testing errors.

**Recommendation**: Start with 50-70% missing data threshold and adjust based on your data quality.

### Standard Deviation Filter
Remove features with low standard deviation across all samples. This is useful for sample cohorts quantified against a common reference.

The percentile you specify refers to the percentage of features with the **lowest standard deviation** that will be **excluded** from analysis. This filter is useful for exploratory clustering without running statistical tests.
