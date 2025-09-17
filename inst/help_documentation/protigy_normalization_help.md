# Dataset Setup

This section explains the data preprocessing options available in ProTIGY for setting up your dataset before analysis.

## Analysis Annotation Column

This is the default annotation column used for all analysis in ProTIGY. This column contains sample metadata that will be used for grouping and analysis throughout the workflow.

- **Purpose**: Provides consistent sample grouping for statistical analysis and visualization
- **Usage**: Used for sample grouping in plots, statistical tests, and analysis results
- **Content**: Contains sample metadata such as tumor type (tumor vs NAT), treatment group (drug A, B, C), time point, etc.

## Gene Symbol Column

If your dataset contains gene symbol information, you can specify which column contains the gene symbols:

- **Default**: If a column named "geneSymbol" exists, it will be automatically selected
- **Custom**: You can select any other column from your dataset's row metadata (rdesc) to use as gene symbols
- **None**: If no gene symbol column is available, select "None"

**Note**: If you select a column other than "geneSymbol", that column will be renamed to "geneSymbol" for downstream analysis. If a "geneSymbol" column already exists, it will be preserved as "geneSymbol_original".

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
- **2-component**: Use a mixture-model approach to separate non-changing from changing features. Features are z-scored using the mean and standard deviation of non-changing features. **Warning: This method is extremely slow for datasets with many (>20) samples.**

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
