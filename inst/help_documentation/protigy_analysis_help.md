### Intensity data

Check the box if you are using raw or log-transformed intensity data. Only the relevant normalization methods and statistical tests for intensity data will now show under 'Data normalization' and 'Select test'.

### Log-transformation

Apply log transformation to the data.

### Normalize per group

If enabled the normalization will be performed within a particular group (Median, Median-MAD, Quantile, VSN). For Median and Median-MAD normalization, the group-level median of sample medians is added to each normalized data value.

### Data normalization

You can apply different normalization methods to the data prior to testing. The methods are applied for each sample (column) separately, except for 'Quantile' and 'VSN' normalization which take the entire matrix into account.

-   **Median**: Subtract the sample median from each value (centering). After normalization, all samples have a median of zero. Intended to be used with **log-transformed ratios**.
-   **Median (non-zero)**: Subtract the sample median from each value and add back the median of all sample medians (which will be the common sample median after normalization). Intended to be used with **log-transformed intensities**.
-   **Median-MAD**: Subtract the sample median and divide by sample MAD (centering plus scaling). After normalization, all samples have a median of zero. Intended to be used with **log-transformed ratios**.
-   **Median-MAD (non-zero)**: Subtract the sample median and divide by sample MAD, and add back the median of all sample medians (which will be the common sample median after normalization). Intended to be used with **log-transformed intensities**.
-   **Upper quartile**: Subtract the sample's 75th percentile from each value. Intended to be used with **log-transformed intensities**.
-   **2-component**: Use a mixture-model approach to separate non-changing from changing features. Z-score all features using the mean and standard deviation of the non-changing features. Intended to be used with **log-transformed ratios**.
-   **Quantile**: Transform the data such that the quantiles of all sample distributions are the equal. **Use with caution as this type of normalization can remove potentially meaningful outliers from the data**.
-   **VSN**: Variance stabilizing normalization. Intended to be used with **raw intensity values**.
-   **none**: The data will be taken as is. Use this option if the data has already been normalized.

### Filter data

**You can filter the data by p-value (non-adjusted or adjusted) and change the p-value cutoff once running the analysis (on the next screen).**

**Missing data:**\
Remove features not quantified in percent of samples specied in the text field. For intensity data, the missing data rate is capped at 99% to prevent downstream statistical testing from throwing an error.

**Reproducibility:**\
Remove features that were not reproducibly quantified across replicate measurements of a group. For duplicate measurements a Bland-Altman Filter of 99.9% (+/-3.29 sigma) will be applied. For more than two replicate measurements per group a generalized reproducibility filter is applied which is based on a linear mixed effects model to model the within-group variance and between-group variance (See 'MethComp book (pp 58-61). *Comparing Clinical Measurement Methods* by Bendix Carstensen' for more details). You can inspect the results of the filtering step in the multiscatter plot under the 'QC'-tab as well as in the interactive scatterplots. Data points removed prior to testing will be depicted in blue. **This type of filter is applied separately to each group.** **This filter is only appropriate for ratio data.**

**StdDev:**\
Remove features with low standard deviation across all samples. Only useful if applied to sample cohorts that were quantified against a common reference. The percentile ***P*** you specify in the slider refers to the ***P*** percent of features having the **lowest standard deviation** across sample columns which will be **excluded prior to analyis**. Using this type of filter is useful to explore result of unsupervised clustering of the data without running a statistical test.

### Select test

You can choose between a one-sample, two-sample moderate T-tests, moderated F-test or no testing.

-   **One-sample mod T**: For each group test whether the group mean is significantly different from zero. Only meaningful to **ratio data**!
-   **Two-sample mod T**: For each possible pairwise comparison of groups test whether the group means are significantly different from each other.
-   **mod F**: Test whether there is a significant difference between any of the defined groups. Should be used if more than 2 groups are being compared.
-   **none**: Don't do any test. Useful for exploratory data analysis such as PCA.
