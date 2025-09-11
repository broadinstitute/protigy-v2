# ProTIGY - Proteogenomics Toolset for Integrative Data Analysis

ProTIGY is a Shiny application that supports datasets organized as a matrix with features (proteins, genes, transcripts) measured across samples (experimental conditions, replicates). ProTIGY can analyze various omics data types including proteomics, post-translational modifications (PTMs), RNA-seq, metabolomics, and other quantitative molecular datasets. ProTIGY allows you to upload and process multiple data types from the same experiment simultaneously (e.g., RNA-seq, proteome, and phosphoproteome data from the same samples), enabling integrated multi-omics analysis.

## Key Features

### 📊 **Data Analysis & Visualization**
- **Quality Control (QC)**: Boxplots, profile plots, correlation analysis, and PCA plots
- **Statistical Analysis**: Moderated t-tests, F-tests, and volcano plots
- **Interactive Plots**: Zoom, pan, and explore your data
- **Summary Statistics**: Data overview and sample information

### 🔧 **Data Processing**
- **Normalization**: Multiple methods including median, quantile, and VSN normalization
- **Filtering**: Remove missing data, low-variance features, and non-reproducible measurements
- **Transformation**: Log transformation and other data preprocessing options

### 📁 **Data Import & Export**
- **Multi-omics Support**: Upload and analyze multiple data types from the same experiment simultaneously
- **Supported Formats**: GCT v1.3 format (TSV/CSV/Excel support coming soon)
- **Export Options**: High-quality figures (PDF), Excel files, and analysis reports

## Getting Started

### 1. **Upload Your Data**
- Use the **Sidebar** to upload your dataset(s)
- Upload multiple GCT files at the same time for multi-omics analysis (e.g., RNA-seq, proteome, phosphoproteome)
- Currently supported format: GCT v1.3 files only
- TSV/CSV/Excel support coming soon
- You may change the default dataset anytime using the sidebar

### 2. **Explore Your Data**
- Use the **QC** tabs to examine data quality:
  - **Boxplots**: Check data distribution across samples
  - **Profile Plots**: Visualize individual feature profiles
  - **Correlation**: Assess sample relationships
  - **PCA**: Identify patterns and outliers

### 3. **Run Statistical Analysis** (Optional)
- Statistical analysis is optional - you can use ProTIGY just for QC and data export
- Navigate to **Statistics** → **Setup** to configure your analysis
- Select statistical tests based on your experimental design
- View results across multiple Statistics subtabs

### 4. **Export Results**
- Download high-quality figures and analysis reports
- Export processed data in various formats

## Need Help?

- **General Help**: This tab provides an overview of ProTIGY's capabilities
- **Analysis Help**: Detailed guidance on analysis parameters and options

## Technical Requirements

- **R Version**: 4.0.0 or higher
- **Memory**: Minimum 8GB RAM recommended; 16GB+ for large datasets (>10,000 features and >50 samples)

## Recommended Software

- **RStudio**: [Download RStudio](https://www.rstudio.com/products/rstudio/download/) (recommended for running ProTIGY)

---

*ProTIGY is developed and maintained by the Broad Proteomics Platform. For technical support or feature requests, please submit via [GitHub](https://github.com/broadinstitute/protigy-v2).*
