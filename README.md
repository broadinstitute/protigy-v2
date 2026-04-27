# ProTIGY - Proteogenomics Toolset for Integrative Data Analysis

![ProTIGY Workflow](inst/doc_images/workflow_diagram.png)

ProTIGY is a Shiny application that supports datasets organized as a matrix with features (proteins, genes, transcripts) measured across samples (experimental conditions, replicates). ProTIGY can analyze various omics data types including proteomics, post-translational modifications (PTMs), RNA-seq, metabolomics, and other quantitative molecular datasets. ProTIGY allows you to upload and process multiple data types from the same experiment simultaneously (e.g., RNA-seq, proteome, and phosphoproteome data from the same samples), enabling integrated multi-omics analysis.

## Installation

Enter the following code into your command line interface.

```bash
# Clone the repository
git clone https://github.com/broadinstitute/protigy-v2.git
```

Once the repository is cloned, open RStudio and enter the following code.

```R
# Change to the repo folder
setwd("protigy-v2")

# Install and load devtools. NOTE: After installing once, you don't need to install every time. Just use library()
install.packages('devtools')
library(devtools)

# Install the package. NOTE: After installing once, you don't need to install every time. Just use library()
devtools::install('.')

# Load the package and start the app
library(Protigy)
Protigy::launchApp()
```

## Key Features

### 📊 **Data Analysis & Visualization**
- **Quality Control (QC)**: Boxplots, profile plots, CV plots, correlation analysis, and PCA plots
- **Statistical Analysis**: Moderated t-tests, F-tests, and volcano plots
- **Interactive Plots**: Zoom, pan, and explore your data
- **Summary Statistics**: Data overview and sample information
- **Color Customization**: Fully customize color schemes for all annotation columns across all datasets

### 🔧 **Data Processing**
- **Normalization**: Multiple methods including median, quantile, and VSN normalization
- **Filtering**: Sample- and row-metadata filters, missing/low-variance filters, and related options
- **Transformation**: Log transformation and other preprocessing
- Setup details (gene symbols, filters, etc.): **Help → Dataset Setup** in the app

### 📁 **Data Import & Export**
- **Multi-omics Support**: Upload and analyze multiple data types from the same experiment simultaneously
- **Supported Formats**: GCT v1.3, CSV, TSV, SSV (semicolon-separated), and Excel files
- **Export Options**: High-quality figures (PDF), GCT files for data, and CSV files for statistics

## UI Navigation

### Sidebar Controls
- Use the **Sidebar** to upload your dataset(s) and configure analysis settings
- Click the **arrow icon** (← Collapse / → Expand) next to the sidebar to close/open the sidebar
- You may change the default dataset anytime using the sidebar

### Multiple Dataset Tabs
If multiple datasets are uploaded, there will be multiple tabs on each page allowing you to view the plots for each dataset separately. You can change the default dataset using the sidebar.

### Plot Customization
Many plots have a **double cogwheel** icon in the top right corner. Clicking this icon provides customization options for the plots. Your customization options will be saved and used for exports.

### Color Customization
The **Customize** tab allows you to fully customize color schemes for all annotation columns across all datasets. You can:
- Maintain consistent colors across datasets (multi-ome mode) or customize each dataset independently (per-ome mode)
- Import and export color schemes as YAML files
- Restore default colors or reset to original app-generated colorblind-safe palettes
- Customize colors for any discrete annotation column in your data

See the **Help → Customization** tab for detailed instructions.

### Changing Settings
If you need to change settings such as normalization/filtering or the default annotation, use the "Back to Setup" button in the sidebar to modify these options.

## Quick Start

### 1. **Upload Your Data**
- Upload one or more files from the same experiment (e.g., different omes such as RNA-seq, prot, phos)
- Files should have overlapping samples but not all samples need to be in all files

**Supported Formats:**
- **GCT**: Gene Cluster Text format (`.gct`) - v1.3 format
- **CSV**: Comma-separated values (`.csv`)
- **TSV**: Tab-separated values (`.tsv`)
- **SSV**: Semicolon-separated values (`.ssv`)
- **Excel**: Microsoft Excel files (`.xlsx`, `.xls`)

**File Requirements:**
- **GCT files**: Must follow GCT v1.3 format specification
- **CSV/TSV/SSV/Excel files**: First row must contain column headers
- Data should have features as rows, samples as columns
- Missing values should be empty cells or `NA`
- All files must be the same type

**Test Data Available:**
- Sample datasets are included in `inst/extdata/` for testing and learning
- **GCT files**: `mb-proteome-ratio-norm-NArm.gct`, `mb-phosphoproteome-ratio-norm-NArm.gct`, and `mb-acetylome-ratio-norm-NArm.gct`
- **CSV/TSV**: matching tabular versions of those three datasets (`mb-*-ratio-norm-NArm.csv` and `.tsv`)
- **Experimental design**: `experimental_design.csv` (for workflows that require a design file)
- Use these files to explore ProTIGY's features before uploading your own data

### 2. **Assign Labels**
Assign meaningful labels to each of your uploaded files. These labels will be used throughout the analysis to identify your datasets.

**Examples of good labels:**
- "prot" (for proteome)
- "phos" (for phosphoproteome) 
- "acetyl" (for acetylome)
- "RNA-seq"

**Requirements:**
- Each label must be unique
- Labels cannot be empty
- Keep labels concise (e.g., "prot" instead of "proteome")

### 3. **Additional Setup for CSV/TSV/SSV/Excel Files**
For CSV/TSV/SSV/Excel files, you'll also need to:
- Select identifier columns (choose which column contains unique feature identifiers)
- Upload experimental design metadata (sample information and experimental conditions)

### 4. **Configure and process each dataset (Setup)**
After upload (and CSV/TSV/SSV/Excel design, if applicable), work through **Setup** in the sidebar for each dataset before analysis:

- **Normalization** (e.g. median, quantile, VSN) and **transformation** (e.g. log2) suited to your data type
- **Filtering**: remove samples or features using metadata rules, missing-value cutoffs, low-variance options, and related controls
- **Default annotation**: pick the sample annotation column used for coloring QC plots and for statistical grouping (it must match your experimental design)
- Optional: **Gene symbol** column and **ID-to-symbol mapping** for display and results (see **Help → Dataset Setup** for detail)
- **Submit** (or equivalent) when ready so ProTIGY builds the processed matrices used in **QC**, **Statistics**, and **Export**. Use **Back to Setup** later if you need to change these steps.

### 5. **Explore Your Data**
- Use the **QC** tabs to examine data quality:
  - **Boxplots**: Check data distribution across samples
  - **Profile Plots**: Visualize individual feature profiles
  - **Correlation**: Assess sample relationships
  - **PCA**: Identify patterns and outliers
  - **CV**: Examine group-wise coefficient of variation distributions and optional CV filtering exports (valid for non-log-transformed intensity data only)

### 6. **Run Statistical Analysis** (Optional)
- Statistical analysis is optional - you can use ProTIGY just for QC and data export
- Navigate to **Statistics** → **Setup** to configure your analysis
- Select statistical tests based on your experimental design
- View results across multiple Statistics subtabs (including **Summary**, **Volcano Plot**, and others)

### 7. **Export Results**
- Export high-quality figures (PDF), data files (GCT), and statistical results (CSV)
- Select which datasets and which modules to export using the dropdown menus

## Technical Requirements

- **R Version**: 4.0.0 or higher
- **Memory**: Minimum 8GB RAM recommended; 16GB+ for large datasets (>10,000 features and >50 samples)
- **Windows build tools**: [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for package installation from source
- **macOS build tools**: [Xcode Command Line Tools](https://developer.apple.com/xcode/resources/) for package installation from source

## Recommended Software

- **RStudio**: [Download RStudio](https://www.rstudio.com/products/rstudio/download/) (recommended for running ProTIGY)

## Getting Help

- **General Help**: The application includes comprehensive help documentation accessible through the Help tab
- **Analysis Help**: Detailed guidance on analysis parameters and options is available within the application
- **Technical Support**: For technical support or feature requests, please submit via [GitHub Issues](https://github.com/broadinstitute/protigy-v2/issues)

## Development

This is a revamp of the original [Protigy app](https://github.com/broadinstitute/protigy) with enhanced multi-omics capabilities and improved user interface.

---

*ProTIGY is developed and maintained by the Broad Proteomics Platform. For technical support or feature requests, please submit via [GitHub](https://github.com/broadinstitute/protigy-v2).*
