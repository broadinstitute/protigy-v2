# ProTIGY - Proteogenomics Toolset for Integrative Data Analysis

ProTIGY is a Shiny app for matrices of features (e.g. proteins, genes) by samples. You can upload several related datasets (multi-omics), explore them with QC plots, run optional statistics, and export figures and tables.

## Key features

- **QC and plots**: Boxplots, profiles, correlation, PCA, and more (with plot options via the cogwheel icon).
- **Statistics**: Optional moderated tests and volcano plots (**Statistics** tab).
- **Processing**: Normalization, filters, log transform—configured in setup (see **Help → Dataset Setup** for options including gene symbols).
- **Import/export**: GCT v1.3, CSV, TSV, SSV, Excel; export PDFs, GCTs, and CSV results.
- **Customize**: Color schemes for annotations (**Customize** tab; details in **Help → Customization**).

## Using the app

- Use the **sidebar** to upload data and open **Back to Setup** to change preprocessing and the default annotation.
- Multiple datasets appear as tabs on each page; pick the default dataset from the sidebar.

## Getting started

1. Upload your files in one session (same type: all GCT or all CSV, etc.).
2. Assign a short unique label per dataset.
3. For CSV/TSV/SSV/Excel: set feature ID columns and upload sample metadata (**Help → CSV/TSV/SSV/Excel Processing**).
4. Complete **Setup**, then explore **QC** and optional **Statistics**.
5. **Export** when ready.

## More help

Use the other **Help** tabs (**Dataset Setup**, **Statistics**, **Multi-ome**, **Customization**) for step-by-step detail.

## Technical notes

- **R**: 4.0.0+; **RAM**: 8 GB minimum, 16 GB+ for large matrices. [RStudio](https://posit.co/download/rstudio-desktop/) is convenient for running the app.

---

*ProTIGY — Broad Proteomics Platform. Support and issues: [GitHub](https://github.com/broadinstitute/protigy-v2).*
