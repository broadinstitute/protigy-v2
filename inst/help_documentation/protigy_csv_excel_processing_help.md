# CSV/TSV/Excel Processing

This section explains how to process CSV, TSV, and Excel files in ProTIGY. These file formats require additional setup steps compared to GCT files, including label assignment, identifier column selection, and experimental design upload.

## Important: File Upload Requirements

**All files must be uploaded at the same time from the same directory.** ProTIGY requires all files to be selected together in a single upload session. You cannot upload files separately or from different directories.

## Workflow Overview

After assigning labels (see General Help), the CSV/TSV/Excel processing workflow consists of three main steps:

1. **Select Identifier Columns** - Choose which column contains unique feature identifiers
2. **Experimental Design** - Upload sample metadata and experimental design
3. **Process Files** - Convert files to GCT format for analysis

## Step 1: Select Identifier Columns

For each dataset, select the column that contains unique feature identifiers (e.g., protein IDs, gene symbols, peptide sequences).

**What to look for:**
- Column with unique identifiers for each row
- Common examples: `id`, `protein_id`, `gene_symbol`, `peptide_sequence`
- Avoid columns with sample data or metadata

**Important:** This column will become the row identifier (`rid`) in the final GCT object.

## Step 2: Experimental Design

The experimental design file contains metadata about your samples and experimental conditions.

### Option 1: Download Template

Click "Download Template" to get a CSV template with:
- **columnName**: Sample IDs (column names from your data files)
- **Experiment**: Experimental condition (e.g., "Control", "Treatment")
- **Group**: Sample group (e.g., "Group1", "Group2")

#### Fill Out Template

1. **columnName**: Contains sample IDs that match column names in your data files
2. **Experiment**: Fill in experimental conditions for each sample
3. **Group**: Assign samples to groups for statistical analysis

**Note**: If a column should be treated as metadata (not used for analysis), leave the entire row blank for that column.

### Option 2: Upload Experimental Design

Upload your completed experimental design file (CSV, TSV, or Excel format).

**Requirements:**
- Must be CSV, TSV, or Excel format
- First column must be named "columnName"
- Sample IDs must match column names in your data files
- All samples must have corresponding entries

## Step 3: Process Files

After uploading your experimental design file, click "Process Files" to:
- Convert CSV/TSV/Excel files to GCT format
- Store experimental design as sample metadata
- Separate sample data from row metadata

After processing, you will move on to normalization and filtering.

## Troubleshooting

### Common Issues

**"Column not found in experimental design"**
- Ensure sample IDs in experimental design exactly match column names in data files
- Check for extra spaces or special characters

**"Failed to process file"**
- Verify file format is supported (CSV, TSV, Excel)
- Check that file is not corrupted
- Ensure first row contains headers

**"No samples found"**
- Verify experimental design file has correct format
- Check that "columnName" is the first column header
- Ensure at least one sample has valid metadata

