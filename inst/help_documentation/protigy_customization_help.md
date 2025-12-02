# Color Customization

ProTIGY allows you to fully customize the color schemes used for annotation columns throughout all visualizations. This enables you to maintain consistent color-coding across plots and match your publication or presentation style.

## Accessing the Customize Tab

The **Customize** tab is located in the main navigation bar, directly after the Help tab. You can access it once your data files have been processed and setup is complete.

**Note**: The color customization interface will only appear after your GCT files have been processed. If you haven't completed setup yet, you'll see a message prompting you to process your data first.

## Color Definition Modes

### Multi-ome (Unified) Mode

- **Purpose**: Maintain consistent colors across all datasets (omes) for the same annotation values
- **Use Case**: When you want the same condition (e.g., "Tumor", "NAT") to have the same color in proteome, phosphoproteome, and RNA-seq plots
- **Behavior**: Changing a color in this mode automatically syncs that color across all individual omes that have the same annotation column and value

**Example**: If you change "Tumor" to red in multi-ome mode, it will be red in all proteome, phosphoproteome, and RNA-seq visualizations.

### Per-ome (Individual) Mode

- **Purpose**: Customize colors independently for each dataset
- **Use Case**: When different omes have different experimental conditions or you want different color schemes for each
- **Behavior**: Colors are only updated for the selected ome and do not affect other omes

**Example**: You can have "Tumor" be red in the proteome but blue in the phosphoproteome.

## Selecting Annotation Columns

The color customization interface includes a dropdown menu to select which annotation column you want to customize:

- **Default Selection**: The analysis annotation column (selected during setup) is shown by default
- **All Columns Available**: You can customize colors for any discrete annotation column in your data
- **Continuous Columns**: Continuous annotation columns (e.g., age, expression values) are not available for color customization as they use gradient color scales

## Customizing Colors

1. **Select an Annotation Column**: Use the dropdown to choose which annotation column to customize
2. **Choose Color Mode**: Select "Multi-ome (Unified)" or "Per-ome (Individual)" based on your needs
3. **Select Ome** (if using Per-ome mode): Choose which dataset to customize
4. **Change Colors**: Click on any color picker to change the color for that condition value
5. **Automatic Updates**: Colors are saved immediately as you change them, and you'll see a notification confirming each change

## Importing Color Schemes

You can import color schemes from YAML files:

1. **Click "Browse..."** in the Import Color Scheme section
2. **Select a YAML file** containing your color scheme
3. **Colors are applied automatically** with smart matching:
   - Conditions that match by name get their colors from the YAML
   - Unmatched conditions keep their original colors or get unused colors from the YAML sequentially
   - Missing annotation columns keep their original colors

**Supported YAML Formats:**
- **ProTIGY Format**: Uses `colors:` with ome-level nesting (exported by ProTIGY)
- **PANOPLY Format**: Uses `groups.colors:` with flat structure (exported by PANOPLY, applies to all omes)

**Note**: Imported color schemes become the new defaults for the "Restore Default Colors" button.

## Exporting Color Schemes

You can export your color scheme in two ways:

### Quick Export from Customize Tab

1. **Click "Export Current Scheme"** in the Customize tab
2. **Save the YAML file** to your desired location
3. **Reuse later** by importing it back into ProTIGY

### Automatic Export from Export Tab

The color scheme is also **automatically included** in all exports from the **Export** tab:

1. **Navigate to the Export tab**
2. **Select your desired datasets and tabs**
3. **Click "Download"** to get a ZIP file
4. **Find your color scheme** in the `customization/color_scheme.yaml` file within the exported ZIP

The exported color scheme file includes:
- All annotation columns with discrete values
- Color assignments for all condition values
- Metadata (creation date, ProTIGY version)

**Note**: If you forget to export from the Customize tab, your color scheme will still be saved automatically when you export your analysis results.

## Restoring Defaults

### Restore Default Colors

- **Purpose**: Restore to the current default color scheme
- **Behavior**: 
  - If you've imported a YAML file, this restores to those imported colors
  - If no YAML was imported, this restores to the original app-generated colorblind-safe palette

### Reset to App Defaults

- **Purpose**: Clear any imported YAML defaults and return to the original app-generated colors
- **Behavior**: 
  - Clears the imported YAML file from the upload box
  - Regenerates the original colorblind-safe color palette
  - Removes imported defaults so "Restore Default Colors" will use app defaults again

## Troubleshooting

**Colors not updating in plots?**
- Make sure you've clicked on the color picker and selected a new color
- Check that you're viewing the correct annotation column in your plots
- Try refreshing the plot or navigating away and back to the tab

**Can't see the Customize tab?**
- Ensure your data files have been processed and setup is complete
- The tab is located directly after the Help tab in the navigation bar

**Imported colors not matching?**
- Check that condition value names match exactly (case-sensitive)
- Verify your YAML file structure is correct
- Use "Reset to App Defaults" if you want to start over

**Missing annotation columns?**
- Only discrete annotation columns can be customized
- Continuous columns (with many unique values) use gradient scales and cannot be customized
- Make sure the annotation column exists in your data's sample metadata

