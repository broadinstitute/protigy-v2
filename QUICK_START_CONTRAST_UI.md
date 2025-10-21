# Quick Start Guide: New Contrast Selection UI

## Immediate Next Steps

### Step 1: Reload the Package (1 minute)

Open R/RStudio and run:

```r
devtools::load_all(".")
```

If you see any errors about missing functions or dependencies, run:

```r
devtools::document()  # Update NAMESPACE
devtools::load_all(".")
```

### Step 2: Launch the App (30 seconds)

```r
Protigy::launchApp()
```

### Step 3: Quick Test (5 minutes)

#### Test with Small Dataset (5 groups)

1. **Load Data**:
   ```r
   # Using included test data
   data(brca_retrospective_v5.0_proteome_gct)
   ```
   OR upload your own GCT/CSV file with 5 groups

2. **Navigate to Statistics**:
   - Go to sidebar: Setup → Data upload (if needed)
   - Go to menu: Statistics → Test Setup

3. **Select Test Type**:
   - Select dataset
   - Choose "Two-sample Moderated T-test"

4. **Try New Features**:
   - ✓ See the quick select buttons at the top
   - ✓ Click "All Pairwise" → Should see all contrasts selected
   - ✓ Notice the "Selected Contrasts" panel at bottom showing count
   - ✓ Try "List View" toggle → Should see familiar dropdown with search
   - ✓ Try "Matrix View" toggle → Should see grid (if you have enough groups)
   - ✓ Click × on a contrast badge → Should remove that contrast

5. **Run Test**:
   - Click "Run Test" button
   - Navigate to Statistics → Summary
   - Verify results appear for selected contrasts

#### Test with Many Groups (optional, 10 minutes)

If you have a dataset with 10+ groups:

1. Load data with 10-20 groups
2. Navigate to Statistics → Test Setup
3. Select "Two-sample Moderated T-test"
4. **Notice**:
   - Default is now "Matrix View"
   - Grid shows all groups as rows and columns
   - Diagonal cells are disabled (grayed out with —)
5. **Try Matrix**:
   - Click individual cells → Should toggle green with checkmark
   - Click "Select Row" button → Selects entire row
   - Click "Select Col" button → Selects entire column
6. **Try Quick Buttons**:
   - "All vs Control" → Should auto-detect control and select those contrasts
   - Notice the notification showing which group was detected as control
   - "Sequential Pairs" → Should select A/B, B/C, C/D pattern

---

## Visual Guide: What You Should See

### Quick Select Buttons
```
Quick Select:  [All Pairwise] [All vs Control] [Sequential Pairs] [Clear All]
```

### View Toggle
```
○ Matrix View  ○ List View
```

### Matrix View (Example with 4 groups: A, B, C, D)
```
          A      B      C      D
      ┌──────┬──────┬──────┬──────┐
  A   │  —   │  ✓   │      │  ✓   │
      ├──────┼──────┼──────┼──────┤
  B   │      │  —   │  ✓   │      │
      ├──────┼──────┼──────┼──────┤
  C   │  ✓   │      │  —   │      │
      ├──────┼──────┼──────┼──────┤
  D   │      │  ✓   │  ✓   │  —   │
      └──────┴──────┴──────┴──────┘
```
- `—` = Disabled (diagonal)
- `✓` = Selected (green cell)
- Empty = Not selected (white cell)

### Selected Contrasts Panel
```
Selected Contrasts (5 of 12)
┌────────────────────────────────────────────────┐
│ [A / B ×] [A / D ×] [B / C ×] [C / A ×] [D / B ×] │
└────────────────────────────────────────────────┘
```

---

## Troubleshooting

### Issue: "Error: could not find function 'generate_all_pairwise'"

**Cause**: Helper functions not loaded

**Fix**:
```r
devtools::document()
devtools::load_all(".")
```

### Issue: "Error: could not find function 'render_contrast_matrix'"

**Same fix as above**

### Issue: Matrix doesn't appear / looks broken

**Fix**:
1. Hard refresh browser: `Ctrl + F5` (Windows) or `Cmd + Shift + R` (Mac)
2. Clear browser cache
3. Restart the Shiny app

### Issue: Clicks on matrix cells don't work

**Check**:
1. Is JavaScript enabled in your browser?
2. Any errors in browser console? (Press F12 to open developer tools)
3. Is the Shiny connection active? (Look for gray "disconnected" message)

### Issue: Quick select buttons don't do anything

**Check**:
1. Are you in "Two-sample Moderated T-test" mode?
2. Do you have at least 2 groups selected?
3. Check R console for error messages

---

## Expected Behavior Summary

| Action | Expected Result |
|--------|----------------|
| Click "All Pairwise" | Select all n×(n-1) contrasts |
| Click "All vs Control" | Auto-detect control, select all vs control, show notification |
| Click "Sequential Pairs" | Select A/B, B/A, B/C, C/B, etc. |
| Click "Clear All" | Deselect everything, summary shows "0 of X" |
| Click matrix cell (white) | Cell turns green with ✓ |
| Click matrix cell (green ✓) | Cell turns white (deselected) |
| Click matrix cell (gray —) | Nothing (diagonal disabled) |
| Click "Select Row" | All cells in that row turn green |
| Click "Select Col" | All cells in that column turn green |
| Toggle Matrix ↔ List | Selections persist, view changes |
| Click × on badge | That contrast is removed, UI updates |
| Search in List View | Contrasts filter as you type |

---

## What to Report

If you find any issues, please note:

1. **What you did**: Specific steps to reproduce
2. **What happened**: Actual behavior
3. **What you expected**: Expected behavior
4. **Environment**:
   - R version
   - Browser (Chrome, Firefox, Edge, etc.)
   - Number of groups in your dataset
5. **Error messages**: From R console or browser console (F12)
6. **Screenshots**: Helpful for UI issues

---

## Performance Expectations

| Groups | Contrasts | Expected Performance |
|--------|-----------|---------------------|
| 2-5    | 2-20      | Instant (<100ms) |
| 10     | 90        | Very fast (<200ms) |
| 20     | 380       | Fast (<500ms) |
| 30     | 870       | Moderate (~1s) |
| 50     | 2,450     | Acceptable (2-3s) |

If performance is significantly worse than this, please report it.

---

## Success Criteria

✅ The implementation is working if:

1. **UI appears correctly**:
   - Quick select buttons visible
   - View toggle buttons visible
   - Matrix or list displays based on number of groups
   - Selected contrasts summary panel visible at bottom

2. **Interactions work**:
   - Clicking buttons changes selections
   - Matrix cells toggle when clicked
   - List view dropdown allows multi-select
   - Badges in summary can be removed

3. **Functional correctness**:
   - "Run Test" button works
   - Results appear in Summary tab
   - Only selected contrasts generate results

4. **Performance acceptable**:
   - No noticeable lag with <10 groups
   - Acceptable lag with 10-30 groups (<1s)
   - Usable with 30-50 groups (<3s)

---

## Next Steps After Testing

### If Everything Works ✅

1. **Use it in your analysis**:
   - Try with your real datasets
   - Explore different selection patterns
   - Compare performance to old version (if you remember)

2. **Provide Feedback**:
   - What works well?
   - What's confusing?
   - What could be improved?
   - Any bugs or edge cases?

3. **Share with colleagues** (optional):
   - See if others find it useful
   - Gather diverse perspectives

### If You Find Issues ⚠️

1. **Document the issue**:
   - Follow "What to Report" section above
   - Take screenshots if applicable

2. **Try workarounds**:
   - Switch views (Matrix ↔ List)
   - Use different selection method
   - Reduce number of groups if possible

3. **Report it**:
   - Open GitHub issue
   - Or email/message with details

---

## Additional Documentation

For more details, see:

- **[CONTRAST_SELECTION_IMPLEMENTATION_SUMMARY.md](./CONTRAST_SELECTION_IMPLEMENTATION_SUMMARY.md)** - Technical overview
- **[CONTRAST_SELECTION_TESTING.md](./CONTRAST_SELECTION_TESTING.md)** - Comprehensive testing guide
- **[inst/help_documentation/protigy_statistics_help.md](./inst/help_documentation/protigy_statistics_help.md)** - User documentation

---

## Getting Help

If you're stuck:
1. Check this guide's Troubleshooting section
2. Check the comprehensive testing guide
3. Review error messages carefully
4. Open browser console (F12) to check for JavaScript errors
5. Reach out with detailed description of issue

---

**Ready? Let's test!** 🚀

```r
devtools::load_all(".")
Protigy::launchApp()
```
