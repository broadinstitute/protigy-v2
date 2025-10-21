# Contrast Selection UI - Testing & Usage Guide

## Overview

The contrast selection UI has been completely redesigned to handle large numbers of groups and contrasts efficiently. The new hybrid approach provides:

1. **Quick Select Buttons** for common selection patterns
2. **Matrix View** for visual, interactive selection (recommended for >10 groups)
3. **List View** with search for traditional dropdown experience (recommended for ≤10 groups)
4. **Selected Contrasts Summary** for at-a-glance overview and individual removal

## Files Modified

### New Files
- `R/tab_stat_setup_contrast_helpers.R` - Helper functions for contrast generation and UI rendering

### Modified Files
- `R/tab_stat_setup.R` - Replaced pickerInput with hybrid selection UI (~280 lines modified/added)
- `inst/custom.css` - Added CSS styling for matrix view and UI components (~230 lines added)
- `inst/help_documentation/protigy_statistics_help.md` - Updated user documentation

## Testing Instructions

### Prerequisites

1. Reload the package:
```r
devtools::load_all(".")
```

2. Launch the app:
```r
Protigy::launchApp()
```

### Test Scenarios

#### **Test 1: Small Dataset (≤10 groups)**

**Expected Behavior:**
- Default to "List View"
- All quick-select buttons should work
- Dropdown should have search functionality

**Steps:**
1. Upload test data with 5 groups
2. Navigate to Statistics > Test Setup
3. Select "Two-sample Moderated T-test"
4. Verify "List View" is default
5. Click "All Pairwise" button → Should select all 20 contrasts (5×4)
6. Click "All vs Control" button → Should detect control group and select appropriate contrasts
7. Click "Sequential Pairs" → Should select 10 contrasts (A/B, B/A, B/C, C/B, etc.)
8. Click "Clear All" → Should deselect everything
9. Manually select contrasts from dropdown using search
10. Verify selected contrasts appear in summary panel
11. Click × on a contrast badge → Should remove that contrast

#### **Test 2: Medium Dataset (10-20 groups)**

**Expected Behavior:**
- Default to "Matrix View"
- Matrix should be scrollable if needed
- Row/column select buttons should work

**Steps:**
1. Upload test data with 15 groups
2. Navigate to Statistics > Test Setup
3. Select "Two-sample Moderated T-test"
4. Verify "Matrix View" is default
5. Verify matrix displays correctly with:
   - 15 rows (numerators)
   - 15 columns (denominators)
   - Diagonal cells disabled (marked with —)
6. Click various matrix cells → Should toggle selection (green checkmark when selected)
7. Click "Select Row" button on a row → Should select all contrasts in that row
8. Click "Select Col" button on a column → Should select all contrasts in that column
9. Toggle between "Matrix View" and "List View" → Selections should persist
10. Verify selected contrasts summary shows correct count (e.g., "42 of 420")

#### **Test 3: Large Dataset (>20 groups)**

**Expected Behavior:**
- Matrix should handle many cells efficiently
- Scrolling should be smooth
- Virtual scrolling enabled in list view

**Steps:**
1. Upload test data with 30 groups
2. Navigate to Statistics > Test Setup
3. Select "Two-sample Moderated T-test"
4. Verify matrix displays with:
   - Proper scrolling (both horizontal and vertical)
   - Sticky headers (row and column headers stay visible while scrolling)
5. Test performance:
   - Click multiple cells quickly → Should respond without lag
   - Use "All Pairwise" button → Should select all 870 contrasts (30×29) quickly
6. Switch to "List View":
   - Search should work efficiently
   - Virtual scrolling should activate (only renders visible options)
7. Verify selected contrasts panel:
   - Should show count (e.g., "50 of 870")
   - Should be scrollable
   - Should allow individual removal

#### **Test 4: Edge Cases**

**Test 4a: Only 2 Groups**
- Should work normally with just 2 contrasts (A/B and B/A)
- Both views should function correctly

**Test 4b: Group Names with Special Characters**
- Test with groups like "Group-1", "Control_v2", "Treatment (high)"
- Verify contrasts display correctly
- Verify clicking works in matrix view

**Test 4c: Very Long Group Names**
- Test with groups with long names (>20 characters)
- Verify matrix cells remain usable
- Verify truncation or wrapping works appropriately

**Test 4d: Switching Between Datasets**
- Select contrasts for dataset 1
- Switch to dataset 2 (different groups)
- Verify contrast selection resets appropriately
- Switch back to dataset 1
- Verify selections are remembered

#### **Test 5: Quick Select Patterns**

**Test 5a: Control Group Detection**
1. Test with group named "Control" → Should be detected
2. Test with group named "ctrl" → Should be detected (case-insensitive)
3. Test with group named "WT" → Should be detected
4. Test with no obvious control → Should default to first alphabetically

**Test 5b: Sequential Pairs**
1. Groups: A, B, C, D, E
2. Click "Sequential Pairs"
3. Verify selects: A/B, B/A, B/C, C/B, C/D, D/C, D/E, E/D (8 contrasts)

#### **Test 6: Run Statistical Test**

**Verify downstream compatibility:**
1. Select contrasts using any method
2. Click "Run Test" button
3. Verify test runs successfully
4. Navigate to Statistics > Summary
5. Verify results are available for all selected contrasts
6. Verify no contrasts appear for unselected comparisons

#### **Test 7: Apply to All Datasets**

**Verify multi-dataset handling:**
1. Load multiple datasets with same annotation groups
2. Select contrasts for first dataset
3. Check "Apply to all datasets"
4. Verify same contrasts applied to all datasets
5. Run tests
6. Verify results for all datasets

### Visual Inspection

Check for visual issues:
- ✓ Matrix cells are aligned properly
- ✓ Hover effects work (blue highlight)
- ✓ Selected cells are green with checkmarks
- ✓ Disabled cells (diagonal) are grayed out
- ✓ Buttons have appropriate styling
- ✓ Summary panel is readable and styled correctly
- ✓ No UI overflow or clipping
- ✓ Responsive on different screen sizes (if testing in browser)

### Performance Benchmarks

Expected performance (rough guidelines):
- **5 groups (20 contrasts)**: Instant response (<100ms)
- **10 groups (90 contrasts)**: Very fast (<200ms)
- **20 groups (380 contrasts)**: Fast (<500ms)
- **30 groups (870 contrasts)**: Moderate (500ms-1s)
- **50 groups (2,450 contrasts)**: Acceptable (1-3s for "All Pairwise")

## Known Limitations

1. **Very Large Datasets (>50 groups)**:
   - Matrix with 2,500+ cells may be slow to render initially
   - Recommend using "All Pairwise" button instead of individual cell clicks
   - List view with search may be more practical

2. **Browser Compatibility**:
   - Tested primarily in Chrome/Edge
   - Older browsers may not support all CSS features (sticky headers)

3. **Mobile/Tablet**:
   - Matrix view may be difficult to use on small screens
   - Recommend list view for mobile devices

## Troubleshooting

### Issue: Matrix doesn't render
**Solution:** Check browser console for JavaScript errors. Ensure `inst/custom.css` is loaded.

### Issue: Clicks don't register in matrix
**Solution:** Verify namespace (`ns()`) is correctly applied. Check that Shiny.setInputValue calls are formatted correctly.

### Issue: Selected contrasts don't persist when switching views
**Solution:** Verify `stat_param()` is being updated correctly. Check reactive dependencies.

### Issue: "All vs Control" doesn't work as expected
**Solution:** Check that control group detection is working. May need to adjust keywords in `detect_control_group()` function.

### Issue: Performance is slow with many groups
**Solution:**
1. Use quick-select buttons instead of individual clicks
2. Consider reducing number of groups if possible
3. Use list view with search for specific contrasts

## Comparison: Old vs New Approach

| Feature | Old (pickerInput) | New (Hybrid) |
|---------|------------------|--------------|
| **Max groups handled well** | ~10 | 50+ |
| **Selection speed (20 groups)** | Slow (scroll & click) | Fast (matrix/buttons) |
| **Visual feedback** | Checkboxes in dropdown | Color-coded matrix |
| **Search capability** | Basic | Enhanced with live search |
| **Quick selection patterns** | Manual | One-click buttons |
| **UI space** | Compact | More prominent |
| **Learning curve** | Familiar | Brief learning needed |

## Next Steps After Testing

If tests pass:
1. Update version number in DESCRIPTION
2. Create pull request with detailed description
3. Update CHANGELOG or NEWS file
4. Consider creating a demo video or screenshot for documentation
5. Gather user feedback for further improvements

## Future Enhancements (Optional)

Consider for future versions:
1. **Save/Load Contrast Sets**: Allow users to save common contrast patterns
2. **Contrast Grouping**: Group contrasts by categories (e.g., "Treatment vs Control", "Time Points")
3. **Matrix Filtering**: Add ability to filter matrix by group name
4. **Color-Coded Groups**: Use colors to distinguish group categories in matrix
5. **Keyboard Shortcuts**: Add keyboard navigation for power users
6. **Export Contrasts**: Allow exporting selected contrasts list
7. **Undo/Redo**: Add undo/redo functionality for selections
8. **Preview Results**: Show preview of which results will be generated

## Questions or Issues?

If you encounter any issues during testing:
1. Check R console for error messages
2. Check browser console (F12) for JavaScript errors
3. Try clearing browser cache and reloading
4. Document the issue with:
   - Number of groups
   - Steps to reproduce
   - Expected vs actual behavior
   - Any error messages
