# Recommendation: Remove data.table Dependency

## Summary

**REMOVE data.table** and use the tidyverse-only optimization approach. This achieves **3.3-3.5x speedup** (vs 4.8x with data.table) while avoiding a new dependency.

## Current Status

The vignette currently uses:
- ✅ data.table for direct string parsing (lines 107-144)
- ✅ Vectorized diagnosis filtering (lines 214-218)

## Recommended Changes

### Remove data.table completely

Replace the `parse_events_to_long()` function with the original tidyverse approach using `separate_wider_delim()` and `pivot_longer()`, BUT keep the vectorized diagnosis filtering.

## What to Change

### In `vignettes/nssp_nowcast.Rmd`

**REMOVE** (lines 106-145):
```r
parse_events_to_long <- function(line_list, event_col_name, id_col_name) {
  library(data.table)  # <-- REMOVE THIS DEPENDENCY
  dt <- as.data.table(line_list)
  # ... data.table code ...
}
```

**REPLACE WITH** the original tidyverse functions:
```r
expand_events <- function(line_list, event_col_name) {
  wide_line_list <- separate_wider_delim(line_list,
    {{ event_col_name }},
    delim = "{", names_sep = "", too_few = "align_start"
  )
  return(wide_line_list)
}

wide_to_long <- function(wide_line_list,
                         event_col_name,
                         values_to,
                         names_to,
                         id_col_name) {
  long_data <- wide_line_list |>
    pivot_longer(
      cols = starts_with({{ event_col_name }}),
      names_to = {{ names_to }},
      values_to = {{ values_to }},
      values_drop_na = FALSE
    ) |>
    mutate(
      event_id = paste(
        .data[[id_col_name]],
        as.numeric(str_extract(as.character(.data[[names_to]]), "[0-9.]+"))
      )
    )
  return(long_data)
}
```

**KEEP** the vectorized diagnosis filtering (lines 214-218):
```r
# This is the KEY optimization - keep this!
diagnosis_pattern <- paste(diagnoses_codes_defn, collapse = "|")
bar_updates <- nssp_updates |>
  filter(str_detect(diagnoses_codes, diagnosis_pattern))
```

### Update Performance Note (line 65-66)

**CHANGE FROM:**
```markdown
<strong>Performance Note:</strong> The preprocessing code in this vignette has been optimized for performance on large datasets. We avoid creating wide intermediate data structures by parsing event strings directly into long format using `data.table`, and use vectorized string matching for diagnosis codes. These optimizations can provide 5-50x speedups on large line-list datasets compared to naive approaches.
```

**TO:**
```markdown
<strong>Performance Note:</strong> The preprocessing code in this vignette has been optimized for performance on large datasets. The key optimization is using vectorized string matching for diagnosis codes (combining all patterns into a single regex and using `str_detect()`), which provides 77-96x speedup over row-by-row iteration. This optimization achieves 3.3-3.5x overall speedup on realistic datasets without requiring additional dependencies.
```

### Update Performance Optimizations Box (lines 265-272)

**CHANGE FROM:**
```markdown
1. **Direct String Parsing:** Instead of first expanding event strings into many columns (wide format) and then pivoting back to long format, we parse strings directly into long format using `data.table`. This avoids creating large intermediate data structures and provides 5-10x speedup on large datasets.

2. **Vectorized Diagnosis Matching:** Rather than iterating row-by-row with `map_lgl()` and checking each diagnosis code pattern separately, we combine all diagnosis codes into a single regex pattern and use vectorized `str_detect()`. This provides 10-50x speedup for the matching step on large datasets.
```

**TO:**
```markdown
1. **Vectorized Diagnosis Matching:** Rather than iterating row-by-row with `map_lgl()` and checking each diagnosis code pattern separately, we combine all diagnosis codes into a single regex pattern and use vectorized `str_detect()`. This provides 77-96x speedup for the matching step and 3.3-3.5x overall speedup on realistic datasets.

The wide pivot approach (using `separate_wider_delim()` and `pivot_longer()`) is retained as it provides good performance while maintaining consistency with tidyverse syntax and avoiding additional dependencies.
```

### In `DESCRIPTION` file

**REMOVE** from `Suggests:` field (if it was added):
```
data.table,
```

## Justification

### Performance Trade-off is Acceptable

| Dataset | Rows  | Tidyverse-only | data.table | Difference |
|---------|-------|----------------|------------|------------|
| Medium  | 500   | 0.081s         | 0.057s     | 0.024s     |
| Large   | 2500  | 2.309s         | 1.578s     | 0.731s     |

**The tidyverse-only approach is only 0.7 seconds slower on 2500 rows.** This is a very acceptable trade-off to avoid a new dependency.

### Key Insight

The isolated benchmarks showed that **diagnosis vectorization alone provides 77-96x speedup** on the filtering step. This is where all the performance gain comes from! The data.table direct parsing adds only ~40% additional speedup, which is nice but not essential.

### Benefits of Removing data.table

1. ✅ **No new dependency** - keeps package lightweight
2. ✅ **Consistent with existing code** - everything uses tidyverse
3. ✅ **Easier to maintain** - one paradigm instead of mixing dplyr + data.table
4. ✅ **Still fast** - 3.3-3.5x speedup is excellent
5. ✅ **More readable** - tidyverse syntax is familiar to most R users

## Implementation Steps

1. Revert the `parse_events_to_long()` function to use `separate_wider_delim()` + `pivot_longer()`
2. Keep the vectorized diagnosis filtering (`paste()` + `str_detect()`)
3. Update performance notes to reflect actual measured speedups
4. Remove data.table from dependencies (if added)
5. Update benchmarking scripts to show tidyverse-only as the recommended approach

## Expected Outcome

- ✅ 3.3-3.5x speedup on realistic datasets (vs 1.0x baseline)
- ✅ No new dependencies required
- ✅ Preprocessing time on 2500 rows: 2.3s (acceptable for most workflows)
- ✅ Consistent tidyverse codebase

## When to Reconsider

Only add data.table back if:
- Users regularly process datasets > 10,000 rows
- The additional 1.4x speedup becomes critical
- User feedback indicates the current performance is insufficient
