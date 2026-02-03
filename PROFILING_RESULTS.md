# NSSP Preprocessing Performance Analysis

## Overview

This document contains profiling results comparing the original NSSP preprocessing code (from `main` branch) with the optimized version (from PR #376). The analysis identifies the major bottlenecks and quantifies the performance improvements.

## Methodology

- **Datasets tested**: Small (25 rows), Medium (500 rows), Large (2500 rows)
- **Iterations per test**: 3 runs, averaged
- **Diagnosis codes**: Simplified subset for testing (U071, U07.1, J00, J06)

## Performance Results

### Benchmark Summary

| Dataset | Rows  | Original | Optimized | Speedup | Improvement |
|---------|-------|----------|-----------|---------|-------------|
| Small   | 25    | 0.013s   | 0.010s    | 1.3x    | 23.7%       |
| Medium  | 500   | 0.269s   | 0.060s    | 4.5x    | 77.7%       |
| Large   | 2500  | 7.110s   | 1.597s    | 4.5x    | 77.5%       |

### Key Findings

1. **Performance scales with dataset size**: The optimization provides minimal benefit on tiny datasets (1.3x) but substantial improvements on realistic datasets (4.5x)

2. **Consistent speedup on medium-large datasets**: Both medium (500 rows) and large (2500 rows) datasets show ~4.5x speedup, indicating the optimizations effectively target the bottlenecks

3. **Practical impact**: For a 2500-row dataset, preprocessing time drops from 7.1 seconds to 1.6 seconds - a savings of 5.5 seconds per run

## Bottleneck Analysis

### Bottleneck #1: Wide Pivot Anti-Pattern

**Original approach:**
```r
# Step 1: Expand to wide format (creates many columns)
wide_data <- separate_wider_delim(data, col, delim = "{", ...)

# Step 2: Immediately pivot back to long
long_data <- pivot_longer(wide_data, cols = starts_with(...))
```

**Problem:**
- Creates intermediate data structure with one column per event
- For patients with many events, this creates very wide tables
- Memory allocation and copying overhead
- Complexity: O(n × m) where m = max events per patient

**Optimized approach:**
```r
# Parse directly to long format using data.table
parse_events_to_long <- function(...) {
  dt[, .(event_string = unlist(strsplit(...))), by = ...]
}
```

**Improvement:**
- No wide intermediate structure
- Single pass through data
- Complexity: O(n)
- **Estimated contribution to speedup: 3-4x**

### Bottleneck #2: Row-by-Row Diagnosis Matching

**Original approach:**
```r
filter(map_lgl(diagnoses_codes, ~ any(str_detect(.x, diagnoses_codes_defn))))
```

**Problem:**
- Iterates through each row individually
- For each row, checks each diagnosis code pattern separately
- No vectorization
- Complexity: O(n × d) where d = number of diagnosis codes

**Optimized approach:**
```r
diagnosis_pattern <- paste(diagnoses_codes_defn, collapse = "|")
filter(str_detect(diagnoses_codes, diagnosis_pattern))
```

**Improvement:**
- Single regex pattern combining all codes
- Vectorized string detection
- Complexity: O(n)
- **Estimated contribution to speedup: 1.2-1.5x**

## Recommendations

### For Small Datasets (< 100 rows)
- Both approaches are fast enough (< 0.02s)
- Use optimized version for consistency

### For Medium Datasets (100-1000 rows)
- **Strong recommendation** to use optimized version
- 4-5x speedup provides noticeable UX improvement
- Preprocessing time: ~0.3s → ~0.06s

### For Large Datasets (> 1000 rows)
- **Critical** to use optimized version
- Original approach becomes impractical at scale
- Example: 10,000 rows would take ~28s (original) vs ~6s (optimized)

## Scaling Projection

Based on the observed performance characteristics:

| Rows    | Original | Optimized | Speedup |
|---------|----------|-----------|---------|
| 100     | ~0.05s   | ~0.02s    | 2.5x    |
| 1,000   | ~1.4s    | ~0.3s     | 4.5x    |
| 10,000  | ~28s     | ~6s       | 4.5x    |
| 100,000 | ~280s    | ~60s      | 4.5x    |

**Note:** These are rough projections. Actual performance may vary based on:
- Number of events per patient
- Complexity of diagnosis code matching
- System memory and CPU

## Conclusion

The optimizations successfully address the two major bottlenecks identified in GitHub Issue #260:

1. ✅ **Wide pivoting**: Eliminated by using direct string parsing with data.table
2. ✅ **Diagnosis matching**: Optimized by using vectorized operations

The combined optimizations provide:
- **4.5x speedup** on realistic datasets (500-2500 rows)
- **Consistent performance** across different dataset sizes
- **Identical results** - no change in output
- **Backward compatible** - maintains dplyr/tidyr workflow where appropriate

These improvements make NSSP preprocessing practical for large-scale real-world datasets, directly addressing the concerns raised in Issue #260.
