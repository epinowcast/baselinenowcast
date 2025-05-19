# Baselinenowcast User Interface Design

## Current State

The `baselinenowcast` package provides low-level modular functions for nowcasting epidemiological data with reporting delays. The current workflow requires users to:

1. Manually convert their data to reporting triangle matrices
2. Call multiple functions sequentially to estimate delays and generate nowcasts
3. Handle all input/output conversions between data.frames and matrices
4. Execute multi-step workflows to properly account for uncertainty

## Proposed User Interface Design

### High-Level Interface Function: `baselinenowcast()`

Create a single user-facing function that serves as the main entry point with smart data format detection:

```r
baselinenowcast(
  data,                      # Will detect input type using S3 methods
  n_history_delay = NULL,    # Observations for delay estimation
  n_history_uncertainty = NULL, # Observations for uncertainty
  strata = NULL,             # Column names for stratification
  strata_sharing = NULL,     # Character: "delay", "uncertainty", both, or empty
  nowcast_date = NULL,       # Date to make nowcast
  include_draws = TRUE,      # Whether to include uncertainty
  draws = 100,               # Number of draws if include_draws=TRUE
  verbose = FALSE,           # Whether to provide step-by-step output
  ...                        # Passed to estimate_uncertainty()
)
```

This function would:
1. Accept various input formats and detect them automatically via S3 methods. Main entry point is the new `reporting_triangle` class.
2. Perform all needed conversions between formats
3. Execute the complete nowcasting workflow
    1. Convert input to reporting triangle with metadata 
    2. Split into strata if needed
    3. Apply strata sharing if specified i.e. perform all steps on the pooled data and use the same estimates for each stratum in the following step.
    4. Process each stratum
    5. Combine results if needed
    6. Format results based on return_format
4. Automatically convert results back to an enhanced data.frame object with a custom class
5. Return results in the user's preferred format

#### Internal Steps in baselinenowcast():

```r
# Pseudocode for baselinenowcast() function internals
function baselinenowcast(data, ...) {
  # 1. Convert input to reporting triangle with metadata 
  rep_triangle <- as_reporting_triangle(data, ...)
  
  # 2. Store reference dates for later conversion back
  ref_dates <- attr(rep_triangle, "reference_dates")
  
  # 3. Split into strata if needed
  if (has_strata(rep_triangle)) {
    tri_list <- split_reporting_triangle(rep_triangle)
  } else {
    tri_list <- list(rep_triangle)
  }
  
  # 4. Apply strata sharing if specified
  shared_delay <- NULL
  shared_uncertainty <- NULL
  
  if (!is.null(strata_sharing) && length(strata_sharing) > 0) {
    # Pool data for estimation once
    pooled_triangle <- combine_triangles(tri_list)
    
    # Estimate delay if requested
    if ("delay" %in% strata_sharing) {
      # Estimate delay once on pooled data
      shared_delay <- estimate_delay(pooled_triangle, n = n_history_delay)
    }
    
    # Estimate uncertainty if requested
    if ("uncertainty" %in% strata_sharing) {
      # Estimate uncertainty once on pooled data
      shared_uncertainty <- estimate_uncertainty(pooled_triangle, n = n_history_uncertainty)
    }
  }
  
  # 5. Process each stratum
  results_list <- lapply(tri_list, function(tri) {
    # 5a. Estimate and apply delay
    # Use shared delay if available, otherwise estimate locally
    if (!is.null(shared_delay) && "delay" %in% strata_sharing) {
      delay_pmf <- shared_delay
    } else {
      delay_pmf <- estimate_delay(tri, n = n_history_delay)
    }
    point_nowcast <- apply_delay(tri, delay_pmf = delay_pmf)
    
    # 5b. If uncertainty requested, estimate and apply
    if (include_draws) {
      # Use shared uncertainty if available, otherwise estimate locally
      if (!is.null(shared_uncertainty) && "uncertainty" %in% strata_sharing) {
        uncertainty_params <- shared_uncertainty
      } else {
        uncertainty_params <- estimate_uncertainty(tri, n = n_history_uncertainty)
      }
      
      nowcast_draws <- apply_uncertainty(
        point_nowcast, 
        tri, 
        uncertainty_params = uncertainty_params,
        draws = draws,
        ...  # Passes error model params
      )
      return(nowcast_draws)
    } else {
      return(point_nowcast)
    }
  })
  
  # 6. Combine results if needed
  if (length(results_list) > 1) {
    results <- combine_results(results_list)
  } else {
    results <- results_list[[1]]
  }
  
  # 7. Create and return baselinenowcast class
  nowcast_result <- structure(
    results,
    class = c("baselinenowcast_nowcast", "reporting_triangle", class(results)),
    original_triangle = rep_triangle,
    reference_dates = ref_dates,
    strata = if (has_strata(rep_triangle)) attr(rep_triangle, "strata") else NULL,
    nowcast_date = nowcast_date
  )
  
  return(nowcast_result)
}
```

### Intermediate-Level Grouped Functions

Create intermediate-level "grouped" function pairs that combine common steps:

```r
# Combined function for estimating and applying delays
estimate_and_apply_delay(
  reporting_triangle,
  n = NULL,  # Number of observations to use
  ...
)

# Combined function for estimating uncertainty and generating draws
estimate_and_apply_uncertainty(
  point_nowcast_matrix,
  reporting_triangle,
  n_history = NULL,
  draws = 100,
  error_model = NULL,   # Custom error model function
  aggregator = NULL,    # Function to aggregate predictions for error estimation
  ...
)
```

### Type Conversion Functions

Add utility functions for data format conversion:

```r
# Convert dataframe to reporting triangle (S3 generic)
as_reporting_triangle <- function(data, ...) UseMethod("as_reporting_triangle")

# S3 method for data.frame
as_reporting_triangle.data.frame <- function(
  data,
  reference_date,
  report_date,
  count,
  strata = NULL,
  max_delay = 30,  # Now here instead of main function
  ...
) {
  # Creates a reporting triangle with attributes:
  # - reference_dates: vector of dates matching triangle rows
  # - strata: strata values if applicable
  # - max_delay: maximum delay value used
}

# S3 method for epinowcast data
as_reporting_triangle.enw_data <- function(
  data,
  max_delay = 30,
  ...
) { ... }

# Convert reporting triangle/nowcast to dataframe with dates
as_data_frame <- function(
  triangle_or_nowcast,
  reference_dates = NULL,  # Vector of dates corresponding to triangle rows
  strata = NULL
) {
  # If reference_dates is NULL, try to get from attributes
  if (is.null(reference_dates)) {
    reference_dates <- attr(triangle_or_nowcast, "reference_dates")
    if (is.null(reference_dates)) {
      stop("Reference dates must be provided or available in attributes")
    }
  }
  
  # Map matrix rows back to dates and return data.frame
}
```

### S3 Methods for Uncertainty Estimation

Implement S3 methods for flexible uncertainty estimation:

```r
# S3 generic
estimate_uncertainty <- function(data, ...) UseMethod("estimate_uncertainty")

# For a single reporting triangle
estimate_uncertainty.reporting_triangle <- function(
  data, 
  n_history = NULL,
  error_model = NULL,
  aggregatior = NULL,
  ...
) { ... }

# For a list of reporting triangles (multiple strata)
estimate_uncertainty.list <- function(
  data,
  n_history = NULL,
  error_model = NULL,
  aggregator = NULL,
  ...
) { ... }

# For a point nowcast with reporting triangle
estimate_uncertainty.baselinenowcast_pointnowcast <- function(
  data,
  reporting_triangle,
  ...
) { ... }
```

### Strata Handling

Add utilities for handling stratified data:

```r
# Split a stratified reporting triangle into a list of triangles
split_reporting_triangle <- function(
  stratified_triangle,
  strata_names = NULL
) { ... }

# Combine a list of triangles into a stratified triangle
combine_reporting_triangles <- function(
  triangle_list,
  strata_names
) { ... }
```

### S3 Methods for Main Interface

Implement S3 methods for handling different input types:

```r
# S3 methods for baselinenowcast main function
baselinenowcast.reporting_triangle <- function(data, ...) { ... }

# S3 methods for output objects
print.baselinenowcast <- function(x, ...) { ... }
plot.baselinenowcast <- function(x, ...) { ... }
summary.baselinenowcast <- function(x, ...) { ... }
```

## Implementation Plan

1. **Phase 1: Intermediate Grouped Functions**
   - Implement `estimate_and_apply_delay()` function
   - Implement `estimate_uncertainty()` S3 generic and methods
   - Implement `estimate_and_apply_uncertainty()` function
   - Ensure compatibility with existing lower-level functions

2. **Phase 2: Data Conversion Layer**
   - Implement `as_reporting_triangle()` S3 generic and methods
   - Implement `as_data_frame()` utility function with date mapping
   - Add validation and helper functions for different input formats
   - Ensure validation logic matches current package (e.g., `.check_na_bottom_right()`)

3. **Phase 3: Strata Handling**
   - Implement `split_reporting_triangle()` and `combine_reporting_triangles()` functions
   - Define how strata will be stored in reporting triangle objects
   - Implement strata borrowing for delay and uncertainty estimation

4. **Phase 4: Main Interface Function**
   - Implement S3 methods for different input types
   - Create the main `baselinenowcast()` function with verbose mode
   - Add comprehensive dispatch logic for different data formats
   - Implement automatic date mapping for data.frame output

5. **Phase 5: Output and Visualization Methods**
   - Add S3 methods for printing, summarizing, and plotting
   - Ensure consistent output formats
   - Add documentation and examples
   - Maintain compatibility with plotting approaches shown in vignettes

## Design Considerations

1. **Backward Compatibility**
   - Maintain all existing low-level functions
   - Ensure the new interface simply builds upon current functionality
   - Document equivalence between old and new interfaces

2. **Flexibility**
   - Allow users to customize any part of the workflow
   - Provide sensible defaults with minimal required arguments
   - Support both simple and complex use cases
   - Preserve mathematical sequence of operations

3. **Performance**
   - Consider efficient implementations for large datasets
   - Allow options for parallel processing where applicable
   - Maintain memory efficiency of matrix operations

4. **Integration**
   - Ensure smooth interoperability with epinowcast
   - Support common epidemiological data formats
   - Preserve existing preprocessing workflows

5. **Strata Complexity Mitigation**
   - Initially implement strata handling at the top level interface only
   - Keep the core low-level functions working with single-stratum triangles
   - Use the split/combine functions to manage complexity between layers

6. **Workflow Preservation**
   - Add verbose mode to explain steps for educational purposes
   - Maintain the mathematical sequence of operations
   - Support retrospective nowcasting use cases
   - Handle complex validation requirements

## Alternative Approaches

1. **Package Function** instead of S3 methods
   - Less object-oriented, more focused on functional programming
   - Simpler implementation but potentially less flexibility

2. **Pipe-friendly** design with magrittr/tidyverse compatibility
   - Functions return consistent objects that can be piped
   - More aligned with modern R data analysis workflows
   - This is the low level interface we have.

3. **Configuration object** approach
   - Users create a configuration object that defines the nowcast
   - Pass this to execution functions that perform the actual computation