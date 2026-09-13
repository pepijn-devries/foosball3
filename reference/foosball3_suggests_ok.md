# Check if All Suggested Packages Are Available

This function returns `TRUE` if all packages required for the graphical
user interface (GUI)
([`foosball3()`](https://pepijn-devries.github.io/foosball3/reference/foosball3.md))
are available.

## Usage

``` r
foosball3_suggests_ok(ignore = "mirai", ...)
```

## Arguments

- ignore:

  A vector of packages to ignore. By default `"mirai"` is ignored, as
  this package is not needed for basic functionality. It is recommended
  to install for better performance.

- ...:

  Arguments passed to
  [`foosball3_list_suggests()`](https://pepijn-devries.github.io/foosball3/reference/foosball3_list_suggests.md)

## Value

A `logical` value, indicating of all required suggests are available.

## Examples

``` r
foosball3_suggests_ok()
#> Loading required namespace: base64enc, bsicons, bslib, DiagrammeR, DiagrammeRsvg, dm, DT
#> Loading required namespace: ggplot2, ggiraph, imager, jsonlite, knitr, lubridate, mirai
#> Loading required namespace: promises, shinybusy, shinyjs, shinyvalidate, shinyWidgets
#> [1] FALSE FALSE FALSE
```
