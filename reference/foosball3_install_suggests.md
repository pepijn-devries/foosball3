# Install Packages Suggested by `foosball3`

Call this function if
[`foosball3_suggests_ok()`](https://pepijn-devries.github.io/foosball3/reference/foosball3_suggests_ok.md)
returns `FALSE`. This will install all suggested packages required to
run the graphical user interface
([`foosball3()`](https://pepijn-devries.github.io/foosball3/reference/foosball3.md)).

## Usage

``` r
foosball3_install_suggests(ignore = "mirai", update = TRUE, ...)
```

## Arguments

- ignore:

  A vector of packages to ignore. By default `"mirai"` is ignored, as
  this package is not needed for basic functionality. It is recommended
  to install for better performance.

- update:

  A `logical` value. If `TRUE`, it will try to update suggested packages
  that are already installed. If `FALSE`, it will only install suggested
  packages that are not yet available.

- ...:

  Arguments passed to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)

## Value

invisible `NULL`

## Examples

``` r
foosball3_list_suggests()
#> [1] "base64enc, bsicons, bslib, DiagrammeRsvg, dm, DT, ggplot2"    
#> [2] "ggiraph, imager, jsonlite, knitr, lubridate, mirai, shinybusy"
#> [3] "shinyjs, shinyvalidate, shinyWidgets"                         
```
