# Install Packages Suggested by `foosball3`

Call this function if
[`foosball3_suggests_ok()`](https://pepijn-devries.github.io/foosball3/reference/foosball3_suggests_ok.md)
returns `FALSE`. This will install all suggested packages required to
run the graphical user interface
([`foosball3()`](https://pepijn-devries.github.io/foosball3/reference/foosball3.md)).

## Usage

``` r
foosball3_install_suggests(ignore = c("testthat", "mirai"), update = TRUE, ...)
```

## Arguments

- ignore:

  A vector of packages to ignore. By default `"mirai"` is ignored, as
  this package is not needed for basic functionality. It is recommended
  to install for better performance. `"testthat"` is also ignored by
  default. It is only used during package development and is not
  required for the GUI at all.

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
foosball3_suggests_ok()
#> [1] TRUE
```
