# Check if All Suggested Packages Are Available

This function returns `TRUE` if all packages required for the graphical
user interface (GUI)
([`foosball3()`](https://pepijn-devries.github.io/foosball3/reference/foosball3.md))
are available.

## Usage

``` r
foosball3_suggests_ok(ignore = c("testthat", "mirai"), ...)
```

## Arguments

- ignore:

  A vector of packages to ignore. By default `"mirai"` is ignored, as
  this package is not needed for basic functionality. It is recommended
  to install for better performance. `"testthat"` is also ignored by
  default. It is only used during package development and is not
  required for the GUI at all.

- ...:

  Arguments passed to
  [`foosball3_list_suggests()`](https://pepijn-devries.github.io/foosball3/reference/foosball3_list_suggests.md)

## Value

A `logical` value, indicating of all required suggests are available.

## Examples

``` r
foosball3_suggests_ok()
#> [1] TRUE
```
