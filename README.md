
# foosball3 <img src="man/figures/logo.svg" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/pepijn-devries/foosball3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pepijn-devries/foosball3/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Create a database to manage foosball tournaments, facilitate live
matches and visualise match results and player statistics.

## Installation

Install latest developmental version from R-Universe:

``` r
install.packages("foosball3", repos = c('https://pepijn-devries.r-universe.dev', 'https://cloud.r-project.org'))
```

If you want to use the graphical user interface, make sure to install
all suggested packages as well.

``` r
library(foosball3)

if (!foosball3_suggests_ok()) {
  foosball3_install_suggests()
}
```

## Example

To start the graphical user interface just call:

``` r
foosball3()
```

## Code of Conduct

Please note that the foosball3 project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
