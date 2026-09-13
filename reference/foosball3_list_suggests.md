# List Packages Suggested by the `foosball3` Package

These are all packages required for the graphical user interface
([`foosball3()`](https://pepijn-devries.github.io/foosball3/reference/foosball3.md)).
Make sure that they are installed

## Usage

``` r
foosball3_list_suggests(...)
```

## Arguments

- ...:

  Ignored

## Value

Returns a vector of package names that are suggested by `foosball3` and
required by its graphical user interface.

## Examples

``` r
foosball3_list_suggests()
#>  [1] "base64enc"     "bsicons"       "bslib"         "DiagrammeR"   
#>  [5] "DiagrammeRsvg" "dm"            "DT"            "ggplot2"      
#>  [9] "ggiraph"       "imager"        "jsonlite"      "knitr"        
#> [13] "lubridate"     "mirai"         "promises"      "shinybusy"    
#> [17] "shinyjs"       "shinyvalidate" "shinyWidgets" 
```
