# Import Foosball Tournament Data from a File

If you have an older file with tournament results, you can try to import
those results with this function. It will create a new copy of the
database (using
[`foosball3_create_db()`](https://pepijn-devries.github.io/foosball3/reference/foosball3_create_db.md)),
making sure it complies with the latest database scheme specification
used by this package.

## Usage

``` r
foosball3_import_db(file, target, ...)
```

## Arguments

- file:

  TODO

- target:

  Target file path where the clean database will be stored. Existing
  files at this location may be overwritten.

- ...:

  Ignored

## Value

Creates a new clean copy of the database. Returns `NULL` invisibly.

## Examples

``` r
# TODO
```
