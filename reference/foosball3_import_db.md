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

  An SQLite file or a zipped collection of csv files (created with
  [`foosball3_export_db()`](https://pepijn-devries.github.io/foosball3/reference/foosball3_export_db.md)),
  to be imported into the standardised database structure.

- target:

  Target file path where the clean database will be stored. Existing
  files at this location may be overwritten.

- ...:

  Ignored

## Value

Creates a new clean copy of the database. Returns `NULL` invisibly.

## Examples

``` r
target   <- tempfile(fileext = ".db")
zip_file <- system.file(package = "foosball3", "example.zip")
foosball3_import_db(zip_file, target)

# Cleanup
unlink(target)
```
