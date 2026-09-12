# Export an Foosball SQLite Database to csv Format

Export a database to a collection of csv-files. Note that you will lose
all views, relational information and constraints from the database.

## Usage

``` r
foosball3_export_db(source, destination, validate = FALSE, ...)
```

## Arguments

- source:

  File path, where the source SQLite database can be found

- destination:

  File path, where the zipped collection of csv-files should be stored

- validate:

  A `logical` value. If `TRUE`, the `source` will be imported to check
  for validity (slower). If `FALSE`, `source` will be used directly
  without checks.

- ...:

  Ignored.

## Value

`NULL` invisible

## Examples

``` r
new_db <- tempfile()
foosball3_create_db(new_db)
new_export <- tempfile(fileext = ".zip")
foosball3_export_db(new_db, new_export)

## Clean up the files created in this example
unlink(new_db, TRUE, TRUE)
unlink(new_export, TRUE, TRUE)
```
