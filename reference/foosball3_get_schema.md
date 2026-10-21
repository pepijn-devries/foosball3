# Get foosball3 Schema Data

Returns information about the relationships between tables in the
database used by `foosball3`

## Usage

``` r
foosball3_get_schema(con, include_views = FALSE, ...)
```

## Arguments

- con:

  A connection to a copy of a foosball3 database. Can be created with
  [`foosball3_create_db()`](https://pepijn-devries.github.io/foosball3/reference/foosball3_create_db.md)
  or
  [`foosball3_import_db()`](https://pepijn-devries.github.io/foosball3/reference/foosball3_import_db.md).

- include_views:

  If `TRUE` it will include 'view' tables, otherwise it will exclude
  them. Default is `FALSE`

- ...:

  Ignored.

## Value

A `dm` class object with relational information.

## Examples

``` r
if (requireNamespace("dm")) {
  db_file <- tempfile(fileext = ".db")
  foosball3_create_db(db_file)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
  on.exit({
    RSQLite::dbDisconnect(con)
    unlink(db_file, TRUE, TRUE)
  }, add = TRUE)
  
  foosball3_get_schema(con, TRUE)
}
#> Loading required namespace: dm
#> ── Table source ────────────────────────────────────────────────────────────────
#> Error: Invalid or closed connection
```
