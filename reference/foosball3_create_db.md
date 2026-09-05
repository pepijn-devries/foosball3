# Create a New Foosball Database

Creates an SQLite file at the specified location. It will contain all
required relationships and constraints and some non-trivial data.

## Usage

``` r
foosball3_create_db(file, ...)
```

## Arguments

- file:

  File path where the database needs to be stored

- ...:

  Ignored

## Value

Returns `NULL` invisibly.

## Examples

``` r
fl <- tempfile(fileext = ".sqlite")
foosball3_create_db(fl)

## Clean up:
unlink(fl, TRUE, TRUE)
```
