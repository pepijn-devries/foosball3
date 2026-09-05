# Get Meta Data for Tables in the Foosball Database Scheme

Returns a list of tables and fields in the Foosball database. In
addition, it contains a description of the table and each field.

## Usage

``` r
foosball3_meta_data(...)
```

## Arguments

- ...:

  Ignored.

## Value

Returns a `data.frame` with description information

## Examples

``` r
foosball3_meta_data()
#> # A tibble: 132 × 3
#>    table        field_name          description                                 
#>    <chr>        <chr>               <chr>                                       
#>  1 pictures     CREATE              Optional pictures of the tournament. Ideal …
#>  2 pictures     PICTURE_ID          Unique identifier for each picture.         
#>  3 pictures     TOURNAMENT_ID       The tournament where the picture was taken. 
#>  4 pictures     PICTURE_DESCRIPTION An optional description of the picture.     
#>  5 pictures     PICTURE_WIDTH       Pixel width of the picture.                 
#>  6 pictures     PICTURE_HEIGHT      Pixel height of the picture.                
#>  7 pictures     JPG_DATA            Image binary data in JPG format. It should …
#>  8 picture_tags CREATE              Table containing coordinates that tag faces…
#>  9 picture_tags TAG_ID              Unique identifier for each tag.             
#> 10 picture_tags PICTURE_ID          Picture that is being tagged.               
#> # ℹ 122 more rows
```
