# Generate Matches for a Tournament Phase

Generates matches for a specific phase in a foosball tournament for a
given set of participants.

## Usage

``` r
foosball3_generate_matches(
  participants,
  tournament_type = "individual",
  tournament_phase = "qualification",
  options,
  progress,
  ...
)
```

## Arguments

- participants:

  A `data.frame` with two columns: first `PARTICIPANT_ID`, a unique
  integer identifier for each participant. Second a column named
  `QUALIFICATION_RATE`, it holds numeric values greater than zero. A
  qualification rate of 0.5 indicates an average player. Values less
  than 0.5 are for less experienced players. Values greater than 0.5
  indicate more experienced players.

- tournament_type:

  Type of tournament for which to render matches. At the moment only
  `"individual"` is supported. This is a tournament where player play in
  variable team compositions, but for individual results.

- tournament_phase:

  The phase of the tournament. At the moment only `qualification` is
  supported. These are matches preceding a semi finals, where
  individuals can collect points individually to qualify for the semi
  finals.

- options:

  Options used for generating the matches. It can be used to tweak the
  effort for balancing matches for player experience.

- progress:

  A callback function for reporting progress. It needs to be a function
  that accepts 1 numeric argument. It is called by the generator, with a
  number between 0 and 1 indicating the progress of the process.

- ...:

  Ignored

## Value

It returns a `data.frame` with three columns. Firstly, `MATCH_ID` which
is a unique integer identifier for each match. Secondly,
`PARTICIPANT_ID`, which is from the input `participants`, and indicates
the participants enrolled in the indicated match. Thirdly,
`POSITION_CODE`, indicating where the participant is positioned for each
specific match: `S1` = striker side 1, `D1` = defender side 1, `S2` =
striker side 2 and finally `D2` = defender side 2.

## Examples

``` r
participants <-
  dplyr::tibble(
    PARTICIPANT_ID = 5L:9L,
    QUALIFICATION_RATE = c(0.3, 0.5, 1, 1.3, 1.5)
  )
opts <- list(
  nsim = 100,
  revolutions = 2,
  weights = list(
    teamup         = 1,
    opposing       = 1,
    match_var      = 1,
    match_bal      = 1,
    match_bal_var  = 1,
    match_bal_extr = 1,
    part_bal       = 1,
    part_bal_var   = 1,
    part_bal_extr  = 1
  ),
  seed = 0
)

foosball3_generate_matches(
  participants, options = opts)
#>    MATCH_ID PARTICIPANT_ID POSITION_CODE
#> 1         1              7            S1
#> 2         2              6            S1
#> 3         3              9            S1
#> 4         4              7            S1
#> 5         5              5            S1
#> 6         6              8            S1
#> 7         7              9            S1
#> 8         8              6            S1
#> 9         9              8            S1
#> 10       10              5            S1
#> 11        1              8            D1
#> 12        2              7            D1
#> 13        3              5            D1
#> 14        4              5            D1
#> 15        5              8            D1
#> 16        6              7            D1
#> 17        7              6            D1
#> 18        8              9            D1
#> 19        9              6            D1
#> 20       10              9            D1
#> 21        1              5            D2
#> 22        2              5            D2
#> 23        3              6            D2
#> 24        4              8            D2
#> 25        5              6            D2
#> 26        6              9            D2
#> 27        7              8            D2
#> 28        8              7            D2
#> 29        9              9            D2
#> 30       10              7            D2
#> 31        1              9            S2
#> 32        2              9            S2
#> 33        3              8            S2
#> 34        4              6            S2
#> 35        5              7            S2
#> 36        6              5            S2
#> 37        7              7            S2
#> 38        8              8            S2
#> 39        9              5            S2
#> 40       10              6            S2
```
