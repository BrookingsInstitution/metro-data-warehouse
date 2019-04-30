# metro.data

<!-- badges: start -->
<!-- badges: end -->

The goal of metro.data is to streamline data analysis workflow at Brookings Metro

## Installation

You can install the version in development of metro.data from Github with: 

``` r
devtools::install_github("BrookingsInstitution/metro.data")
```

## Example

Get relation file of county, cbsa, and state
``` r
head(county_cbsa_st)
<<<<<<< HEAD

# A tibble: 6 x 16
  code.county name.county population.coun~ employment.coun~ pct.urban.county type.county code.state name.state population.state
  <chr>       <chr>                  <dbl>            <dbl>            <dbl> <chr>       <chr>      <chr>                 <dbl>
1 01099       Monroe Cou~            21745             5400            21.0  nonmetro c~ 01         Alabama             4850771
2 01079       Lawrence C~            33288             3138             8.71 metro coun~ 01         Alabama             4850771
3 01081       Lee County~           156597            43945            72.6  metro coun~ 01         Alabama             4850771
4 01093       Marion Cou~            30058             7656            11.2  nonmetro c~ 01         Alabama             4850771
5 01107       Pickens Co~            20170             2639             0    metro coun~ 01         Alabama             4850771
6 01119       Sumter Cou~            13084             2991             0    nonmetro c~ 01         Alabama             4850771
# ... with 7 more variables: employment.state <dbl>, code.cbsa <chr>, name.cbsa <chr>, type.cbsa <chr>, population.cbsa <dbl>,
#   employment.cbsa <dbl>, size.cbsa <chr>
=======
>>>>>>> 8efa7fd8151f8d7fe1af7b1da7cfa99a5bc09a53

```
# A tibble: 6 x 16
  code.county name.county population.coun~ employment.coun~ pct.urban.county type.county code.state name.state population.state
  <chr>       <chr>                  <dbl>            <dbl>            <dbl> <chr>       <chr>      <chr>                 <dbl>
1 01099       Monroe Cou~            21745             5400            21.0  nonmetro c~ 01         Alabama             4850771
2 01079       Lawrence C~            33288             3138             8.71 metro coun~ 01         Alabama             4850771
3 01081       Lee County~           156597            43945            72.6  metro coun~ 01         Alabama             4850771
4 01093       Marion Cou~            30058             7656            11.2  nonmetro c~ 01         Alabama             4850771
5 01107       Pickens Co~            20170             2639             0    metro coun~ 01         Alabama             4850771
6 01119       Sumter Cou~            13084             2991             0    nonmetro c~ 01         Alabama             4850771
# ... with 7 more variables: employment.state <dbl>, code.cbsa <chr>, name.cbsa <chr>, type.cbsa <chr>, population.cbsa <dbl>,
#   employment.cbsa <dbl>, size.cbsa <chr>
