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

```

Get a list of cbsa codes of top 100 metros

``` r
get_code_cbsa100()

  [1] "13820" "46060" "38060" "30780" "32820" "41860" "41940" "40900" "41740" "31080" "40140" "23420" "12540" "37100" "44700"
 [16] "19740" "17820" "35300" "25540" "49340" "14860" "37980" "35840" "27260" "19660" "33100" "45300" "37340" "36740" "29460"
 [31] "15980" "47900" "12060" "12260" "46520" "14260" "41180" "16980" "26900" "17140" "31140" "19780" "36540" "28140" "48620"
 [46] "35380" "12940" "14460" "39300" "44140" "12580" "24340" "19820" "33460" "27140" "29820" "35620" "10900" "10740" "10580"
 [61] "45060" "40380" "15380" "39100" "20500" "16740" "47260" "49180" "24660" "39580" "49660" "17460" "18140" "19430" "10420"
 [76] "45780" "36420" "46140" "38900" "25420" "38300" "42540" "17900" "16700" "24860" "34980" "28940" "19100" "41700" "12420"
 [91] "26420" "21340" "32580" "41620" "36260" "39340" "40060" "42660" "33340" "31540"

```

