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
county_cbsa_st
```


Get cbsa code, name, populaiton and employment for top 100 metros in the US:

``` r
metro.data::get_metro100()
```

Get cbsa size by population:

``` r
metro.data::get_metrosize()
```
