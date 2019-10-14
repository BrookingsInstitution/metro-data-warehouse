# metro.data

<!-- badges: start -->
<!-- badges: end -->

The goal of metro.data is to streamline data analysis workflow at Brookings Metro

## Installation

You can install the version in development of metro.data from Github with: 

``` r
devtools::install_github("BrookingsInstitution/metro-data-warehouse")
library(metro.data)
```

## Datasets

### Get relation file of county, cbsa, and state
``` r
head(county_cbsa_st)


# A tibble: 6 x 15
  stco_code co_name co_pop co_emp co_pcturban co_type st_fips st_name st_emp cbsa_code cbsa_name
  <chr>     <chr>    <dbl>  <dbl>       <dbl> <fct>   <chr>   <chr>    <dbl> <chr>     <chr>    
1 01099     Monroe~  21745   5400       21.0  rural ~ 01      Alabama 1.61e6 NA        NA       
2 01079     Lawren~  33288   3138        8.71 small ~ 01      Alabama 1.61e6 19460     Decatur,~
3 01081     Lee Co~ 156597  43945       72.6  small ~ 01      Alabama 1.61e6 12220     Auburn-O~
4 01093     Marion~  30058   7656       11.2  rural ~ 01      Alabama 1.61e6 NA        NA       
5 01107     Picken~  20170   2639        0    small ~ 01      Alabama 1.61e6 46220     Tuscaloo~
6 01119     Sumter~  13084   2991        0    rural ~ 01      Alabama 1.61e6 NA        NA       
# ... with 4 more variables: cbsa_type <fct>, cbsa_pop <dbl>, cbsa_emp <dbl>, cbsa_size <fct>
```
### Get place to county crosswalk

``` r
head(place2county)
  stpl_fips stco_fips st_fips st_ab             pl_name      co_name  pl_pop16 afact1 afact2
1   0100100     01017      01    AL      Abanda CDP, AL  Chambers AL   189.912      1  0.006
2   0100124     01067      01    AL  Abbeville city, AL     Henry AL  2666.561      1  0.155
3   0100460     01073      01    AL Adamsville city, AL Jefferson AL  4529.245      1  0.007
4   0100484     01133      01    AL    Addison town, AL   Winston AL   736.979      1  0.031
5   0100676     01065      01    AL      Akron town, AL      Hale AL   337.748      1  0.023
6   0100820     01117      01    AL  Alabaster city, AL    Shelby AL 32769.300      1  0.156

``` r

## Functions
### Get cbsa code of top 100 metros

``` r
cbsa100_code

  [1] "13820" "46060" "38060" "30780" "32820" "41860" "41940" "40900" "41740" "31080" "40140" "23420" "12540"
 [14] "37100" "44700" "19740" "17820" "35300" "25540" "49340" "14860" "37980" "35840" "27260" "19660" "33100"
 [27] "45300" "37340" "36740" "29460" "15980" "47900" "12060" "12260" "46520" "14260" "41180" "16980" "26900"
 [40] "17140" "31140" "19780" "36540" "28140" "48620" "35380" "12940" "14460" "39300" "44140" "12580" "24340"
 [53] "19820" "33460" "27140" "29820" "35620" "10900" "10740" "10580" "45060" "40380" "15380" "39100" "20500"
 [66] "16740" "47260" "49180" "24660" "39580" "49660" "17460" "18140" "19430" "10420" "45780" "36420" "46140"
 [79] "38900" "25420" "38300" "42540" "17900" "16700" "24860" "34980" "28940" "19100" "41700" "12420" "26420"
 [92] "21340" "32580" "41620" "36260" "39340" "40060" "42660" "33340" "31540"

```
### Create a plot using Brookings Metro theme
 
```r
bbplot(iris, aes(x = iris$Sepal.Length, y = iris$Sepal.Width))+
  geom_point()
```

### Create a metadata file here using an R Shiny app: https://sifan.shinyapps.io/create_metadata/


