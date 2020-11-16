Itereation and List Columns
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ------ tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6, 
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis", 
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Lists

You can put anything in a list

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)

l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.559752 -0.785620  0.047387  0.009793  0.638725  2.851332

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]][1:3]
```

    ## [1] 5 6 7

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## For loop

Create new list

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 0.2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 2.491866 2.616209 2.331649 2.894339 3.354413 3.714179 2.012146 2.069296
    ##  [9] 2.081179 2.148978 4.046697 3.433529 3.335130 1.244767 1.825261 1.495434
    ## [17] 2.071393 3.454537 3.714574 3.099900
    ## 
    ## $b
    ##  [1]   0.47491567   8.57618354   4.73951489  -4.06982223   4.96154898
    ##  [6]   1.27216720   5.36106014  -4.13923781  -4.35091160 -10.62202559
    ## [11]  -0.45053942   3.37386253   1.14586487  -2.70861756   5.22024399
    ## [16]   1.45467879   5.99632036 -10.90077886   6.72263764  -0.07222105
    ## [21]   2.39884965   8.58634346   4.67196319   1.83434965   5.79937206
    ## [26]   5.96368877   8.95834565  -7.13768065  -6.49770535   1.36735795
    ## 
    ## $c
    ##  [1]  9.881010  9.827460  9.980488 10.287684  9.921258 10.050808  9.939117
    ##  [8]  9.907030 10.207455  9.705918  9.854713  9.971890  9.973285  9.737969
    ## [15]  9.912325  9.896851 10.269806  9.980010  9.816140  9.481001 10.062831
    ## [22] 10.298513 10.012237 10.007079  9.637654 10.276952  9.937296 10.173950
    ## [29] 10.083561  9.848128  9.893931  9.670258  9.923145 10.471206 10.096058
    ## [36] 10.147395 10.263080  9.829832  9.718126 10.151613
    ## 
    ## $d
    ##  [1] -0.7174344 -2.5843738 -3.6503639 -0.9439383 -2.4312469 -3.1968774
    ##  [7] -3.2402244 -2.2150923 -1.9945391 -2.4835088 -2.2311160 -1.7894781
    ## [13] -2.8308938 -3.2283947 -3.0975981 -2.6875968 -4.7215874 -4.1742049
    ## [19] -1.9997313 -3.5667360

``` r
list_norm[[1]]
```

    ##  [1] 2.491866 2.616209 2.331649 2.894339 3.354413 3.714179 2.012146 2.069296
    ##  [9] 2.081179 2.148978 4.046697 3.433529 3.335130 1.244767 1.825261 1.495434
    ## [17] 2.071393 3.454537 3.714574 3.099900

Pause and get our old function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

I can apply this to each list element

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.67 0.810

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.26  5.47

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.208

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.69 0.982

Let’s use a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

## Let’s try map

``` r
output = map(list_norm, mean_and_sd)
```

what if you want different function

``` r
output = map(list_norm, median)
```

Map variants

``` r
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## list columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    sample = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(sample)
```

    ## $a
    ##  [1] 2.491866 2.616209 2.331649 2.894339 3.354413 3.714179 2.012146 2.069296
    ##  [9] 2.081179 2.148978 4.046697 3.433529 3.335130 1.244767 1.825261 1.495434
    ## [17] 2.071393 3.454537 3.714574 3.099900
    ## 
    ## $b
    ##  [1]   0.47491567   8.57618354   4.73951489  -4.06982223   4.96154898
    ##  [6]   1.27216720   5.36106014  -4.13923781  -4.35091160 -10.62202559
    ## [11]  -0.45053942   3.37386253   1.14586487  -2.70861756   5.22024399
    ## [16]   1.45467879   5.99632036 -10.90077886   6.72263764  -0.07222105
    ## [21]   2.39884965   8.58634346   4.67196319   1.83434965   5.79937206
    ## [26]   5.96368877   8.95834565  -7.13768065  -6.49770535   1.36735795
    ## 
    ## $c
    ##  [1]  9.881010  9.827460  9.980488 10.287684  9.921258 10.050808  9.939117
    ##  [8]  9.907030 10.207455  9.705918  9.854713  9.971890  9.973285  9.737969
    ## [15]  9.912325  9.896851 10.269806  9.980010  9.816140  9.481001 10.062831
    ## [22] 10.298513 10.012237 10.007079  9.637654 10.276952  9.937296 10.173950
    ## [29] 10.083561  9.848128  9.893931  9.670258  9.923145 10.471206 10.096058
    ## [36] 10.147395 10.263080  9.829832  9.718126 10.151613
    ## 
    ## $d
    ##  [1] -0.7174344 -2.5843738 -3.6503639 -0.9439383 -2.4312469 -3.1968774
    ##  [7] -3.2402244 -2.2150923 -1.9945391 -2.4835088 -2.2311160 -1.7894781
    ## [13] -2.8308938 -3.2283947 -3.0975981 -2.6875968 -4.7215874 -4.1742049
    ## [19] -1.9997313 -3.5667360

``` r
listcol_df %>% filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  sample      
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations

``` r
mean_and_sd(listcol_df$sample[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.67 0.810

``` r
mean_and_sd(listcol_df$sample[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.26  5.47

``` r
mean_and_sd(listcol_df$sample[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.208

Can I just map?

``` r
map(listcol_df$sample, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.67 0.810
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.26  5.47
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.208
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.69 0.982

Can I add a list column? Saving output in this dataframe

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(sample, mean_and_sd), 
    medians = map(sample, median)
    )
```

## Weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\hbrig\AppData\Local\Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2020-10-05 18:45:50 (7.537)

    ## file min/max dates: 1869-01-01 / 2020-10-31

    ## using cached file: C:\Users\hbrig\AppData\Local\Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2020-10-05 18:46:09 (1.703)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: C:\Users\hbrig\AppData\Local\Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2020-10-05 18:46:18 (0.882)

    ## file min/max dates: 1999-09-01 / 2020-10-31

Get our list columns

``` r
weather_nest = 
  weather_df %>% 
  nest(data = date:tmin)
```

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ... with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # ... with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # ... with 355 more rows

``` r
weather_nest$data[[1]]
```

    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ... with 355 more rows

Suppose I want to regress ‘tmax’ on ‘tmin’ for each station

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

Want a function to do this regression

``` r
weather_lm = function(df){
  
  lm(tmax ~ tmin, data = df)
  
}

weather_lm(weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

as for loop

``` r
output = vector("list", 3)

for (i in 1:3) {
  
  output[[i]] = weather_nest$data[[i]]
  
}
```

Use a map statement

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

What about a map in a list column

``` r
weather_nest = 
  weather_nest %>% 
  mutate(models = map(data, weather_lm))

weather_nest$models
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221
