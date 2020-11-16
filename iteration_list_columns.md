Itereation and List Columns
================

``` r
library(tidyverse)
```

    ## -- Attaching packages -----------------

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------------------------
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
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.08544 -0.54663  0.03467  0.05497  0.67382  2.31941

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
    ##  [1] 0.03599031 3.45647622 2.87954447 3.45569141 1.99064795 2.99888549
    ##  [7] 2.05561240 2.40830820 1.57860016 2.84205188 3.13245950 3.03678721
    ## [13] 3.32374796 2.65948295 1.91342852 3.43788213 2.03603250 4.36880727
    ## [19] 2.43616412 3.63147567
    ## 
    ## $b
    ##  [1]  0.98429608  3.79653887  0.96216452  7.02980299 -0.91714641 -3.30802620
    ##  [7] -0.37547574 -0.76708521 -2.72412977 -1.25636526 -4.37606598 -0.47352584
    ## [13] -0.24106165 -4.06797750 -1.31084158 -7.68561375  8.42135197  2.28709077
    ## [19] -7.15468433 -6.26539241 -4.28908704  2.64037669  2.15036608 -3.31204840
    ## [25] -2.60095725  0.48476221 -5.88805034  3.33896982 -6.68296706 -0.07783292
    ## 
    ## $c
    ##  [1] 10.029856  9.850348 10.273366 10.004322 10.171898 10.218455 10.035938
    ##  [8]  9.947973 10.003567 10.141475  9.993935  9.771484  9.817040 10.041524
    ## [15] 10.046227  9.842157  9.598291  9.999525  9.822675 10.094702 10.188557
    ## [22]  9.665567 10.569972 10.073932 10.155767  9.940968 10.058997 10.049723
    ## [29] 10.221605 10.153994 10.077505  9.761872  9.874843  9.859768  9.899593
    ## [36]  9.905742  9.946678  9.718497  9.683587  9.699382
    ## 
    ## $d
    ##  [1] -3.4050825 -3.8713036 -1.4943147 -3.4058699 -4.7287923 -3.9036973
    ##  [7] -3.8286464 -1.5983998 -2.2405834 -3.7798282 -1.6730109 -2.5788034
    ## [13] -3.0940886 -3.7605264 -2.5446742 -1.9152754 -1.4963641 -4.4333833
    ## [19] -2.8746578 -0.8584134

``` r
list_norm[[1]]
```

    ##  [1] 0.03599031 3.45647622 2.87954447 3.45569141 1.99064795 2.99888549
    ##  [7] 2.05561240 2.40830820 1.57860016 2.84205188 3.13245950 3.03678721
    ## [13] 3.32374796 2.65948295 1.91342852 3.43788213 2.03603250 4.36880727
    ## [19] 2.43616412 3.63147567

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
    ## 1  2.68 0.936

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.06  3.96

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.194

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.87  1.11

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
