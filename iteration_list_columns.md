Itereation and List Columns
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ---- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------- tidyverse_conflicts() --
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
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.1142 -0.5765  0.1601  0.1146  0.8059  2.6422

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
    ##  [1] 2.5249955 4.0450324 3.5479659 3.3622448 5.1573232 3.9882118 3.7225453
    ##  [8] 2.7010365 4.8201765 2.6990133 3.3683754 3.6339455 2.3918356 0.4601406
    ## [15] 5.0633208 2.4836491 4.3403378 1.9576506 4.2879522 2.9343858
    ## 
    ## $b
    ##  [1] -3.3940890  3.7113028 -5.0333000  0.6381511 -8.3595848 -3.4126698
    ##  [7] -7.0699145 -5.0355480 -1.9237212 -4.4960905  9.6453761  0.7756012
    ## [13] -2.7071832 -0.7777110  5.0867692 -1.1367178  0.9183465  6.8293666
    ## [19] -8.7622629 -7.0870161  4.6762491 -1.7626867 -2.5544213  0.2262692
    ## [25] -1.7734742  7.8111603  3.2415143 -7.4416637 -2.3293689 -3.0910634
    ## 
    ## $c
    ##  [1]  9.663214  9.906666 10.039069  9.775875  9.945072  9.486269 10.000293
    ##  [8]  9.647729  9.816676  9.711991  9.975708 10.256220  9.632057  9.994864
    ## [15]  9.941738  9.534038 10.245374  9.717757  9.836742  9.994882 10.020318
    ## [22]  9.904948  9.799020 10.334048 10.129532  9.710902 10.264547  9.683369
    ## [29] 10.105506 10.372818 10.141737  9.776636 10.208998 10.047140 10.203109
    ## [36] 10.207824 10.129402 10.060117 10.192562  9.949723
    ## 
    ## $d
    ##  [1] -3.3725102 -3.6995389 -0.9704899 -2.9209496 -2.4839956 -3.2325942
    ##  [7] -3.8259577 -3.3658887 -1.9926432 -3.3195600 -2.2129222 -3.4869846
    ## [13] -2.6526452 -2.9809173 -3.7037874 -2.6611982 -3.4703962 -3.1080641
    ## [19] -5.0612504 -4.0741845

``` r
list_norm[[1]]
```

    ##  [1] 2.5249955 4.0450324 3.5479659 3.3622448 5.1573232 3.9882118 3.7225453
    ##  [8] 2.7010365 4.8201765 2.6990133 3.3683754 3.6339455 2.3918356 0.4601406
    ## [15] 5.0633208 2.4836491 4.3403378 1.9576506 4.2879522 2.9343858

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
    ## 1  3.37  1.15

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.15  4.81

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.229

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.13 0.855

Let’s use a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```
