Itereation and List Columns
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------ tidyverse_conflicts() --
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
    ## -3.2316 -0.4017  0.2595  0.1670  0.8879  2.1972

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
    ##  [1] 0.5803615 4.3462254 2.4187111 2.8810465 3.4409695 3.7601838 4.5075199
    ##  [8] 3.1064840 1.3105319 3.4556820 1.4462803 4.2122853 2.8120904 2.1655108
    ## [15] 2.9158812 3.7534909 2.4230875 2.5758861 1.9616670 2.1908404
    ## 
    ## $b
    ##  [1] -3.87743937  1.09589197  3.02405125 -1.60291842 -0.77545510 -0.62489450
    ##  [7] -3.47031324 -3.10249268  0.52987556 10.26509339 -3.66643776  6.71289060
    ## [13]  4.06113002 -5.35223430  2.86487361 -0.11666069 -0.01785567 -0.29019382
    ## [19] -0.80288435 -2.35220535 -9.14642151 -2.26607438 -6.35388384  5.60498989
    ## [25]  0.50647952  0.69292516 -0.72534448  1.49371381 12.60436700 -4.12911078
    ## 
    ## $c
    ##  [1] 10.038301  9.977055 10.034962 10.127154 10.305829  9.849876 10.312190
    ##  [8]  9.835584  9.815780  9.561905  9.757599  9.936145  9.923998  9.562918
    ## [15] 10.084730 10.068755  9.694640  9.811062 10.098616  9.822844 10.259795
    ## [22]  9.858501 10.009146 10.356539  9.827231  9.719380  9.942925 10.065165
    ## [29]  9.720694 10.239941 10.336015  9.693125  9.609883 10.070752 10.239161
    ## [36] 10.162271 10.146432  9.647002  9.984024 10.471880
    ## 
    ## $d
    ##  [1] -3.843715 -3.994444 -3.701508 -3.123632 -3.793040 -2.105456 -4.171881
    ##  [8] -4.113311 -2.088137 -3.409227 -1.916633 -5.098555 -3.186965 -1.643852
    ## [15] -3.549500 -4.445307 -1.012058 -2.964361 -2.516360 -4.119199

``` r
list_norm[[1]]
```

    ##  [1] 0.5803615 4.3462254 2.4187111 2.8810465 3.4409695 3.7601838 4.5075199
    ##  [8] 3.1064840 1.3105319 3.4556820 1.4462803 4.2122853 2.8120904 2.1655108
    ## [15] 2.9158812 3.7534909 2.4230875 2.5758861 1.9616670 2.1908404

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
    ## 1  2.81  1.05

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0261  4.60

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.237

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.24  1.06

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
    ##  [1] 0.5803615 4.3462254 2.4187111 2.8810465 3.4409695 3.7601838 4.5075199
    ##  [8] 3.1064840 1.3105319 3.4556820 1.4462803 4.2122853 2.8120904 2.1655108
    ## [15] 2.9158812 3.7534909 2.4230875 2.5758861 1.9616670 2.1908404
    ## 
    ## $b
    ##  [1] -3.87743937  1.09589197  3.02405125 -1.60291842 -0.77545510 -0.62489450
    ##  [7] -3.47031324 -3.10249268  0.52987556 10.26509339 -3.66643776  6.71289060
    ## [13]  4.06113002 -5.35223430  2.86487361 -0.11666069 -0.01785567 -0.29019382
    ## [19] -0.80288435 -2.35220535 -9.14642151 -2.26607438 -6.35388384  5.60498989
    ## [25]  0.50647952  0.69292516 -0.72534448  1.49371381 12.60436700 -4.12911078
    ## 
    ## $c
    ##  [1] 10.038301  9.977055 10.034962 10.127154 10.305829  9.849876 10.312190
    ##  [8]  9.835584  9.815780  9.561905  9.757599  9.936145  9.923998  9.562918
    ## [15] 10.084730 10.068755  9.694640  9.811062 10.098616  9.822844 10.259795
    ## [22]  9.858501 10.009146 10.356539  9.827231  9.719380  9.942925 10.065165
    ## [29]  9.720694 10.239941 10.336015  9.693125  9.609883 10.070752 10.239161
    ## [36] 10.162271 10.146432  9.647002  9.984024 10.471880
    ## 
    ## $d
    ##  [1] -3.843715 -3.994444 -3.701508 -3.123632 -3.793040 -2.105456 -4.171881
    ##  [8] -4.113311 -2.088137 -3.409227 -1.916633 -5.098555 -3.186965 -1.643852
    ## [15] -3.549500 -4.445307 -1.012058 -2.964361 -2.516360 -4.119199

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
    ## 1  2.81  1.05

``` r
mean_and_sd(listcol_df$sample[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0261  4.60

``` r
mean_and_sd(listcol_df$sample[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.237

Can I just map?

``` r
map(listcol_df$sample, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.81  1.05
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0261  4.60
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.237
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.24  1.06

Can I add a list column? Saving output in this dataframe

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(sample, mean_and_sd), 
    medians = map(sample, median)
    )
```
