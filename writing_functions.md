Writing Functions
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------
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

## Do Something Simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.124115604  1.228926068  1.116957917  0.709935503 -0.100667582
    ##  [6]  0.528221772  0.348545115  1.456359382 -0.276893326  0.408231933
    ## [11] -0.286739275  0.329392945  1.065695601  0.266388778 -0.005877642
    ## [16] -0.115921582  0.534453726 -2.504102379 -1.260713657 -1.602362993
    ## [21]  0.092103520  0.277957195  0.202221003  1.714571542 -1.152922703
    ## [26] -0.594373729  0.141349013 -1.025485670 -0.906868140 -1.712497938

``` r
#Z scores, SD away from the mean
```

I want a function to compute z scores

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if(length(x) < 3) {
    stop("Inpute must have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_scores(x_vec)
```

    ##  [1]  1.124115604  1.228926068  1.116957917  0.709935503 -0.100667582
    ##  [6]  0.528221772  0.348545115  1.456359382 -0.276893326  0.408231933
    ## [11] -0.286739275  0.329392945  1.065695601  0.266388778 -0.005877642
    ## [16] -0.115921582  0.534453726 -2.504102379 -1.260713657 -1.602362993
    ## [21]  0.092103520  0.277957195  0.202221003  1.714571542 -1.152922703
    ## [26] -0.594373729  0.141349013 -1.025485670 -0.906868140 -1.712497938

Try function on other things

``` r
z_scores(3)
```

    ## Error in z_scores(3): Inpute must have at least 3 numbers

``` r
#Sd of 3 can't do
#can't do mean on character var 
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
#can't take mena of dataset, only number list
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

``` r
#coerced to 0 and 1
```
