Writing Functions
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts -----------------
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

    ##  [1] -0.25759399  0.10521635 -0.65143649  0.25412812 -0.22571020 -1.57608801
    ##  [7]  2.47270294 -0.07539634  0.43087262  1.26032047  0.30869865  0.43081919
    ## [13] -0.85909048 -0.35450083 -0.97611060 -0.40895219 -2.01697399 -0.09369472
    ## [19]  1.02218730  0.35962215  0.24707361  0.47140379 -1.98951809 -0.05639246
    ## [25]  0.88168918 -0.91815340  0.49914991  1.74699031 -0.82871146  0.79744864

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

    ##  [1] -0.25759399  0.10521635 -0.65143649  0.25412812 -0.22571020 -1.57608801
    ##  [7]  2.47270294 -0.07539634  0.43087262  1.26032047  0.30869865  0.43081919
    ## [13] -0.85909048 -0.35450083 -0.97611060 -0.40895219 -2.01697399 -0.09369472
    ## [19]  1.02218730  0.35962215  0.24707361  0.47140379 -1.98951809 -0.05639246
    ## [25]  0.88168918 -0.91815340  0.49914991  1.74699031 -0.82871146  0.79744864

Try function on other things. These should give errors.

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

## Multiple Outputs

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if(length(x) < 3) {
    stop("Inpute must have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

Check that function worked

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.48  4.13

## Multiple inputs

Give me a sample size, mean, sd

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

#create dataframe
sim_data %>% 
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.74  3.29

``` r
#get mean and sd
```

I’d like to do this with a function

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 3) {
  
  sim_data = 
  tibble(
    x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
  
}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.88  2.92

``` r
sim_mean_sd(samp_size = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.03  3.05

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.63  2.80

## Lets review Napoleon Dynamite

Scraping reviews from Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

#extract elements of data we're interested in - ID css tag
#Selector gadget
#Convert to text

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  #get first digit number between 0-9 at start of string input 
  as.numeric()
#convert from char to numeric = review stars

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_page1 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
#put reviews into tibble
```

What about next page of reviews?

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

#extract elements of data we're interested in - ID css tag
#Selector gadget
#Convert to text

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  #get first digit number between 0-9 at start of string input 
  as.numeric()
#convert from char to numeric = review stars

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_page2 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
#put reviews into tibble
```

Don’t want to just copy and paste. There are several pages of reviews
(1800 reviews).

Turn into function. Input, read URL for that page of reviews.

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

Let’s try the function

``` r
dyn_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"

read_page_reviews(dyn_url)
```

    ## # A tibble: 10 x 3
    ##    title                     stars text                                         
    ##    <chr>                     <dbl> <chr>                                        
    ##  1 A top favorite movie !!       5 "Love this movie, needed to add it to my col~
    ##  2 Best.Movie!                   5 "I enjoyed showing my children this \"classi~
    ##  3 Great Movie                   5 "I love this movie. Showed it to my middle s~
    ##  4 Tina, you fat lard, come~     5 "A very quotable, awkard and hilarious movie~
    ##  5 Funny!                        4 "It is a great movie although it’s a little ~
    ##  6 Excellent for families        5 "Highly recommend for family entertainment"  
    ##  7 Hilarious!                    5 "Hilarious!"                                 
    ##  8 Excellent in all fronts.      5 "Excellent in all fronts."                   
    ##  9 good                          5 "good"                                       
    ## 10 Buy                           5 "Very good movie not very expensive"

Read a few pages of reviews

``` r
dyn_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dyn_urls = str_c(dyn_url_base, 1:5)

dyn_urls[3]
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"

``` r
all_reviews = 
  bind_rows(
    read_page_reviews(dyn_urls[1]),
    read_page_reviews(dyn_urls[2]),
    read_page_reviews(dyn_urls[3]),
    read_page_reviews(dyn_urls[4]),
    read_page_reviews(dyn_urls[5])
  )

all_reviews
```

    ## # A tibble: 0 x 3
    ## # ... with 3 variables: title <chr>, stars <dbl>, text <chr>

If we wanted 50 pages of reviews, would prefer not to have this code 50
times.
