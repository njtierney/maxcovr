
<!-- README.md is generated from README.Rmd. Please edit that file -->
maxcovr
=======

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/njtierney/maxcovr?branch=master&svg=true)](https://ci.appveyor.com/project/njtierney/maxcovr)[![Travis-CI Build Status](https://travis-ci.org/njtierney/maxcovr.svg?branch=master)](https://travis-ci.org/njtierney/maxcovr)[![Coverage Status](https://img.shields.io/codecov/c/github/njtierney/maxcovr/master.svg)](https://codecov.io/github/njtierney/maxcovr?branch=master)

maxcovr provides tools to make it easy to solve the "maximum covering location problem", a binary optimisation problem described by [Church](http://www.geog.ucsb.edu/~forest/G294download/MAX_COVER_RLC_CSR.pdf). Currently it uses the `lp` solver from the `lpsolve` package.

How to Install
==============

``` r

# install.packages("devtools")
devtools::install_github("njtierney/maxcovr")
```

Usage
=====

Disclaimer: The following is a fictitious example using real world data.

Consider the toy example where we are playing a tower defense game and we need to place crime surveillance towers to detect crime.

We have two datasets, `york`, and `york_crime`:

-   `york` contains listed building GPS locations in the city of York, provided by the city of york
-   `york_crime` contains a set of crime data from the [`ukpolice` package](https://www.github.com/njtierney/ukpolice), containing crime data from September 2016.

In this game we already have a few towers built, which are placed on top of the listed buildings with a grade of I. We will call this dataset `york_selected`, and the remaining building locations `york_unselected`

``` r

library(maxcovr)
library(tidyverse)
#> Loading tidyverse: ggplot2
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages ----------------------------------------------
#> filter(): dplyr, stats
#> lag():    dplyr, stats

# subset to be the places with towers built on them.
york_selected <- york %>% filter(grade == "I")

york_unselected <- york %>% filter(grade != "I")
```

The purpose of the game is to build towers in places so that they are within 100m of crime. We are going to use the crime data that we have to help us choose ideal locations to place towers.

This can be illustrated with the following graphic, where the red circles indicate the current coverage of the building locations.

``` r

library(leaflet)

leaflet() %>%
    addCircleMarkers(data = york, 
                     radius = 1,
                     color = "blue") %>%
    addCircles(data = york_selected, 
               radius = 100,
               stroke = FALSE,
               color = "red") %>%
    addTiles()
#> Assuming 'long' and 'lat' are longitude and latitude, respectively
#> Assuming 'long' and 'lat' are longitude and latitude, respectively
```

![](README-unnamed-chunk-2-1.png)

Currently they are very squashed, we want to build towers out further to maximise the coverage. The question is, where do you put them?

Let's have a look at how far away the towers are from current crime, using the `nearest` function, takes the two data.frames and calculates the distance between every building (york\_selected) and crime (york\_crime), and returns a dataframe with every crime and the distance to the nearest building, along with the appropriate columns from the building dataframe.

``` r

dat_dist <- york_selected %>% nearest(york_crime)

head(dat_dist)
#> # A tibble: 6 × 22
#>   to_id nearest_id   distance              category
#>   <dbl>      <dbl>      <dbl>                 <chr>
#> 1     1         66  165.85752 anti-social-behaviour
#> 2     2         48 2086.76298 anti-social-behaviour
#> 3     3         55   68.23116 anti-social-behaviour
#> 4     4         11  286.34132 anti-social-behaviour
#> 5     5         25  535.78713 anti-social-behaviour
#> 6     6         20  159.90888 anti-social-behaviour
#> # ... with 18 more variables: persistent_id <chr>, date <chr>,
#> #   lat_to <dbl>, long_to <dbl>, street_id <chr>, street_name <chr>,
#> #   context <chr>, id <chr>, location_type <chr>, location_subtype <chr>,
#> #   outcome_status <chr>, long_nearest <dbl>, lat_nearest <dbl>,
#> #   object_id <int>, desig_id <chr>, pref_ref <int>, name <chr>,
#> #   grade <chr>
```

You can instead return a dataframe which has every building in the rows, and the nearest crime to the building, by simply changing the order.

``` r

dat_dist_bldg <- york_crime %>% nearest(york_selected)
head(dat_dist_bldg)
#> # A tibble: 6 × 22
#>   to_id nearest_id  distance   long_to   lat_to object_id desig_id
#>   <dbl>      <dbl>     <dbl>     <dbl>    <dbl>     <int>    <chr>
#> 1     1         33  35.95383 -1.085647 53.95965      6144  DYO1195
#> 2     2        183  35.79129 -1.085433 53.96776      6142  DYO1373
#> 3     3        503  95.34739 -1.083081 53.96131      3463   DYO365
#> 4     4        273  44.28661 -1.080678 53.95818      3461   DYO583
#> 5     5        908  26.51255 -1.082697 53.95686      3460   DYO916
#> 6     6        495 325.87131 -1.134198 54.00190      3450  DYO1525
#> # ... with 15 more variables: pref_ref <int>, name <chr>, grade <chr>,
#> #   category <chr>, persistent_id <chr>, date <chr>, lat_nearest <dbl>,
#> #   long_nearest <dbl>, street_id <chr>, street_name <chr>, context <chr>,
#> #   id <chr>, location_type <chr>, location_subtype <chr>,
#> #   outcome_status <chr>
```

To evaluate the coverage we can use `summarise_coverage`

``` r

dat_dist %>% 
    mutate(is_covered = distance <= 100) %>%
    summarise_coverage()
#> # A tibble: 1 × 7
#>   distance_within n_cov   pct_cov n_not_cov pct_not_cov dist_avg  dist_sd
#>             <dbl> <int>     <dbl>     <int>       <dbl>    <dbl>    <dbl>
#> 1             100   339 0.1868798      1475   0.8131202 1400.192 1596.676
```

This tells us that out of all the crime, 18.68% of it is within 100m, 339 crimes are covered, but the mean distance to the surveillance camera is 1400m.

Maximising coverage
-------------------

Say then we want to add another 20 surveillance towers, but we want to use the best 20, we use `max_coverage`.

``` r

system.time(
# mc_20 <- max_coverage(A = dat_dist_indic,
mc_20 <- max_coverage(existing_facility = york_selected,
                      proposed_facility = york_unselected,
                      user = york_crime,
                      n_added = 20,
                      distance_cutoff = 100)
)
#>    user  system elapsed 
#>   1.735   0.187   1.986
```

If you want to find the improvement in coverage from the original state we do the following:

One can also use `map` from `purrr` to fit many different configurations of `n_added`. (Future work will look into allowing `n_added` to take a vector of arguments).

``` r
library(purrr)

n_add_vec <- c(20, 40, 60, 80, 100)

system.time(
map_mc_model <- map_df(.x = n_add_vec,
                       .f = ~max_coverage(existing_facility = york_selected,
                                          proposed_facility = york_unselected,
                                          user = york,
                                          distance_cutoff = 100,
                                          n_added = .))
)
#>    user  system elapsed 
#>  14.447   1.107  16.725
```

This returns a list of dataframes, which we can bind together like so:

``` r

map_cov_results <- bind_rows(map_mc_model$model_coverage)
```

We can then visualise the effect on coverage:

``` r

bind_rows(map_mc_model$existing_coverage[[1]],
          map_cov_results) %>%
    ggplot(aes(x = factor(n_added),
               y = pct_cov)) + 
    geom_point() +
    geom_line(group = 1) + 
    theme_minimal()
```

![](README-unnamed-chunk-10-1.png)

performing cross validation on max\_coverage
============================================

Thanks to the `modelr` package, it is relatively straightforward to perform cross validation.

``` r
# first we partition the data into 5 folds
library(modelr)
mc_cv <- modelr::crossv_kfold(york_crime, 5) %>% 
    # we change the test and train sets from the `resample`
    # to tibbles
    mutate(test = map(test,as_tibble),
           train = map(train,as_tibble))
```

This creates a dataframe with test and training sets

``` r

mc_cv
#> # A tibble: 5 × 3
#>                   train                test   .id
#>                  <list>              <list> <chr>
#> 1 <tibble [1,451 × 12]> <tibble [363 × 12]>     1
#> 2 <tibble [1,451 × 12]> <tibble [363 × 12]>     2
#> 3 <tibble [1,451 × 12]> <tibble [363 × 12]>     3
#> 4 <tibble [1,451 × 12]> <tibble [363 × 12]>     4
#> 5 <tibble [1,452 × 12]> <tibble [362 × 12]>     5
```

We then fit the model on the training set using `map_df`

``` r

# then we fit the model
system.time(
    mc_cv_fit <- map_df(mc_cv$train,
                     ~max_coverage(existing_facility = york_selected,
                                   proposed_facility = york_unselected,
                                   user = ., # training set goes here
                                   n_added = 20,
                                   distance_cutoff = 100))
)
#>    user  system elapsed 
#>   5.202   0.549   5.928
```

Then we can use the `summary_mc_cv` function to extract out the summaries from each fold

``` r

summarised_cv <- summary_mc_cv(mc_cv_fit, mc_cv)
summarised_cv
#> # A tibble: 5 × 9
#>   n_added n_fold distance_within n_cov   pct_cov n_not_cov pct_not_cov
#>     <dbl>  <chr>           <dbl> <int>     <dbl>     <int>       <dbl>
#> 1      20      1             100   196 0.1350793      1255   0.8649207
#> 2      20      2             100   226 0.1557547      1225   0.8442453
#> 3      20      3             100   207 0.1426602      1244   0.8573398
#> 4      20      4             100   216 0.1488629      1235   0.8511371
#> 5      20      5             100   194 0.1336088      1258   0.8663912
#> # ... with 2 more variables: dist_avg <dbl>, dist_sd <dbl>
```

You can then do nifty things like explore how the coverage changes over the folds

``` r

summarised_cv %>%
    ggplot(aes(x = n_fold,
               y = pct_cov)) + 
    geom_point() +
    geom_line(group = 1) + 
    ylim(0.05,0.25) + 
    theme_minimal()
```

![](README-unnamed-chunk-15-1.png)

Here we see that the pct\_coverage doesn't seem to change much across the folds.

Coming up next, we will explore how to perform cross validation over all areas

``` r
# 
# n_add_vec <- c(20,40)
# 
# system.time(
#     mc_cv_fit_many <- 
#         map2_df(mc_cv$train, # training set goes here
#                 n_add_vec,
#                 .f = ~max_coverage,
#                 existing_facility = york_selected,
#                 proposed_facility = york_unselected,
#                 distance_cutoff = 100)
# )
# 
# 
# system.time(
#     mc_cv_fit_many <- 
#         pmap(.l = list(user = mc_cv$train, # training set goes here
#                        n_added = n_add_vec),
#              .f = ~max_coverage,
#              existing_facility = york_selected,
#              proposed_facility = york_unselected,
#              distance_cutoff = 100)
# )
```

Known Issues
============

-   `max_coverage()` may take a bit of time to run, depending on your data size. From initial testing, if the product of the number of rows of the `proposed_facilities` and `users` exceeds 100 million, it might take more than 1 minute. Of course, this may depend on the structure / complexity of your data and problem.

Future Work
===========

Through December 2016 I will be focussing on making `maxcovr` more usable, building in better summaries into the model fitting process, keeping the work in a dataframe, adding speed improvements using c++ where possible, and implementing an optional gurobi solver. We will also be creating standardized plots for exploration of data and results.

In 2017 I will be providing alternative interfaces to other solvers, potentially using something like [`ompr`](https://github.com/dirkschumacher/ompr), to give users their own choice of solver, such as glpk or CPLEX. In this time I will also be looking into exploiting embarassingly parallel features for data preprocessing.

If you have any suggestions, please file an issue and I will get to it as soon as I can.

Code of Conduct
===============

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
