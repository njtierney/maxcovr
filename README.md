
<!-- README.md is generated from README.Rmd. Please edit that file -->
maxcovr
=======

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/njtierney/maxcovr?branch=master&svg=true)](https://ci.appveyor.com/project/njtierney/maxcovr)[![Travis-CI Build Status](https://travis-ci.org/njtierney/maxcovr.svg?branch=master)](https://travis-ci.org/njtierney/maxcovr)

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

Let's have a look at how far away the towers are from current crime, using the `facility_user_dist` function, which treats the current locations as "facilities", and the individuals as "users". `facility_user_dist` takes the arguments `facility`, a data.frame with columns names "lat" and "long", and `user`, a data.frame with columns "lat" and "long". To reiterate, **facility** is where we want to build the surveillance towers, and **user** is each crime location.

``` r

dat_dist <- facility_user_dist(facility = york_selected, user = york_crime)
```

`facility_user_dist` calculates the distance between every building (york\_selected) and crime (york\_crime), and by default returns a dataframe with every crime and the distance to the nearest building, along with the appropriate columns from the building dataframe.

``` r

dim(york_crime)
#> [1] 1814   12
dim(york_selected)
#> [1] 71  7
dim(dat_dist)
#> [1] 1814   23
```

You can instead return a dataframe which has every building in the rows, and the nearest crime to the building. This is achieved by setting the "nearest" option to be "nearest = user", instead of the default "nearest = facility".

``` r

facility_user_dist(facility = york_selected, 
                   user = york_crime, 
                   nearest = "user")
#> # A tibble: 71 × 23
#>                 category
#>                    <chr>
#> 1  anti-social-behaviour
#> 2  anti-social-behaviour
#> 3  anti-social-behaviour
#> 4  anti-social-behaviour
#> 5  anti-social-behaviour
#> 6  anti-social-behaviour
#> 7          bicycle-theft
#> 8  anti-social-behaviour
#> 9  anti-social-behaviour
#> 10 anti-social-behaviour
#> # ... with 61 more rows, and 22 more variables: persistent_id <chr>,
#> #   date <chr>, lat_user <dbl>, long_user <dbl>, street_id <chr>,
#> #   street_name <chr>, context <chr>, id <chr>, location_type <chr>,
#> #   location_subtype <chr>, outcome_status <chr>, user_id <int>,
#> #   long_facility <dbl>, lat_facility <dbl>, object_id <int>,
#> #   desig_id <chr>, pref_ref <int>, name <chr>, grade <chr>,
#> #   facility_id <int>, distance <dbl>, is_covered <lgl>
```

The distances between all buildings and crimes can be returned using "nearest = both".

``` r

facility_user_dist(facility = york_selected,
                   user = york_crime,
                   nearest = "both")
#> # A tibble: 128,794 × 23
#>                 category
#>                    <chr>
#> 1  anti-social-behaviour
#> 2  anti-social-behaviour
#> 3  anti-social-behaviour
#> 4  anti-social-behaviour
#> 5  anti-social-behaviour
#> 6  anti-social-behaviour
#> 7  anti-social-behaviour
#> 8  anti-social-behaviour
#> 9  anti-social-behaviour
#> 10 anti-social-behaviour
#> # ... with 128,784 more rows, and 22 more variables: persistent_id <chr>,
#> #   date <chr>, lat_user <dbl>, long_user <dbl>, street_id <chr>,
#> #   street_name <chr>, context <chr>, id <chr>, location_type <chr>,
#> #   location_subtype <chr>, outcome_status <chr>, user_id <int>,
#> #   long_facility <dbl>, lat_facility <dbl>, object_id <int>,
#> #   desig_id <chr>, pref_ref <int>, name <chr>, grade <chr>,
#> #   facility_id <int>, distance <dbl>, is_covered <lgl>
```

Note: `facility_user_dist` might take a while to run as it creates the cross product of the rows and calculates the distances using haversines formula. It then filters the rows to return the closest facility to a user (when nearest = facility), the closest user to a facility (when nearest = user). The entire cross product is returned when nearest = "both". Future work will explore custom c++ code to make this process faster.

To evaluate the coverage we use `summarise_coverage()`.

`summarise_coverage` uses the distance matrix calculated by `facility_user_dist`.

``` r

dist_summary <- summarise_coverage(dat_dist)
dist_summary
#> # A tibble: 1 × 7
#>   distance_within n_cov   pct_cov n_not_cov pct_not_cov dist_avg  dist_sd
#>             <dbl> <int>     <dbl>     <int>       <dbl>    <dbl>    <dbl>
#> 1             100   339 0.1868798      1475   0.8131202 1400.192 1596.676
```

This tells us that out of all the crime, 18.68% of it is within 100m, 339 crimes are covered, but the mean distance to the surveillance camera is 1400m.

Maximising coverage
-------------------

Say then we want to add another 20 surveillance towers, but we want to use the best 20, we use `max_coverage`.

However, `max_coverage` requires an "A Matrix", which we create using `dat_dist_indic`, which cannot contain crime incidents that are already covered, as we want to maximise the coverage of the crimes not yet covered.

``` r

# identify those crime (user) events that are not covered
# so that they aren't used in the optimisation

tower_id_not_covered <- dat_dist %>%
    filter(is_covered == FALSE) %>%
    select(id)

dat_crime_not_cov <- tower_id_not_covered %>%
    left_join(york_crime, by = "id")
```

``` r

system.time(
    dat_dist_all <- facility_user_dist(facility = york_unselected,
                                       user = dat_crime_not_cov,
                                       coverage_distance = 100,
                                       nearest = "both")
)
#>    user  system elapsed 
#>   2.295   0.411   2.808

# we also need to ensure two things:
# Only consider crimes that aren't already covered

# dat_dist_all <- dat_dist_all %>% filter(is_covered == FALSE)

# remove facilities that have class == I (are currently used).


system.time(
dat_dist_indic <- facility_user_indic(dat_dist_all, dist_indic = 100)
)
#>    user  system elapsed 
#>   1.252   0.277   1.744
```

``` r

system.time(
mc_20 <- max_coverage(A = dat_dist_indic,
                      facility = york_unselected,
                      user = dat_crime_not_cov,
                      n_added = 20)
)
#>    user  system elapsed 
#>   1.055   0.154   1.418
```

If you want to calculate the improvement in coverage from the previous model, we do the following:

``` r

model_20_coverage <- bind_rows(mc_20$facility_selected,
                               york_selected) %>%
    facility_user_dist(facility = .,
                       user = york_crime) %>%
    summarise_coverage()

bind_rows(dist_summary,
          model_20_coverage)
#> # A tibble: 2 × 7
#>   distance_within n_cov   pct_cov n_not_cov pct_not_cov  dist_avg
#>             <dbl> <int>     <dbl>     <int>       <dbl>     <dbl>
#> 1             100   339 0.1868798      1475   0.8131202 1400.1918
#> 2             100   540 0.2976847      1274   0.7023153  788.9317
#> # ... with 1 more variables: dist_sd <dbl>
```

One can also use `map` from `purrr` to fit many different configurations of `n_added`

``` r
library(purrr)

n_add_vec <- c(20, 40, 60, 80, 100)

system.time(
map_mc_model <- map(.x = n_add_vec,
                    .f = ~max_coverage(A = dat_dist_indic,
                                       facility = york_unselected,
                                       user = dat_crime_not_cov,
                                       n_added = .))
)
#>    user  system elapsed 
#>   5.241   0.719   6.728
```

``` r

# map_model_coverage <- map(.x = map_mc_model,
#                           .f = ~bind_rows(.$facility_selected)) %>%
#     bind_rows(york_selected) %>%
    
    
map_model_coverage <- map_mc_summary <- function(mc_model,
                                                 york_selected,
                                                 york_crime){
    
    bind_rows(mc_model$facility_selected,
              york_selected) %>%
    facility_user_dist(facility = .,
                       user = york_crime) %>%
    summarise_coverage()
}
```

``` r
map_cov_results <- map(.x = map_mc_model,
                       .f = ~map_model_coverage(mc_model = .,
                                                york_selected = york_selected,
                                                york_crime = york_crime))
```

We can then visualise the effect on coverage:

``` r

bind_rows(dist_summary,
          map_cov_results) %>%
    mutate(n_added = c(0,20,40,60,80,100)) %>%
    select(n_added,
           everything()) %>%
    ggplot(aes(x = factor(n_added),
               y = pct_cov)) + 
    geom_point() +
    geom_line(group = 1) + 
    theme_minimal()
```

![](README-unnamed-chunk-15-1.png)

Known Issues
============

-   `facility_user_dist()` is slow for larger datasets (if the row product is larger than 1 billion rows), as it requires calculating the pairwise combination of every distance, and then subsetting based on what is closest. Future work is investigating faster, more sensible approaches that use the minimal (and sensible) amount of information from data

-   `max_coverage()` may take a bit of time to run, depending on your data size. If the product of your pairwise distance matrix exceeds 1 billion rows, it might take more than 1 minute.

`maxcovr` is still in beta, so there are likely to be unidentified bugs, please keep this in mind!

Future Work
===========

Through December 2016 I will be focussing on making `maxcovr` more usable, building in summaries into the model fitting process, keeping the work in a dataframe, making it faster using c++ for common summaries, and implementing gurobi for the Solver Engine. Also on the cards are some standardized plots for exploration of data and results.

In 2017 I will be providing alternative interfaces to other solvers, potentially using something like [`ompr`](https://github.com/dirkschumacher/ompr), to give users their own choice of solver, such as glpk or CPLEX.

If you have any suggestions, please file an issue and I will get to it as soon as I can.

Code of Conduct
===============

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
