# maxcovr 0.0.3.9000 (2016-12-03)

# UNDER DEVELOPMENT

* added a vignette that describes how to perform cross validation on the maxcovr problem using modelr and purrr

* maxcovr now returns a tibble of lists. This makes it easier to perform other tasks, and more consistent and easier to work with.


# maxcovr 0.0.2.9991 (2016-11-30)

## NEW FEATURES

* added c++ functions to calculate the distance matrices and binary matrices
* added wrapper function `nearest` to find the nearest lat/long points from one dataframe to another and then calculate the distance between the two. This is at least 10 times faster than the previous method using joins and dplyr.
* changed API for max_coverage, it now takes 3 dataframes, but it does more of the behind the scenes work, at the cost of taking slightly longer as it calculates the A matrix. Speedups are definitely still available though, as the c++ functions are yet to be parallelised.
* max_coverage now also calculates a summary, which can be retrieved as `mc_model$summary`.

## BUG FIXES

* fixed c++ code for distance_matrix

# maxcovr 0.0.0.9800 (2016-11-27)

## NEW FEATURES

* added `summarise_coverage()` function, which calculates coverage based upon a distance dataframe created from `facility_user_dist()`.
* added `york_crime` data containing crime locations around the city of york.


## MINOR IMPROVEMENTS

* improved README to include examples of usage (again)
* removed artifacts relating to previous work; maxcovr now works for other kinds of data

# maxcovr 0.0.0.9700 (2016-11-14)

# NEW FEATURES

* renamed from `copertura` to `maxcovr`
* added `york` listed building data, taken from [data.gov.uk](https://data.gov.uk/dataset/listed-buildings24/resource/8c32fb55-0e40-457f-98f9-6494503e283b)

## UNDER DEVELOPMENT

* gurobi solver in max_coverage.
* faster matrix methods in max_coverage

# maxcovr 0.0.0.9600 (2016-11-07)

## NEW FEATURES

* added draft cpp code for `spherical_distance` and `distance_matrix`
* updated `max_coverage` to include the `extract_mc_result` inside the function, rather than needing a separate function call. This was to save space, as the A matrix and friends can be rather large, especially if you are running the coverage multiple times under different conditions.

# maxcovr 0.0.2.9500

## NEW FEATURES

* fixed bug in spherical_distance to return metres. Before this is was assuming there were 100m in a kilometer!
* added parameters to `facility_user_distance` - `coverage_distance`, which allows you to specify the coverage you are interested in, and `nearest`, where you specify `nearest` to be "facility" if you want the nearest facility to each user, "user" if you want the nearest user to each facility, and NULL if you just want the complete pairwise distances.

# maxcovr 0.0.2.90000

## NEW FEATURES

* Added a `NEWS.md` file to track changes to the package.
* Added a helper function to assist with extracting the results from the model
* This will eventually be broken up into smaller functions, so that you can be really specific about what function you want to run to get what result. Ideally, each function, except for the one that runs the model, should return one, or maybe two arguments.   


<!--NEW FEATURES, MINOR IMPROVEMENTS, BUG FIXES, DEPRECATED AND DEFUNCT -- >
