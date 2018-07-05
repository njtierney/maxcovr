# maxcovr 0.1.0.9000 (2018-07-05)

# maxcovr 0.1.0 (2018-07-05) "Mach in D Minor"

This release is created to assist in replicating previous journal articles
that used this software. The API of maxcovr is likely to change from here.

# maxcovr 0.0.5.9700 (2017-02-08)

## NEW FEATURES (UNDER DEVELOPMENT)

* `max_coverage_relocation` now works for different data, thanks to a bug fix.
* prototype solver engines added. maxcovr now works with `glpk`, and `gurobi`, in addition to `lpSolve`. Testing still to be conducted.
* summary function `coverage` added as a one liner to take two dataframes and from the coverage of one dataframe on another. Documentation and tests still need work
* `summary_mc_cv_relocate` added for `relocation` methods. This will eventially be an S3 method, I think?
* rewrote the extraction functions for results from `max_coverage`, replacing them with key functions. This will make things easier to debug and extend in the future. These functions start with `extract_`. There will likely be some updates to this in the future
* started work on refactoring the `max_coverage` functions to work with vectors, this fule is called `maxcovr-refactor-vectors`. This provides some more efficient computation of preparation of matrices for optimisation, and will provide substantial speedups for larger N, and for when multiple `n_added`'s are needed.
* `max_coverage` now returns the entire solution from `lpSolve`, except for the constraints, because they are too large (can easily be over 1Gb)

## MINOR IMPROVEMENTS

* `summary.maxcovr` and `summary.maxcovr_relocation` method now return information about the previous distances.
* Improvements have been made to the overall returning API of `max_coverage_relocation` so that it plays better with extracting summary information.

## BUG FIXES

* Removed the scraggly test dataset that was lurking in the shadows and making my model not work.

# maxcovr 0.0.4.9000 (2017-01-17)

## NEW FEATURES (UNDER DEVELOPMENT)

* new function, `max_coverage_relocation`, takes a arguments for total cost, installation cost, and relocation costs and then works out how many facilities it can place, and potentially remove and replace to obtain optimum coverage. The function is currently under development. In the future it will be absorted into `max_coverage`.

* added a results extraction method for `max_coverage_relocation`, `extract_mc_result_relocation`. Eventually this will be be absorbed into the `extract_mc_result` function, through some kind of S3 method.

* added `print` and `summary` S3 method for `max_coverage` and `max_coverage_relocation`, and an `is.maxcovr_relocation` and `is.maxcovr`, which should be handy for testing.

## BUG FIXES

* Changed the specification of m_under_i and m_over_i to more clearly reflect "The gain of removing an AED from position i"

* Removed the scraggly test dataset that was lurking in the shadows and making my model not work.


# maxcovr 0.0.3.9000 (2016-12-03)

## UNDER DEVELOPMENT

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
