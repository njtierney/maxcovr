# copertura 0.0.0.9600 (2016-11-07)

## NEW FEATURES

* added draft cpp code for `spherical_distance` and `distance_matrix`
* updated `max_coverage` to include the `extract_mc_result` inside the function, rather than needing a separate function call. This was to save space, as the A matrix and friends can be rather large, especially if you are running the coverage multiple times under different conditions.

# copertura 0.0.2.9500

## NEW FEATURES

* fixed bug in spherical_distance to return metres. Before this is was assuming there were 100m in a kilometer!
* added parameters to `facility_user_distance` - `coverage_distance`, which allows you to specify the coverage you are interested in, and `nearest`, where you specify `nearest` to be "facility" if you want the nearest facility to each user, "user" if you want the nearest user to each facility, and NULL if you just want the complete pairwise distances.

# copertura 0.0.2.90000

## NEW FEATURES

* Added a `NEWS.md` file to track changes to the package.
* Added a helper function to assist with extracting the results from the model
* This will eventually be broken up into smaller functions, so that you can be really specific about what function you want to run to get what result. Ideally, each function, except for the one that runs the model, should return one, or maybe two arguments.   


<!--NEW FEATURES, MINOR IMPROVEMENTS, BUG FIXES, DEPRECATED AND DEFUNCT -- >
