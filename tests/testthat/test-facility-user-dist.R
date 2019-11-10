library(dplyr)
york_sample <- york[1:100, ]
york_crime_sample <- york_crime[1:100, ]

fud <- facility_user_dist(facility = york_sample,
                          user = york_crime_sample,
                          coverage_distance = 100,
                          nearest = "facility")

fud_user <- facility_user_dist(facility = york_sample,
                               user = york_crime_sample,
                               coverage_distance = 100,
                               nearest = "user")

fud_both <- facility_user_dist(facility = york_sample,
                               user = york_crime_sample,
                               coverage_distance = 100,
                               nearest = "both")

test_that("facility_user_dist dims are correct", {
  expect_equal(nrow(fud), 100)
  expect_equal(ncol(fud), 23)
  expect_equal(nrow(fud_user), 100)
  expect_equal(ncol(fud_user), 23)
  expect_equal(nrow(fud_both), 10000)
  expect_equal(ncol(fud_both), 23)
})

fud_names <- c("category",
               "persistent_id",
               "date",
               "lat_user",
               "long_user",
               "street_id",
               "street_name",
               "context",
               "id",
               "location_type",
               "location_subtype",
               "outcome_status",
               "user_id",
               "long_facility",
               "lat_facility",
               "object_id",
               "desig_id",
               "pref_ref",
               "name",
               "grade",
               "facility_id",
               "distance",
               "is_covered")

test_that("facility_user_dist colnames are correct", {
  expect_equal(names(fud), fud_names)
  expect_equal(names(fud_user), fud_names)
  expect_equal(names(fud_both), fud_names)
})

# fud$distance[1:10] %>% clipr::write_clip()
# fud_user$distance[1:10] %>% clipr::write_clip()
# fud_both$distance[1:10] %>% clipr::write_clip()

test_dist <- c(35.9538304360533,
  39.1579634183108,
  51.86122278729,
  71.1570227797109,
  72.9559841716581,
  74.212107230042,
  84.5365495693242,
  84.5365495693242,
  90.8733481867223,
  95.67669665991)


test_dist_user <- c(35.9538304360533,
                    39.1579634183108,
                    44.219815510871,
                    46.8039599545661,
                    51.86122278729,
                    71.1570227797109,
                    72.9559841716581,
                    81.2208374331007,
                    90.8733481867223,
                    91.1252782407915)


test_dist_both <- c(499.677213095804,
                    1048.61399413466,
                    1017.21840417504,
                    985.989826235699,
                    4160.31232284114,
                    4160.31232284114,
                    116.685973920053,
                    120.533572731341,
                    608.734657854414,
                    471.860232504794)

test_that("facility_user_dist is reasonably close", {
    expect_true(all(dplyr::near(fud$distance[1:10], test_dist)))
    expect_true(all(dplyr::near(fud_user$distance[1:10], test_dist_user)))
    expect_true(all(dplyr::near(fud_both$distance[1:10], test_dist_both)))
})
