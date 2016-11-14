library(readr)

download.file(url = "http://data.cyc.opendata.arcgis.com/datasets/fd6a709a29bf4f6ca085adb956ce127c_15.csv",
              destfile = "york.csv")

york <- readr::read_csv("york.csv")

names(york) <- c("long",
                                  "lat",
                                  "object_id",
                                  "desig_id",
                                  "pref_ref",
                                  "name",
                                  "grade")

# names(york_listed_buildings)

# View(york_listed_buildings)
