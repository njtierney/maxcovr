library(ukpolice)
library(rgeos)
library(sp)
library(tidyverse)
library(maxcovr)
york_poly <- ukp_geo_chull(york, lat, long)

plot(york$long, york$lat)
polygon(york_poly$long, york_poly$lat, asp=1)

york_poly_mat <- cbind(york_poly$long, york_poly$lat)

centroid <- Polygon(york_poly_mat) |>
    SpatialPoints |>
    gCentroid |>
    data.frame

points(centroid, col="red")

z <- 0.05

newhull <- cbind(york_poly_mat, centroid) |>
    mutate(dx = `1` - x,
           dy = `2` - y) |>
    # euclidean distance
    mutate(dr = sqrt(dx^2 + dy^2),
           dx = dx/dr,
           dy = dy/dr) |>
    mutate(newx = `1` + z*dx,
           newy = `2` + z*dy)

# ggplot(data=newhull, aes(x=newx, y=newy)) +
#     geom_polygon(fill="grey", alpha=0.5) +
#     geom_polygon(fill="grey", alpha=0.5,
#                  aes(x=`1`, y=`2`)) +
#     geom_point(data=as.data.frame(york_poly_mat),
#                aes(x=V1, y=V2)) +
#     coord_equal()

newhull_poly <- data_frame(long = newhull$newx,
                           lat = newhull$newy)

york_crime <- ukp_crime_poly(poly = newhull_poly)

# devtools::use_data(york_crime)
