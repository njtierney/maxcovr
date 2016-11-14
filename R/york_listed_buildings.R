#' York Listed Buildings.
#'
#' Listed buildings provided by the city of York Council
#'
#' @format A data frame with seven variables: \code{long}, \code{lat},
#'   \code{object_id}, \code{desig_id}, \code{pref_ref}, \code{name}, and
#'   \code{grade}.
#'
#' \describe{
#' \item{\code{long}}{longitude}
#' \item{\code{lat}}{latitude}
#' \item{\code{object_id}}{unique identification of the building}
#' \item{\code{desig_id}}{another ID related to a feature that is not yet known to me}
#' \item{\code{pref_ref}}{another ID related to a feature that is not yet known to me}
#' \item{\code{name}}{name of the building}
#' \item{\code{grade}}{one of the three (I, II, III) cateogories of listed buildings}
#' }
#'
#' For further details, see \url{https://www.york.gov.uk/info/20215/conservation_and_listed_buildings/1346/listed_buildings} and \url{https://data.gov.uk/dataset/listed-buildings24/resource/8c32fb55-0e40-457f-98f9-6494503e283b}
#'
"york_listed_buildings"
