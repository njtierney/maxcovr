#' York Listed Buildings.
#'
#' Listed buildings provided by the City of York Council, made available here:
#' <https://data.gov.uk/dataset/listed-buildings24/resource/8c32fb55-0e40-457f-98f9-6494503e283b>.
#' This data contains public sector information licensed under the Open
#' Government Licence v3.0:
#' <https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/>.
#'
#' @format A data frame with seven variables: `long`, `lat`, `object_id`,
#'   `desig_id`, `pref_ref`, `name`, and `grade`.
#'
#' \describe{
#' \item{`long`}{longitude of the building}
#' \item{`lat`}{latitude of the building}
#' \item{`object_id`}{unique identifier for the building}
#' \item{`desig_id`}{ID related to a feature that is not yet known to me}
#' \item{`pref_ref`}{ID related to a feature that is not yet known to me}
#' \item{`name`}{name of the building}
#' \item{`grade`}{one of the three (I, II, III) cateogories of listed buildings}
#' }
#'
#'   For further details, see
#'   <https://www.york.gov.uk/info/20215/conservation_and_listed_buildings/1346/listed_buildings>
#'   and
#'   <https://data.gov.uk/dataset/listed-buildings24/resource/8c32fb55-0e40-457f-98f9-6494503e283b>
#'
"york"
