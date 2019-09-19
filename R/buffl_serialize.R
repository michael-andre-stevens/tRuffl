

#' Serialize an R variable
#'
#'
#' @param x an R variable
#'
#' @return a Json string that holds the R variable and its attributes
#' @export
json_serialize <- function(x) {
  x %>%
    jsonlite::toJSON(dataframe="columns")
}


#' Unserialize an R variable
#'
#' @param x Json string
#'
#' @return an R variable that is reconstructed from the Json string
#' @export
json_unserialize <- function(x) {
  x %>%
    jsonlite::fromJSON() %>%
    data.frame()
}
