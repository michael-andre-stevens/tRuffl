#' Convert sociodemo variables to factors
#'
#' Some variables are present in each and every buffl survey. This function converts these into the right format.
#'
#' @param x a data.frame
#'
#' @return the data frame with the fixed variables in the right format
#' @export
buffl_factorize_sociodemos <- function(x) {
  x %>%
    dplyr::mutate(user.gender=factor(.data$user.gender)) %>%
    dplyr::mutate(user.age=as.numeric(.data$user.age)) %>%
    dplyr::mutate(user.civilStatus=factor(.data$user.civilStatus)) %>%
    dplyr::mutate(user.hasChildren=factor(.data$user.hasChildren)) %>%
    dplyr::mutate(user.country=factor(.data$user.country)) %>%
    dplyr::mutate(user.zipCode=factor(.data$user.zipCode))
}
