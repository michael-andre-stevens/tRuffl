#' Read a buffl csv datafile
#'
#' @param path path to a file
#' @param guess_max maximum number of records to use for guessing column types
#' @param csv type of csv file (can be csv or csv2)
#' @param ... further arguments passed to or from other methods
#'
#' @return a tibble
#' @export
import_csv <- function(path, guess_max=10000, csv="csv", ...) {

  # add leading zeroes to numbers
  # so that string sort
  # gives the same result as a numerical sort
  fix_buffl_names <- function(x) {

    nn <- names(x)

    f <- function(x) {
      x <- gsub('[[:punct:] ]+','',x)
      x <- as.numeric(x)
      sprintf("%03d", x)
    }

    ff <- function(x) {
      m <- gregexpr("\\[([0-9]+)\\]", x)  # getal tussen []
      regmatches(x, m) <- lapply(regmatches(x, m), f)
      x
    }

    names(x) <- ff(nn)
    x
  }

  # reorder blocks by index
  reorder_buffl_names <- function(x) {

    na_to_zero <- function(x) {
      x[is.na(x)] <- 0
      x
    }

    nn <- tibble::tibble(names=names(x)) %>%
      dplyr::mutate(number = suppressWarnings(readr::parse_number(.data$names))) %>%
      dplyr::mutate(number = na_to_zero(.data$number)) %>%
      dplyr::arrange(.data$number)

    x %>% dplyr::select(nn$names)
  }

  if(csv=="csv") {
    raw_input <- suppressMessages(readr::read_csv(path, guess_max=guess_max, na = c("", "NA", "N/A"), ...))
  } else if (csv=="csv2") {
    raw_input <- suppressMessages(readr::read_delim(delim=";", path, guess_max=guess_max, na = c("", "NA", "N/A"), ...))
  } else {
    stop("Unknown csv type")
  }

  # tbd: within q block, order as: start, stop, type, question, answer(s), ...
  raw_input %>%
    fix_buffl_names() %>%
    reorder_buffl_names() %>%
    dplyr::select(
      .data$campaign.name,
      .data$campaign.startedAt,
      .data$campaign.finishedAt,
      tidyselect::matches("campaign"),
      tidyselect::matches("user"),
      tidyselect::matches("block"),
      tidyselect::everything())
}



