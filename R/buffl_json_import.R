#' Read a buffl json datafile
#'
#' @param path path to a file
#'
#' @return a list
#' @export
import_json <- function(path) {

  x <- jsonlite::fromJSON(path, flatten=TRUE)

  sociodemo <- import_json_sociodemo(x)

  get_number_of_questions <- function(json) {
    length(json$results$blockResults[[1]])
  }

  questions <- list()
  for (i in 1:get_number_of_questions(x)) {
    questions[[i]] <- import_json_question(x, i)
  }
  questions <- data.frame(questions)

  dplyr::bind_cols(sociodemo, questions) %>%
      tibble::as.tibble()
}



#' Cleanup a buffl sociodemo
#'
#' @param x a list
#'
#' @return the sociodemo variables
#' @keywords internal
import_json_sociodemo <- function(x) {

  campaign.name = x$name

  x$results %>%
    dplyr::select(-.data$blockResults) %>%
    dplyr::select(-.data$id) %>%
    dplyr::mutate_at(c("startedAt", "finishedAt"), lubridate::ymd_hms) %>%
    dplyr::rename(campaign.startedAt=.data$startedAt, campaign.finishedAt=.data$finishedAt) %>%
    dplyr::mutate(campaign.name=campaign.name) %>%
    dplyr::select(.data$campaign.name, tidyselect::everything())
}


#' Cleanup a buffl item
#'
#' @param x a list with the log of the complete questionnaire
#' @param number the block number that has to be cleaned
#'
#' @return x with cleaned variable(s)
#' @keywords internal
import_json_question <- function(x, number) {

  get_question_type <- function(json, question) {

    type_name <- sprintf("block%03d.type", question)
    question_name <- sprintf("block%03d.question", question)

    json$blocks[question,] %>%
      dplyr::select(-.data$id) %>%
      dplyr::rename(!!type_name := .data$type, !!question_name := .data$question)
  }

  get_answers <- function(json, idx) {

    json <- json$results$blockResults
    ld <- length(json)

    select_idx <- function(idx) { idx }

    my_nrow <- function(x) {
      nn <- nrow(x)
      ifelse(is.numeric(nn), nn, 0)
    }

    fill_na <- function(df) {
      tidyr::complete(df, idx=1:ld)
    }

    idxes <-
      json %>%
      purrr::map(select_idx(idx)) %>%
      purrr::map(my_nrow) %>%
      unlist() %>%
      rle() %>%
      `[[`(1) %>%
      cumsum() +
      `+`(1)


    outdata <- json[[ld]][idx][[1]]
    outdata$idx <- idxes[-length(idxes)]

    outdata <- outdata %>% fill_na() %>% dplyr::select(-idx)

    outdata <- outdata %>% dplyr::mutate_at(c("startedAt", "finishedAt"), lubridate::ymd_hms)


    prefix <- sprintf("block%03d.", idx)
    names(outdata) <-  paste0(prefix, names(outdata))
    outdata
  }

  qq <- get_question_type(x, number) %>% dplyr::mutate(one=1)
  aa <- get_answers(x, number) %>% dplyr::mutate(one=1)
  dplyr::full_join(qq, aa) %>%
      dplyr::mutate(number = c(dplyr::n(), 1:(dplyr::n()-1))) %>%
      dplyr::arrange(number) %>%
      dplyr::select(-.data$one, -.data$number) %>%
      dplyr::select(tidyselect::matches("type"),
        tidyselect::matches("startedAt"),
        tidyselect::matches("finishedAt"),
        tidyselect::matches("question"),
        tidyselect::matches("answer"),
        tidyselect::matches("answerString"))
}



