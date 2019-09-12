

#' Read a buffl datafile
#'
#' @param path path to a file
#' @param guess_max maximum number of records to use for guessing column types
#' @param csv type of csv file (can be csv or csv2)
#' @param ... further arguments passed to or from other methods
#'
#' @return a tibble
#' @export
read_buffl <- function(path, guess_max=10000, csv="csv", ...) {

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


#' Return the first non-missing value of a vector
#'
#' This is useful for inspecting the content of the 'type' variables. Type can be:
#'
#' \describe{
#'   \item{Slider}{A likert style item}
#'   \item{Multiple Choice Question}{A multiple choice item where you select one alternative}
#'   \item{Checkboxes}{A multiple choice item where you select one or more alternatives}
#'   \item{Open Question}{Free text}
#'   \item{Video}{Watch a video, no response from the user}
#' }
#'
#' @param x a vector
#'
#' @return the first non-missing value
#' @keywords internal
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  x[1]
}




#' Cleanup a variable block from a video
#'
#' @param x a tibble with the variables that are logged for a video (type, startedAt, finishedAt, question).
#'
#' @return the response variable that was computed (all missings as no respons is expected)
#' @keywords internal
cleanup_buffl_video <- function(x) {

  # ungroup
  x <- dplyr::ungroup(x)

    # the question label
  question_label <- x %>%
    dplyr::summarize_at(dplyr::vars(tidyselect::matches("question")), first_non_na) %>%
    dplyr::pull()

  # the name of the output
  idx <- readr::parse_number(names(x)[1])
  varname <- sprintf("Q%03d", idx)

  # the value (NA)
  value <- as.numeric(rep(NA, nrow(x)))

  attr(value, "label") <- question_label

  value
}


#' Cleanup a variable block from an open question
#'
#' @param x a tibble with the variables that are logged for an open question (type, startedAt, finishedAt, question, answer)
#'
#' @return the response variable that was computed (a character variable)
#' @keywords internal
cleanup_buffl_openquestion <- function(x) {

  # ungroup
  x <- dplyr::ungroup(x)

  # the question label
  question_label <- x %>%
    dplyr::summarize_at(dplyr::vars(tidyselect::matches("question")), first_non_na) %>%
    dplyr::pull()

  # the value
  value <- x %>%
    dplyr::select(tidyselect::matches("answer")) %>%
    dplyr::pull()

  attr(value, "label") <- question_label
  value
}


#' Cleanup a variable block from a slider
#'
#' @param x a tibble with the variables that are logged for a slider (type, startedAt, finishedAt, question, answer)
#'
#' @return the response variable that was computed (an integer variable)
#' @keywords internal
cleanup_buffl_slider <- function(x) {

  # ungroup
  x <- dplyr::ungroup(x)

    # the question label
  question_label <- x %>%
    dplyr::summarize_at(dplyr::vars(tidyselect::matches("question")), first_non_na) %>%
    dplyr::pull()

  # the value
  value <- x %>%
    dplyr::select(tidyselect::matches("answer")) %>%
    dplyr::pull()

  attr(value, "label") <- question_label
  value
}



#' Cleanup a variable block from a multiple choice item
#'
#' @param x a tibble with the variables that are logged for a multiple choice item (type, startedAt, finishedAt, question, answer, answerString)
#'
#' @return the response variable that was computed (a factor)
#' @keywords internal
cleanup_buffl_multiplechoice <- function(x) {

  # ungroup
  x <- dplyr::ungroup(x)

  # the question label
  question_label <- x %>%
    dplyr::summarize_at(dplyr::vars(tidyselect::matches("question")), first_non_na) %>%
    dplyr::pull()

  # the numeric value
  value <- x %>%
    dplyr::select(tidyselect::matches("answer$")) %>%
    dplyr::pull() %>%
    as.numeric()

  # factor levels: all possible numeric values
  numvals <- x %>%
    dplyr::select(tidyselect::matches("answer$")) %>%
    dplyr::pull() %>%
    stats::na.omit() %>%
    as.numeric()

  # factor labels: all possible string values
  stringvals <- x %>%
    dplyr::select(tidyselect::matches("answerString$")) %>%
    dplyr::pull() %>%
    stats::na.omit() %>%
    as.character()

  # sort levels and labels
  inlevels <- sort(numvals)
  inlabels <- stringvals[order(numvals)]

  # make factor
  # default is unordered, but we sort by numerical label
  ordered = FALSE
  if (!ordered) {
    value <- factor(value, levels=inlevels, labels=inlabels)
  } else {
    value <- ordered(value, levels=inlevels, labels=inlabels)
  }

  # add label
  attr(value, "label") <- question_label
  value
}


#' Cleanup a variable block from a checkbox item
#'
#' @param x a tibble with the variables that are logged for a multiple checkbox item (type, startedAt, finishedAt, question, answers, answerStrings)
#'
#' @return the response variables that were computed (a set of logical variables)
#' @keywords internal
cleanup_buffl_checkbox <- function(x) {

  # ungroup
  x <- dplyr::ungroup(x)

  # the question label
  question_label <- x %>%
    dplyr::summarize_at(dplyr::vars(tidyselect::matches("question")), first_non_na) %>%
    dplyr::pull()

  # convert numeric response to a matrix
  numvar <- x %>%
    dplyr::select(tidyselect::matches("answers$")) %>%
    dplyr::pull() %>%
    stringr::str_split(",", simplify=TRUE) %>%
    apply(2, as.numeric)

  # convert string response to a matrix
  stringvar <- x %>%
    dplyr::select(tidyselect::matches("answerStrings$")) %>%
    dplyr::pull() %>%
    as.character() %>%
    stringr::str_split(",(?! )", simplify=TRUE) %>%
    apply(2, stringr::str_trim)

  # binary matrix that indicates what alternatives were selected
  binvar <- numvar %>%
    data.frame %>%
    dplyr::mutate(idx = 1:dplyr::n()) %>%
    tidyr::gather('key', 'value', -'idx') %>%
    dplyr::mutate(value=.data$value+1) %>%
    dplyr::select(-.data$key) %>%
    dplyr::mutate(key=1) %>%
    dplyr::distinct() %>%
    tidyr::spread('value', 'key', fill=0) %>%
    dplyr::select(-.data$idx, -"<NA>")

  # generate mapping table for levels and labels
  numvals <- as.numeric(numvar+1) %>%
    unique() %>%
    stats::na.omit() %>%
    as.numeric()

  stringvals <- as.character(stringvar) %>%
    unique()  %>%
    stats::na.omit() %>%
    as.character()

  inlevels <- sort(numvals)
  inlabels <- stringvals[order(numvals)]

  # generate label for each question
  for (i in 1:ncol(binvar)) {
    attr(binvar[,i], "label") <- paste(inlabels[i])
    attr(binvar[,i], "label0") <- question_label
  }

  my_as_integer <- function(x) {
    atx <- attributes(x)
    x <- as.integer(x)
    attributes(x) <- atx
    x
  }

  binvar %>% dplyr::mutate_all(my_as_integer)
}





#' Cleanup a buffl item
#'
#' @param x a tibble with the log of the complete questionnaire
#' @param number the block number that has to be cleaned
#'
#' @return x with cleaned variable(s)
#' @export
cleanup_buffl_question <- function(x, number) {

  number <- sprintf("%03d", number[1])
  outname <- sprintf("Q%s", number[1])

  df <- x %>%
    dplyr::select(tidyselect::matches(number))

  question_type <- df %>%
    dplyr::summarize_at(dplyr::vars(tidyselect::matches("type")), first_non_na) %>%
    dplyr::pull()

  if(question_type == "Slider") {
    out <- cleanup_buffl_slider(df)
  } else  if(question_type == "Multiple Choice Question") {
    out <- cleanup_buffl_multiplechoice(df)
  } else if (question_type == "Checkboxes") {
    out <- cleanup_buffl_checkbox(df)
  } else if (question_type == "Video") {
    out <- cleanup_buffl_video(df)
  } else if (question_type == "Open Question") {
    out <- cleanup_buffl_openquestion(df)
  } else {
    stop("Unknown variable type")
  }

  if(is.list(out)) {
    x %>% dplyr::select(-tidyselect::matches(!!outname)) %>% tibble::add_column(!!outname:=out) %>% tidyr::unpack(!!outname, names_sep="_")
  }
  else {
    x %>% dplyr::select(-tidyselect::matches(!!outname)) %>% tibble::add_column(!!outname:=out)
  }
}


#' Cleanup a buffl sociodemo
#'
#' @param x a variable
#'
#' @return the cleaned variable
#' @export
cleanup_buffl_sociodemo <- function(x) {

  if(is.logical(x)) {
    out <- factor(x, levels=c(FALSE, TRUE), labels=c("FALSE", "TRUE"))
  }
  else if (Hmisc::all.is.numeric(x, extras="N/A")) {
    out <- as.numeric(x)
  } else if (length(unique(x)) < 20) {
    out <- as.factor(x)
  } else {
    out <- x
  }

  out
}




