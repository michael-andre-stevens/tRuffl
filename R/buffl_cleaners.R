

#' Cleanup a buffl sociodemo
#'
#' @param x a variable
#'
#' @return the cleaned variable
#' @export
cleanup_sociodemo <- function(x) {

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




#' Cleanup a buffl item
#'
#' @param x a tibble with the log of the complete questionnaire
#' @param number the block number that has to be cleaned
#'
#' @return x with cleaned variable(s)
#' @export
cleanup_question <- function(x, number) {

  number <- sprintf("%03d", number[1])
  outname <- sprintf("Q%s", number[1])
  outname2 <- sprintf("Q%s_", number[1])

  multicol_output <- FALSE

  df <- x %>%
    dplyr::select(tidyselect::matches(number))

  question_type <- df %>%
    dplyr::summarize_at(dplyr::vars(tidyselect::matches("type")), first_non_na) %>%
    dplyr::pull()

  if(question_type == "Slider") {
    out <- cleanup_slider(df)
  } else  if(question_type == "Multiple Choice Question") {
    out <- cleanup_multiplechoice(df)
  } else if (question_type == "Checkboxes") {
    out <- cleanup_checkbox(df)
    out2 <- dplyr::bind_cols(compute_checkbox_presence(out), compute_checkbox_order(out))
    multicol_output <- TRUE
  } else if (question_type == "Video") {
    out <- cleanup_video(df)
  } else if (question_type == "Open Question") {
    out <- cleanup_openquestion(df)
  } else {
    stop("Unknown variable type")
  }

  if(!multicol_output) {
    x %>%
      dplyr::select(-tidyselect::matches(!!outname)) %>%
      tibble::add_column(!!outname:=out)
  }
  else {
    x %>%
      dplyr::select(-tidyselect::matches(!!outname)) %>%
      tibble::add_column(!!outname:=out) %>%
      tibble::add_column(!!outname2:=out2) %>%
      tidyr::unpack(!!outname2, names_sep="")
  }
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
cleanup_video <- function(x) {

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
cleanup_openquestion <- function(x) {

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
cleanup_slider <- function(x) {

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
cleanup_multiplechoice <- function(x) {

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


#' Cleanup a variable block from a checkbox choice item
#'
#' @param x a tibble with the variables that are logged for a multiple choice item (type, startedAt, finishedAt, question, answer, answerString)
#'
#' @return the response variable that was computed (a string with a levels attribute)
#' @keywords internal
cleanup_checkbox <- function(x) {

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

  # the response values for each participant
  value <- x %>%
    dplyr::select(tidyselect::matches("answers$")) %>%
    dplyr::pull()

  attr(value, "levels") <- inlabels
  attr(value, "class") <- "checkbox"
  attr(value, "label") <- question_label
  value
}



#' Title
#'
#' @param x a checkbox variable
#'
#' @return the response variables that were computed (a set of logical variables)
#' @export
compute_checkbox_presence <- function(x) {

  my_as_integer <- function(x) {
    atx <- attributes(x)
    x <- as.integer(x)
    attributes(x) <- atx
    x
  }

  binvar <- x %>%
    stringr::str_split(",", simplify=TRUE) %>%
    apply(2, as.numeric) %>%
    data.frame %>%
    dplyr::mutate(idx = 1:dplyr::n()) %>%
    tidyr::gather('key', 'value', -'idx') %>%
    dplyr::mutate(value=.data$value+1) %>%
    dplyr::select(-.data$key) %>%
    dplyr::mutate(key=1) %>%
    dplyr::distinct() %>%
    tidyr::spread('value', 'key', fill=0) %>%
    dplyr::select(-.data$idx, -matches("<NA>"))

  # generate label for each question
  inlevels <- attr(x, "levels")
  for (i in 1:ncol(binvar)) {
    attr(binvar[,i], "label") <- paste(inlevels[i])
    attr(binvar[,i], "label0") <- attr(x, "label")
  }
  binvar <- binvar %>% dplyr::mutate_all(my_as_integer)
  names(binvar) <- paste("I", names(binvar), sep="")

  binvar
}


#' Title
#'
#' @param x a checkbox variable
#'
#' @return the response variables that were computed (a set of rank variables)
#' @export
compute_checkbox_order <- function(x) {

  my_as_integer <- function(x) {
    atx <- attributes(x)
    x <- as.integer(x)
    attributes(x) <- atx
    x
  }

  fix_colnames <- function(x) {
    colnames(x) <- paste0("X", 1:ncol(x))
    x
  }

  rankvar <- x %>%
    stringr::str_split(",", simplify=TRUE) %>%
    apply(2, as.numeric) %>%
    fix_colnames() %>%
    data.frame() %>%
    dplyr::mutate(X0 = Inf) %>%
    dplyr::mutate(idx = 1:dplyr::n()) %>%
    tidyr::gather('key', 'value', -'idx') %>%
    dplyr::mutate(value=.data$value+1) %>%
    dplyr::mutate(key=as.numeric(stringr::str_replace(.data$key, "X", ""))) %>%
    dplyr::distinct() %>%
    stats::na.omit() %>%
    dplyr::distinct(.data$idx, .data$value, .keep_all=TRUE) %>%
    tidyr::spread('value', 'key', fill=NA) %>%
    dplyr::select(-.data$idx, -.data$`Inf`)


  # generate label for each question
  inlevels <- attr(x, "levels")
  for (i in 1:ncol(rankvar)) {
    attr(rankvar[,i], "label") <- paste(inlevels[i])
    attr(rankvar[,i], "label0") <- attr(x, "label")
  }
  rankvar <- rankvar %>% dplyr::mutate_all(my_as_integer)
  names(rankvar) <- paste("R", names(rankvar), sep="")

  rankvar
}




#' Cleanup a variable block from a checkbox item
#'
#' @param x a tibble with the variables that are logged for a multiple checkbox item (type, startedAt, finishedAt, question, answers, answerStrings)
#'
#' @return the response variables that were computed (a set of logical variables)
#' @keywords internal
cleanup_checkbox0 <- function(x) {

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

  # numeric matrix that indicates the order of the alternatives
  rankvar <- numvar %>%
    data.frame %>%
    dplyr::mutate(X0 = Inf) %>%
    dplyr::mutate(idx = 1:dplyr::n()) %>%
    tidyr::gather('key', 'value', -'idx') %>%
    dplyr::mutate(value=.data$value+1) %>%
    dplyr::mutate(key=as.numeric(stringr::str_replace(.data$key, "X", ""))) %>%
    dplyr::distinct() %>%
    stats::na.omit() %>%
    dplyr::distinct(.data$idx, .data$value, .keep_all=TRUE) %>%
    tidyr::spread('value', 'key', fill=NA) %>%
    dplyr::select(-.data$idx, -.data$`Inf`)

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

  for (i in 1:ncol(rankvar)) {
    attr(rankvar[,i], "label") <- paste(inlabels[i])
    attr(rankvar[,i], "label0") <- question_label
  }

  my_as_integer <- function(x) {
    atx <- attributes(x)
    x <- as.integer(x)
    attributes(x) <- atx
    x
  }

  binvar <- binvar %>% dplyr::mutate_all(my_as_integer)
  rankvar <- rankvar %>% dplyr::mutate_all(my_as_integer)

  names(binvar) <- paste("I", names(binvar), sep="")
  names(rankvar) <- paste("R", names(rankvar), sep="")

  dplyr::bind_cols(binvar, rankvar)
}

