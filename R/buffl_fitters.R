#' Fit a linear model
#'
#' This model is used for slider questions
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#'
#' @return the model fit
#' @export
fit_linear_model <- function(data) {

  if (ncol(data) == 1) {
    names(data) <- "y"
    form <- stats::as.formula("y ~ 1")
  } else if(ncol(data) == 2) {
    names(data) <- c("y", "x")
    form <- stats::as.formula("y ~ x")
  } else {
    stop("fit_model expects two variables as input")
  }

  stats::lm(form, data=data)
}



#' Fit a linear model with a logistic link function
#'
#' This model is used for multiple choice questions with two alternatives
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#'
#' @return the model fit
#' @export
fit_logistic_model <- function(data) {

  if (ncol(data) == 1) {
    names(data) <- "y"
    form <- stats::as.formula("y ~ 1")
  } else if(ncol(data) == 2) {
    names(data) <- c("y", "x")
    form <- stats::as.formula("y ~ x")
  } else {
    stop("fit_model expects two variables as input")
  }

  stats::glm(form, data=data, family=stats::binomial)
}



#' Fit a multinomial model
#'
#' This model is used for multiple choice questions with more than two unordered alternatives
#'
#' The default function uses the nnet::multinom function
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#'
#' @return the model fit
#' @export
fit_multinomial_model <- function(data) {

  if (ncol(data) == 1) {
    names(data) <- "y"
    form <- stats::as.formula("y ~ 1")
  } else if(ncol(data) == 2) {
    names(data) <- c("y", "x")
    form <- stats::as.formula("y ~ x")
  } else {
    stop("fit_model expects two variables as input")
  }

  nnet::multinom(form, data=data, trace=FALSE, model=TRUE)
}


#' Fit an ordinal model
#'
#' This model is used for multiple choice questions with more than two ordered alternatives
#'
#' The default function uses the MASS::polr function
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#'
#' @return the model fit
#' @export
fit_ordinal_model <- function(data) {

  if (ncol(data) == 1) {
    names(data) <- "y"
    form <- stats::as.formula("y ~ 1")
  } else if(ncol(data) == 2) {
    names(data) <- c("y", "x")
    form <- stats::as.formula("y ~ x")
  } else {
    stop("fit_model expects two variables as input")
  }

  MASS::polr(form, data=data, Hess=TRUE)
}



#' Fit a mixed effects linear model with a logistic link function
#'
#' This model is used for checkbox questions (multiple choice where more than one alternative can be selected)
#'
#' The default function uses the lme4:glmer function
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#'
#' @return the model fit
#' @export
fit_mixed_model <- function(data) {

  qq <- names(data)[1]

  if (ncol(data) == 1) {
    names(data) <- "y"
    form <- stats::as.formula("y ~ alternative + (1|id)")
  } else if(ncol(data) == 2) {
    names(data) <- c("y", "x")
    form <- stats::as.formula("y ~ alternative * x + (1|id)")
  } else {
    stop("fit_model expects two variables as input")
  }

  yy <- compute_checkbox_presence(data$y)

  longdata <- data %>%
    tibble::add_column(yy) %>%
    tidyr::unpack(yy) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::select(-.data$y) %>%
    tidyr::pivot_longer(cols=tidyselect::starts_with("I", ignore.case=FALSE), names_to = "alternative", values_to = "y") %>%
    dplyr::arrange(.data$id)


  suppressWarnings(lme4::glmer(form, data=longdata, family=stats::binomial))
}





#' Fit a fixed effects linear model with a logistic link function
#'
#' This model is used for checkbox questions (multiple choice where more than one alternative can be selected)
#'
#' Alternative implementation that uses the stats::glm function (and fits a fixed effects model instead of a mixed effects model)
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#'
#' @return the model fit
#' @export
fit_fixed_model <- function(data) {

  qq <- names(data)[1]

  if (ncol(data) == 1) {
    names(data) <- "y"
    form <- stats::as.formula("y ~ alternative")
  } else if(ncol(data) == 2) {
    names(data) <- c("y", "x")
    form <- stats::as.formula("y ~ alternative * x")
  } else {
    stop("fit_model expects two variables as input")
  }

  yy <- compute_checkbox_presence(data$y)

  longdata <- data %>%
    tibble::add_column(yy) %>%
    tidyr::unpack(yy) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::select(-.data$y) %>%
    tidyr::pivot_longer(cols=tidyselect::starts_with("I", ignore.case=FALSE), names_to = "alternative", values_to = "y") %>%
    dplyr::arrange(.data$id)

  stats::glm(form, data=longdata, family=stats::binomial)
}








