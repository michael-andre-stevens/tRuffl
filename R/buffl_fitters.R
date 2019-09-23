#' Process the arguments of a fitter function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return a dataframe
#' @keywords internal
make_data <- function(y, x=NULL) {
  if (is.null(x)) {
    data.frame(y=dplyr::pull(y))
  } else {
    data.frame(y=dplyr::pull(y), x=dplyr::pull(x))
  }
}


#' Process the arguments of a fitter function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return a dataframe
#' @keywords internal
make_long_data <- function(y, x=NULL) {
  if (is.null(x)) {
    y %>%
      dplyr::ungroup() %>%
      dplyr::mutate(id = 1:dplyr::n()) %>%
      tidyr::gather('alternative', 'y', -'id') %>%
      dplyr::arrange(.data$id)
  } else {
      data.frame(y, x) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(id = 1:dplyr::n()) %>%
        tidyr::gather('alternative', 'y', -'id', -'x') %>%
        dplyr::mutate(alternative=factor(.data$alternative)) %>%
        dplyr::arrange(.data$id)
  }
}

#' Process the arguments of a fitter function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return a formula
#' @keywords internal
make_formula <- function(y, x=NULL) {
  if (is.null(x)) {
    stats::as.formula("y ~ 1")
  } else {
    stats::as.formula("y ~ x")
  }
}


#' Process the arguments of a fitter function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return a formula
#' @keywords internal
make_mixed_formula <- function(y, x=NULL) {
  if (is.null(x)) {
    formula <- stats::as.formula("y ~ alternative + (1|id)")
  } else {
    formula <- stats::as.formula("y ~ alternative * x + (1|id)")
  }
}


#' Process the arguments of a fitter function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return a formula
#' @keywords internal
make_fixed_formula <- function(y, x=NULL) {
  if (is.null(x)) {
    formula <- stats::as.formula("y ~ alternative")
  } else {
    formula <- stats::as.formula("y ~ alternative * x")
  }
}


#' Fit a linear model
#'
#' This model is used for slider questions
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return the model fit
#' @export
fit_linear_model <- function(y, x=NULL) {

  dfx <- make_data(y, x)
  form <- make_formula(y, x)
  fit <- stats::lm(form, data=dfx)
  fit
}


#' Fit a linear model with a logistic link function
#'
#' This model is used for multiple choice questions with two alternatives
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return the model fit
#' @export
fit_logistic_model <- function(y, x=NULL) {

  dfx <- make_data(y, x)
  dfx$y <- as.logical(dfx$y)
  form <- make_formula(y, x)
  fit <- stats::glm(form, data=dfx, family=stats::binomial)
  fit
}


#' Fit a multinomial model
#'
#' This model is used for multiple choice questions with more than two unordered alternatives
#'
#' The default function uses the nnet::multinom function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return the model fit
#' @export
fit_multinomial_model <- function(y, x=NULL) {

  dfx <- make_data(y, x)
  form <- make_formula(y, x)
  fit <- nnet::multinom(form, data=dfx, trace=FALSE, model=TRUE)
  fit
}


#' Fit a multinomial model
#'
#' This model is used for multiple choice questions with more than two unordered alternatives
#'
#' Alternative implementation that uses the mlogit::mlogic function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return the model fit
#' @export
fit_multinomial_model2 <- function(y, x=NULL) {

  dfx <- make_data(y, x)

  if (is.null(x)) {
    dfxm <- mlogit::mlogit.data(dfx, choice="y", shape = "wide", alt.levels=levels(y))
    fit <- mlogit::mlogit(y ~ 0 | 1, data = dfxm)
  } else {
    dfxm <- mlogit::mlogit.data(dfx, choice="y", shape = "wide", alt.levels=levels(y))
    fit <- mlogit::mlogit(y ~ 0 | x, data = dfxm)
  }
  fit
}


#' Fit an ordinal model
#'
#' This model is used for multiple choice questions with more than two ordered alternatives
#'
#' The default function uses the MASS::polr function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return the model fit
#' @export
fit_ordinal_model <- function(y, x=NULL) {

  dfx <- make_data(y, x)
  form <- make_formula(y, x)
  fit <- MASS::polr(form, data=dfx, Hess=TRUE)
  fit
}


#' Fit an ordinal model
#'
#' This model is used for multiple choice questions with more than two ordered alternatives
#'
#' Alternative implementation that uses the ordinal::clm function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return the model fit
#' @export
fit_ordinal_model2 <- function(y, x=NULL) {

  dfx <- make_data(y, x)
  form <- make_formula(y, x)
  fit <- ordinal::clm(form, data=dfx)
  fit
}


#' Fit a mixed effects linear model with a logistic link function
#'
#' This model is used for checkbox questions (multiple choice where more than one alternative can be selected)
#'
#' The default function uses the lme4:glmer function
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return the model fit
#' @export
fit_mixed_model <- function(y, x=NULL) {


  dfx <- make_long_data(y, x)
  form <- make_mixed_formula(y, x)
  fit <- suppressWarnings(lme4::glmer(form, data=dfx, family=stats::binomial))
  fit
}



#' Fit a fixed effects linear model with a logistic link function
#'
#' This model is used for checkbox questions (multiple choice where more than one alternative can be selected)
#'
#' Alternative implementation that uses the stats::glm function (and fits a fixed effects model instead of a mixed effects model)
#'
#' @param y the dependent variable
#' @param x an optional predictor
#'
#' @return the model fit
#' @export
fit_fixed_model <- function(y, x=NULL) {

  dfx <- make_long_data(y, x)
  form <- make_fixed_formula(y, x)

  fit <- stats::glm(form, data=dfx, family=stats::binomial)
  fit
}




