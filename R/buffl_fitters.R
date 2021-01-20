#' Fit a model
#'
#' This is the main entry function that calls the most appropriate fitter function
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#' @param prefer_mixed_model if TRUE (the default) prefer a mixed effects model for
#' checkbox questions. If FALSE prefer a fixed effects model for checkbox questions
#' (faster but underestimates the variance of the estimates)
#'
#' @return the model fit
#' @export
fit_model <- function(data, prefer_mixed_model=TRUE) {

  depvar <- data[,1] %>% dplyr::pull()


  if (is.numeric(depvar)) {
    fit_linear_model(data)

  } else if (is.factor(depvar)) {
    if(length(levels(depvar)) == 2) {
      fit_logistic_model(data)
    } else if (is.ordered(depvar)) {
      fit_ordinal_model(data)
    } else {
      fit_multinomial_model(data)
    }

  } else if (class(depvar)=="checkbox") {
    if(prefer_mixed_model) {
      fit_mixed_model(data)
    } else {
      fit_fixed_model(data)
    }

  } else {
    stop("fit model expects a numeric, factor or checkbox question")
  }
}


#' Fit a linear model
#'
#' This model is used for slider questions
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#'
#' @return the model fit
#' @export
fit_linear_model <- function(data) {

  nd <- names(data)
  if (length(nd) == 1) {
    form <- stats::as.formula(paste(nd, "~ 1"))
  } else if(length(nd) == 2) {
    form <- stats::as.formula(paste(nd[1], "~", nd[2]))
  } else {
    stop("fit_model expects maximally two variables as input")
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

  nd <- names(data)
  if (length(nd) == 1) {
    form <- stats::as.formula(paste(nd, "~ 1"))
  } else if(length(nd) == 2) {
    form <- stats::as.formula(paste(nd[1], "~", nd[2]))
  } else {
    stop("fit_model expects maximally two variables as input")
  }
  stats::glm(form, data=data, family=stats::binomial)
}


#' Prepare data for a fixed or mixed linear model fit
#'
#' This model is used for checkbox questions (multiple choice where more than one alternative can be selected)
#'
#' @param data a dataframe with the dependent variable and an optional covariate
#'
#' @return the data in long format
#' @keywords internal
make_long_data <- function(data) {


  y <- data[,1] %>% dplyr::pull()
  yy <- compute_checkbox_presence(y)
  ymap <- data.frame(alternative=attr(y, "levels"), alternative.idx=names(yy), stringsAsFactors=FALSE) %>%
    dplyr::mutate(alternative=forcats::fct_inorder(.data$alternative))

  nd <- names(data)

    longdata <- data %>%
    tibble::add_column(yy) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::select(-tidyselect::matches(nd[1])) %>%
    tidyr::pivot_longer(cols=tidyselect::starts_with("I", ignore.case=FALSE), names_to = "alternative.idx", values_to = nd[1]) %>%
    dplyr::arrange(.data$id)

  longdata %>% dplyr::left_join(ymap, by="alternative.idx")
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

  nd <- names(data)
  if (length(nd) == 1) {
    form <- stats::as.formula(paste(nd, "~ alternative"))
  } else if(length(nd) == 2) {
    form <- stats::as.formula(paste(nd[1], "~ alternative *", nd[2]))
  } else {
    stop("fit_model expects maximally two variables as input")
  }

  longdata <- make_long_data(data)

  stats::glm(form, data=longdata, family=stats::binomial)
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

  nd <- names(data)
  if (length(nd) == 1) {
    form <- stats::as.formula(paste(nd, "~ alternative + (1|id)"))
  } else if(length(nd) == 2) {
    form <- stats::as.formula(paste(nd[1], "~ alternative *", nd[2], "+ (1|id)"))
  } else {
    stop("fit_model expects maximally two variables as input")
  }

  longdata <- make_long_data(data)

  suppressWarnings(lme4::glmer(form, data=longdata, family=stats::binomial))
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

  nd <- names(data)
  if (length(nd) == 1) {
    form <- stats::as.formula(paste(nd, "~ 1"))
  } else if(length(nd) == 2) {
    form <- stats::as.formula(paste(nd[1], "~", nd[2]))
  } else {
    stop("fit_model expects maximally two variables as input")
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

  nd <- names(data)
  if (length(nd) == 1) {
    form <- stats::as.formula(paste(nd, "~ 1"))
  } else if(length(nd) == 2) {
    form <- stats::as.formula(paste(nd[1], "~", nd[2]))
  } else {
    stop("fit_model expects maximally two variables as input")
  }

  MASS::polr(form, data=data, Hess=TRUE)
  #ordinal::clm(form, data=data)
}


