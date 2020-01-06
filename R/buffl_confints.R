#' Extract confidence intervals from a
#'
#' This is the main entry function that calls the most appropriate interval function
#'
#' @param fit a fitted model
#'
#' @return the model fit
#' @export
confint_model <- function(fit) {

  class_fit <- class(fit)[1]

  if(class_fit=="lm") {
    confint_linear_model(fit)
  } else if (class_fit=="glm"){
    if("alternative" %in% all.vars(stats::formula(fit))) {
      confint_fixed_model(fit)
    } else {
      confint_logistic_model(fit)
    }
  } else if (class_fit=="glmerMod") {
    confint_mixed_model(fit)
  } else if (class_fit=="multinom") {
    confint_multinomial_model(fit)
  } else if (class_fit=="polr") {
    confint_ordinal_model(fit)
  } else {
    stop("confint_model expects an lm, glm, glmerMod, multinom or polr fit")
  }
}


#' Confidence intervals around the estimates of a linear model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_linear_model <- function(fit) {

  stopifnot(class(fit)=="lm")
  stopifnot(ncol(fit$model) %in% c(1, 2))
  vars <- all.vars(stats::formula(fit))

  if (length(vars)==1) {
    stats::predict(fit, data.frame(1), se=TRUE, interval="confidence") %>%
      purrr::pluck(1) %>%
      data.frame() %>%
      dplyr::rename(lower=.data$lwr, upper=.data$upr)
  } else if (length(vars)==2) {
    effects::effect(vars[2], fit) %>%
      data.frame() %>%
      dplyr::select(-.data$se)
  } else {
    stop("confint_model expects a model with maximally one predictor")
  }
}


#' Confidence intervals around the intercept of a logistic model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_logistic_model <- function(fit) {

  stopifnot(all(class(fit)==c("glm", "lm")))
  stopifnot(stats::family(fit)$family == "binomial")
  stopifnot(stats::family(fit)$link == "logit")

  vars <- all.vars(stats::formula(fit))

  if (length(vars)==1) {
    out <- stats::predict(fit,  newdata=data.frame(1), type="link", se.fit=TRUE) %>%
      data.frame() %>%
      dplyr::mutate(lower=.data$fit-1.96*.data$se.fit) %>%
      dplyr::mutate(upper=.data$fit+1.96*.data$se.fit) %>%
      dplyr::select(-'se.fit', -'residual.scale') %>%
      dplyr::mutate_all(stats::plogis)
  } else if (length(vars)==2) {
    out <- effects::effect(vars[2], fit) %>%
      data.frame() %>%
      dplyr::select(-.data$se)
  } else {
    stop("confint_model expects a model with maximally one predictor")
  }

  lvls <-  levels(fit$data[,1] %>% dplyr::pull())

  out2 <- out %>%
    dplyr::mutate(fit=1-.data$fit) %>%
    dplyr::mutate(tmp=.data$lower) %>%
    dplyr::mutate(upper=1-.data$lower) %>%
    dplyr::mutate(lower=1-.data$tmp) %>%
    dplyr::select(-.data$tmp) %>%
    dplyr::mutate(alternative=lvls[1])

  out <- out %>% dplyr::mutate(alternative=lvls[2])

  out <- dplyr::bind_rows(out, out2) %>%
    dplyr::select(.data$alternative, tidyselect::everything())

  out
}


#' Confidence intervals around the intercept of a logistic fixed effects model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_fixed_model <- function(fit) {

  stopifnot(all(class(fit)==c("glm", "lm")))
  stopifnot(stats::family(fit)$family == "binomial")
  stopifnot(stats::family(fit)$link == "logit")

  vars <- all.vars(stats::formula(fit))

  if (length(vars)==2) {
    out <- effects::effect(vars[2], fit) %>%
      data.frame() %>%
      dplyr::select(-.data$se)
  } else if (length(vars)==3) {
    out <-  effects::effect(paste(vars[2], vars[3], sep=":"), fit) %>%
      data.frame() %>%
      dplyr::select(-.data$se)
  } else {
    stop("confint_model expects a model with maximally one predictor")
  }
  out %>% dplyr::arrange(.data$alternative)
}


#' Confidence intervals around the intercept of a logistic mixed effects model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_mixed_model <- function(fit) {

  stopifnot(class(fit)=="glmerMod")

  vars <- all.vars(stats::formula(fit))

  # work around bug in effects function
  if(exists("longdata")) {
    longdata5684354543450000 <- longdata
  }
  longdata <<- fit@frame

  if (length(vars)==3) {
    out <- effects::effect(vars[2], fit) %>%
      data.frame() %>%
      dplyr::select(-.data$se)
  } else if (length(vars)==4) {
    out <- effects::effect(paste(vars[2], vars[3], sep=":"), fit) %>%
      data.frame() %>%
      dplyr::select(-.data$se)
  } else {
    if(exists("longdata5684354543450000")) {
      longdata <- longdata5684354543450000
    }
    stop("confint_model expects a model with maximally one predictor")
  }
  if(exists("longdata5684354543450000")) {
    longdata <- longdata5684354543450000
  }
  out %>% dplyr::arrange(.data$alternative)
}


#' Confidence intervals around the intercept of a logistic mixed effects model
#'
#' @param fit the fitted model
#'
#' @return category estimates with confidence interval
#' @export
confint_mixed_model_bolker <- function(fit) {

  stopifnot(class(fit)=="glmerMod")

  #http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions

  # apply the formula
  newdata <- fit@frame %>% dplyr::filter(.data$id==1)
  mm <- stats::model.matrix(stats::terms(fit), newdata)
  newdata$fit <- as.numeric(mm %*% lme4::fixef(fit))

  pvar1 <- diag(mm %*% tcrossprod(as.matrix(stats::vcov(fit)),as.matrix(mm)))
  #pvar1 <- diag(mm %*% stats::vcov(fit) %*% t(mm)) # fixed effects variance

  tvar1 <- pvar1 + lme4::VarCorr(fit)[[1]][1]  # fixed effects + random effects variance
  cmult <- 1.96

  newdata <- data.frame(
    newdata,
    lower = newdata$fit-cmult*sqrt(pvar1),
    upper = newdata$fit+cmult*sqrt(pvar1),
    lower_rf = newdata$fit-cmult*sqrt(tvar1),
    upper_rf = newdata$fit+cmult*sqrt(tvar1)
  )

  newdata$fit <- stats::plogis(newdata$fit)
  newdata$lower <- stats::plogis(newdata$lower)
  newdata$upper <- stats::plogis(newdata$upper)
  newdata$lower_rf <- stats::plogis(newdata$lower_rf)
  newdata$upper_rf <- stats::plogis(newdata$upper_rf)

  nd <- names(newdata)
  newdata %>%
    dplyr::select(-tidyselect::matches(nd[1]), -.data$id) %>%
    data.frame(row.names=NULL) %>%
    tibble::column_to_rownames(var="alternative")
}


#' Confidence intervals around the intercept of a multinomial model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_multinomial_model <- function(fit) {

  stopifnot(all(class(fit)==c("multinom", "nnet")))
  vars <- all.vars(stats::formula(fit))

  if (length(vars)==1) {

    # use the delta method for intercept-only model
    ncat <- nrow(stats::coef(fit))
    parameterNames <- paste0("b", 1:ncat)

    denominator <- paste0("exp(", parameterNames, ")", collapse="+")
    denominator <- paste0("(1+", denominator, ")")

    g <- vector("character", ncat+1)
    g[1] <- paste0("1/", denominator)
    for(i in 1:ncat) {
      g[i+1] <- paste0("exp(", parameterNames[i], ")/", denominator)
    }

    out <- list()
    for(i in 1:length(g)) {
      out[[i]] <- car::deltaMethod(fit, g[i], parameterNames=parameterNames)
    }

    out <- dplyr::bind_rows(out) %>%
      dplyr::rename(fit=.data$Estimate) %>%
      dplyr::select(-.data$SE) %>%
      dplyr::rename(lower=.data$`2.5 %`) %>%
      dplyr::rename(upper=.data$`97.5 %`)

    alternatives <- fit$lev
    out %>%
      dplyr::mutate(alternative=alternatives) %>%
      dplyr::select(.data$alternative, tidyselect::everything())

  } else if (length(vars)==2) {

    # use effects function if we have a main effect in the model
    confint_multinomial_ordinal_effect(fit)

  } else {
    stop("confint_model expects a model with maximally one predictor")
  }
}


#' Confidence intervals around the intercept of an ordinal model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_ordinal_model <- function(fit) {

  stopifnot(class(fit)=="polr")

  vars <- all.vars(stats::formula(fit))

  if (length(vars)==1) {

    # use the delta method for intercept-only model
    ncat <- length(fit$zeta)
    beta_names <- NULL
    if(length(stats::coef(fit))>0) {
      beta_names <- paste0("beta", seq_along(stats::coef(fit)))
    }
    zeta_names <- paste0("zeta", seq_along(fit$zeta))
    parameterNames <- c(beta_names, zeta_names)

    # call the delta method
    # polr intercepts are stored in zeta, not in beta
    # we need a wrapper around the delta method
    # that takes this into account
    mydeltamethod <- function(fit, g.) {
        cf <- c(stats::coef(fit), fit$zeta)
        vcv <- stats::vcov(fit)
        rownames(vcv) <- colnames(vcv) <- names(cf) <- parameterNames
        car::deltaMethod(cf, g., vcov.=vcv)
    }

    reshape_prediction <- function(x) {
      dplyr::bind_rows(x) %>%
        dplyr::rename(fit=.data$Estimate) %>%
        dplyr::select(-.data$SE) %>%
        dplyr::rename(lower=.data$`2.5 %`) %>%
        dplyr::rename(upper=.data$`97.5 %`)
    }

    add_alternative <- function(x, prob="probability") {
      lvl <- fit$lev
      x %>%
        dplyr::mutate(alternative=lvl) %>%
        dplyr::mutate(probability=prob) %>%
        dplyr::select(.data$probability, .data$alternative, tidyselect::everything())

    }

    # compute functions for cumulative, reverse cumulative and marginal probabilities
    # apply the delta method on the functions

    # cumulative probability
    g_cum <- vector("character", ncat)
    out_cum <- list()
    for(i in 1:length(g_cum)) {
      g_cum[i] <- paste0("exp(", parameterNames[i], ")/(1+exp(", parameterNames[i], "))")
      out_cum[[i]] <- mydeltamethod(fit, g_cum[i])
    }
    out_cum <- reshape_prediction(out_cum) %>%
      tibble::add_row(fit=1, lower=1, upper=1) %>%
      add_alternative("cumulative probability")

    # reverse cumulatve probability
    g_revcum <- vector("character", ncat)
    out_revcum <- list()
    for(i in 1:length(g_revcum)) {
      g_revcum[i] <- paste0("1-", g_cum[i])
      out_revcum[[i]] <- mydeltamethod(fit, g_revcum[i])
    }
    out_revcum <- reshape_prediction(out_revcum) %>%
      tibble::add_row(fit=1, lower=1, upper=1, .before=1) %>%
      add_alternative("reverse cumulatve probability")

    # probability mass
    g_prob <- vector("character", ncat+1)
    g_prob[1] <- g_cum[1]
    out_prob <- list()
    for(i in 2:(length(g_prob)-1)) {
      g_prob[i] <- paste0(g_cum[i], " - ", g_cum[i-1])
    }
    g_prob[length(g_cum)+1] <- paste0(1, " - ", g_cum[length(g_cum)])

    for(i in 1:length(g_prob)) {
      out_prob[[i]] <- mydeltamethod(fit, g_prob[i])
    }

    out_prob <- reshape_prediction(out_prob) %>%
      add_alternative("probability mass")

    dplyr::bind_rows(out_prob, out_cum, out_revcum)

  } else if (length(vars)==2) {

    # collect some information about the model
    term <- stringr::str_split(fit$terms, "~")[[3]]
    fx <- effects::effect(term, fit)

    modmat <- fx$model.matrix
    intmat <- matrix(rep(0, length(fit$zeta)^2), nrow=length(fit$zeta))
    diag(intmat) <- 1

    lvls <- fx$variables[[1]]$levels
    zetas <- names(fit$zeta)

    # helper function for computing ci's
    compute_confint <- function(fit, hypothesis) {

      beta <- c(-stats::coef(fit), fit$zeta)
      vc <- stats::vcov(fit)
      vc[1,] <- 	-vc[1,]
      vc[,1] <- 	-vc[,1]

      mean <- hypothesis %*% beta
      se <- sqrt(hypothesis %*% vc %*% hypothesis)
      lower <- mean - stats::qnorm(.975) * se
      upper <- mean + stats::qnorm(.975) * se
      c(mean, lower, upper)
    }

    # compute cumulative probability at each boundary
    out <- data.frame(alternative = rep(zetas, each=length(lvls)), predictor = rep(lvls, times=length(zetas)), fit=NA, lower=NA, upper=NA)
    k <- 1
    for (j in 1:nrow(intmat)) {
      for (i in 1:nrow(modmat)) {
        hypothesis <- c(modmat[i,], intmat[j,])
        out[k, 3:5] <- compute_confint(fit, hypothesis) %>% stats::plogis()
        k <- k+1
      }
    }

    # compute cumulative probability for each category
    nalt <- length(fit$lev)
    nlev <- length(lvls)
    cumulative <- data.frame(alternative=rep(fit$lev, each=length(lvls)),  predictor = rep(lvls, times=length(fit$lev)), fit=1, lower=1, upper=1)

    ((nalt-1) * nlev)
    cumulative[1:((nalt-1) * nlev),3:5] <- out[,3:5]

    # compute reverse cumulative probability for each category
    reverse_cumulative <- data.frame(alternative=rep(fit$lev, each=length(lvls)),  predictor = rep(lvls, times=length(fit$lev)), fit=1, lower=1, upper=1)
    reverse_cumulative[(nlev+1):(nalt*nlev),3:5] <- 1-out[,3:5]
    tmp <- reverse_cumulative$upper
    reverse_cumulative$upper <- reverse_cumulative$lower
    reverse_cumulative$lower <- tmp

    # compute probability mass
    probability_mass <- fit %>% confint_multinomial_ordinal_effect()

    # format output
    cumulative <- dplyr::rename(cumulative, !!term := "predictor")
    reverse_cumulative <- dplyr::rename(reverse_cumulative, !!term := "predictor")

    probability_mass$probability <- "probability mass"
    cumulative$probability <- "cumulative probability"
    reverse_cumulative$probability <- "reverse cumulative probability"

    output <- dplyr::bind_rows(probability_mass, cumulative, reverse_cumulative)
    output %>% dplyr::select(.data$probability, dplyr::everything())

  } else {
    stop("confint_model expects a model with maximally one predictor")
  }
}


#' Compute effects for a multinomial/ordinal model with one predictor
#'
#' @param fit the fitted model
#'
#' @return effect estimates with confidence interval
#' @keywords internal
confint_multinomial_ordinal_effect <- function(fit) {

  term <- stringr::str_split(fit$terms, "~")[[3]]
  fx <- effects::effect(term, fit)

  addx <- function(x) x %>% dplyr::mutate(X=paste0("X", 1:dplyr::n()))
  addy <- function(x) x %>% dplyr::mutate(Y=paste0("Y", 1:dplyr::n()))
  set_names <- function(x) {
    stats::setNames(x, paste0("Y", 1:ncol(x)))
  }

  fxx <- fx$x %>% addx()
  fyy <- data.frame(alternative=fx$y) %>% addy()

  longform <- function(x) {
    x %>%
      data.frame() %>%
      set_names() %>%
      addx() %>%
      tidyr::gather("Y", "value", -.data$X)
  }

  fxf <- fx$prob %>% longform() %>% dplyr::rename(fit=.data$value)
  fxl <- fx$lower.prob %>% longform() %>% dplyr::rename(lower=.data$value)
  fxu <- fx$upper.prob %>% longform() %>% dplyr::rename(upper=.data$value)

  fxf %>%
    dplyr::inner_join(fxl, by = c("X", "Y")) %>%
    dplyr::inner_join(fxu, by = c("X", "Y")) %>%
    dplyr::inner_join(fxx, by = "X") %>%
    dplyr::inner_join(fyy, by = "Y") %>%
    dplyr::arrange(.data$Y, .data$X) %>%
    dplyr::select(.data$alternative, tidyselect::matches(term),
                  .data$fit, .data$lower, .data$upper) %>%
    dplyr::mutate(alternative=forcats::fct_inorder(.data$alternative))
}

