#' Confidence intervals around the intercept of a linear model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_linear_model <- function(fit) {

  stopifnot(class(fit)=="lm")

  stats::predict(fit, newdata=data.frame(1), se=TRUE, interval="confidence") %>%
    purrr::pluck(1) %>%
    data.frame() %>%
    dplyr::rename(lower=.data$lwr, upper=.data$upr)
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

  stats::predict(fit,  newdata=data.frame(1), type="link", se.fit=TRUE) %>%
    data.frame() %>%
    dplyr::mutate(lower=.data$fit-1.96*.data$se.fit) %>%
    dplyr::mutate(upper=.data$fit+1.96*.data$se.fit) %>%
    dplyr::select(-'se.fit', -'residual.scale') %>%
    dplyr::mutate_all(stats::plogis)
}


#' Confidence intervals around the intercept of a multinomial model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_multinomial_model <- function(fit) {

  stopifnot(all(class(fit)==c("multinom", "nnet")))

  # use the delta method
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

  rownames(out) <- fit$lev
  out
}


#' Confidence intervals around the intercept of an ordinal model
#'
#' @param fit the fitted model
#'
#' @return intercept estimate with confidence interval
#' @export
confint_ordinal_model <- function(fit) {

  stopifnot(class(fit)=="polr")

  # use the delta method
  # polr intercepts are stored in zeta, not in beta
  # we need a wrapper around the delta method
  # that takes this into account
  mydeltamethod <- function(fit, g.) {
      cf <- fit$zeta
      vcv <- stats::vcov(fit)
      names(cf) <- paste("b", 1:length(cf), sep="")
      rownames(vcv) <- colnames(vcv) <- names(cf)
      car::deltaMethod(cf, g., vcov.=vcv)
  }

  # compute functions for cumulative, degressive and mass probabilities
  ncat <- length(fit$zeta)
  parameterNames <- paste0("b", 1:ncat)

  g_cum <- vector("character", ncat)
  for(i in 1:length(g_cum)) {
    g_cum[i] <- paste0("exp(", parameterNames[i], ")/(1+exp(", parameterNames[i], "))")
  }

  g_revcum <- vector("character", ncat)
  for(i in 1:length(g_revcum)) {
    g_revcum[i] <- paste0("1-", g_cum[i])
  }

  g_prob <- vector("character", ncat+1)
  g_prob[1] <- g_cum[1]
  for(i in 2:(length(g_prob)-1)) {
    g_prob[i] <- paste0(g_cum[i], " - ", g_cum[i-1])
  }
  g_prob[length(g_cum)+1] <- paste0(1, " - ", g_cum[length(g_cum)])

  # apply the delta method on the functions
  out_cum <- list()
  for(i in 1:length(g_cum)) {
    out_cum[[i]] <- mydeltamethod(fit, g_cum[i])
  }

  out_revcum <- list()
  for(i in 1:length(g_revcum)) {
    out_revcum[[i]] <- mydeltamethod(fit, g_revcum[i])
  }

  out_prob <- list()
  for(i in 1:length(g_prob)) {
    out_prob[[i]] <- mydeltamethod(fit, g_prob[i])
  }

  out_cum <- dplyr::bind_rows(out_cum) %>%
    dplyr::rename(fit=.data$Estimate) %>%
    dplyr::select(-.data$SE) %>%
    dplyr::rename(lower=.data$`2.5 %`) %>%
    dplyr::rename(upper=.data$`97.5 %`) %>%
    tibble::add_row(fit=1, lower=1, upper=1)

  out_revcum <- dplyr::bind_rows(out_revcum) %>%
    dplyr::rename(fit=.data$Estimate) %>%
    dplyr::select(-.data$SE) %>%
    dplyr::rename(lower=.data$`2.5 %`) %>%
    dplyr::rename(upper=.data$`97.5 %`) %>%
    tibble::add_row(fit=1, lower=1, upper=1, .before=1)

  out_prob <- dplyr::bind_rows(out_prob) %>%
    dplyr::rename(fit=.data$Estimate) %>%
    dplyr::select(-.data$SE) %>%
    dplyr::rename(lower=.data$`2.5 %`) %>%
    dplyr::rename(upper=.data$`97.5 %`)

  rownames(out_cum) <- fit$lev
  rownames(out_revcum) <- fit$lev
  rownames(out_prob) <- fit$lev

  list("probability"=out_prob, "cumulative probability"=out_cum, "reverse cumulative probability"=out_revcum)
}


#' Confidence intervals around the intercept of a logistic mixed effects model
#'
#' @param fit the fitted model
#'
#' @return category estimates with confidence interval
#' @export
confint_mixed_model <- function(fit) {

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

  newdata %>%
    dplyr::select(-.data$y, -.data$id) %>%
    data.frame(row.names=NULL) %>%
    tibble::column_to_rownames(var="alternative")
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

  newdata <- fit$data %>% dplyr::filter(.data$id==1)

  out <- stats::predict(fit, newdata, type="link", se.fit=TRUE) %>%
    data.frame() %>%
    dplyr::mutate(lower=.data$fit-1.96*.data$se.fit) %>%
    dplyr::mutate(upper=.data$fit+1.96*.data$se.fit) %>%
    dplyr::select(-'se.fit', -'residual.scale') %>%
    dplyr::mutate_all(stats::plogis)

  rownames(out) <- newdata$alternative
  out
}



