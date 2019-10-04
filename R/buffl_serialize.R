#' Serialize an R dataframe
#'
#' @param x an R dataframe
#'
#' @return a Json string that holds the R dataframe with all its attributes
#' @export
json_serialize <- function(x) {

  serialize_variable <- function(x) {

    classx <- class(x)[1]

    if (!classx %in% c("integer", "numeric", "ordered", "factor", "checkbox")) {
      stop("unknown variable type")
    }

    if (classx %in% c("ordered", "factor")) {
      levelsx <- levels(x)
      valuesx <- as.numeric(x)
    } else if (classx %in% "checkbox") {
      levelsx <- attr(x, "levels")
      valuesx <- as.list(x)
    } else {
      levelsx <- NULL
      valuesx <- as.numeric(x)
    }


    labelx <- attr(x, "label")
    label0x <- attr(x, "label0")

    list(class=classx, values=valuesx, levels=levelsx, label=labelx, label0=label0x)
  }

  x <- as.list(x)
  for (i in names(x)) {
    x[[i]] <- serialize_variable(x[[i]])
  }
  jsonlite::toJSON(x)
}



#' Unserialize a Json string
#'
#' @param x Json string
#'
#' @return an R dataframe reconstructed from the Json string
#' @export
json_unserialize <- function(x) {

  unserialize_variable <- function(x) {

    classx <- x$class
    valuesx <- x$values
    levelsx <- x$levels
    labelx <- x$label
    label0x <- x$label0

    if (!classx %in% c("integer", "numeric", "ordered", "factor", "checkbox")) {
      stop("unknown variable type")
    }
    if (classx %in% c("ordered", "factor")) {
      x <- factor(valuesx, labels=levelsx, ordered = classx=="ordered")
    } else if (classx %in% "checkbox") {
      x <- as.character(valuesx)
      attr(x, "levels") <- levelsx
      attr(x, "class") <- "checkbox"
    } else {
      x <- valuesx
    }

    if(length(labelx)>0) {
      attr(x, "label") <- labelx
    }

    if(length(label0x)>0) {
      attr(x, "label0") <- label0x
    }

    x
  }
  x <- jsonlite::fromJSON(x)

  for(i in names(x)) {
    x[[i]] <- unserialize_variable(x[[i]])
  }


  tibble::as_tibble(data.frame(x, stringsAsFactors=FALSE))
}


#' Convert a checkbox function to a data.frame
#'
#' We need this function as R only wants to enter known variable types
#' into a data.frame
#'
#' @param x a checkbox question
#'
#' @return a data.frame with the checkbox question and its attributes
#' @method as.data.frame checkbox
#' @export
as.data.frame.checkbox <- function(x, ...) {

  nm <- paste(deparse(substitute(x), width.cutoff = 500L),
              collapse = " ")

  xlevels <- attr(x, "levels")
  xlabel <- attr(x, "label")
  x <- as.character(x)
  x <- data.frame(x, stringsAsFactors=FALSE)
  attr(x$x, "levels") <- xlevels
  attr(x$x, "label") <- xlabel
  attr(x$x, "class") <- "checkbox"
  names(x) <- names(nm)
  x
}

