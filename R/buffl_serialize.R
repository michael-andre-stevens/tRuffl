#' Serialize an R dataframe
#'
#' @param x an R dataframe
#'
#' @return a Json string that holds the R dataframe with all its attributes
#' @export
json_serialize <- function(x) {

  serialize_variable <- function(x) {

    classx <- class(x)[1]

    if (!classx %in% c("integer", "numeric", "ordered", "factor")) {
      stop("unknown variable type")
    }

    if (classx %in% c("ordered", "factor")) {
      levelsx <- levels(x)
    } else {
      levelsx <- NULL
    }

    valuesx <- as.numeric(x)
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

    if (!classx %in% c("integer", "numeric", "ordered", "factor")) {
      stop("unknown variable type")
    }

    if (classx %in% c("ordered", "factor")) {
      x <- factor(valuesx, labels=levelsx, ordered = classx=="ordered")
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

  tibble::as_tibble(data.frame(x))
}

