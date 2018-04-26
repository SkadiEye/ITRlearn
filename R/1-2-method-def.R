###########################################################
### Define generic functions: show
### Other generic functions print, '[', '$', plot are currently not available

#' Print first several elements of a vector in a slot of an object.
#'
#' \code{show} method call this function to print a vector.
#'
#' @param object The object.
#' @param slot.name The slot name storing the vector.
#' @param k First k elements will be printed.
#'
#' @importFrom methods show
#' @importFrom methods slot
#'
#' @export
show.vec <- function(object, slot.name, k = 10) {

  if(missing(slot.name)){

    vec <- object
    slot.name <- ""
  } else
    vec <- methods::slot(object, slot.name)
  cat("@"); cat(slot.name); cat(": ["); cat(length(vec))
  if(!is.null(vec))
    cat("]",
        as.character(vec[1:min(k, length(vec))]),
        ifelse(length(vec) > k, "... ", " "), "\n")
  else
    cat("NULL")
}

#' Print first several columns of a data.frame in a slot of an object.
#'
#' \code{show} method call this function to print a data.frame.
#'
#' @param object The object.
#' @param slot.name The slot name storing the data.frame
#' @param k First k columns will be printed.
#' @param l First l rows will be printed.
#' @importFrom methods show
#'
#' @export
show.df <- function(object, slot.name,
                    k = ncol(methods::slot(object, slot.name)),
                    l = nrow(methods::slot(object, slot.name))) {

  df <- methods::slot(object, slot.name)
  cat("@"); cat(slot.name); cat(": \n")
  if(ncol(df) > k) {

    print(cbind(data.frame(df,
                           stringsAsFactors = FALSE,
                           check.names = FALSE,
                           fix.empty.names = FALSE)[1:min(l, nrow(df)), 1:k],
                data.frame("..." = rep("...", min(l, nrow(df))), stringsAsFactors = FALSE)))
  } else
    print(data.frame(df,
                     stringsAsFactors = FALSE,
                     check.names = FALSE,
                     fix.empty.names = FALSE))[1:min(l, nrow(df)), ]

  if(nrow(df) > l)
    cat("... \n")
}

#################################
#### TrtDataObj class

#' @describeIn TrtDataObj Method to show \code{TrtDataObj} object.
#'
#' @param object A \code{TrtDataObj} object.
#'
#' @export
setMethod(
  "show",
  "TrtDataObj",
  function(object) {

    show.vec(object, "trtResp", 6)
    show.vec(object, "trtLabl", 6)
    show.vec(object, "trtTrue", 6)
    show.vec(object, "trtLevl", 6)
    show.df(object, "X", 6, 6)
    cat("@X:",
        nrow(object@X), "subjects by", ncol(object@X), "variables. \n")
    show.vec(object, "sample.weight", 6)
    show.vec(object, "sample.inclsn", 6)
  }
)

#################################
#### TrtDataIdeal class

#' @describeIn TrtDataIdeal Method to show \code{TrtDataIdeal} object.
#'
#' @param object A \code{TrtDataIdeal} object.
#'
#' @export
setMethod(
  "show",
  "TrtDataIdeal",
  function(object) {

    show.vec(object, "trtResp1", 6)
    show.vec(object, "trtResp2", 6)
    show.vec(object, "label", 6)
    show.df(object, "X", 6, 6)
    cat("@X:",
        nrow(object@X), "subjects by", ncol(object@X), "variables. \n")
  }
)

#################################
#### wClsObj class

#' @describeIn wClsObj Method to show \code{wClsObj} object.
#'
#' @param object A \code{wClsObj} object.
#'
#' @export
setMethod(
  "show",
  "wClsObj",
  function(object) {

    show.vec(object, "weight", 6)
    show.vec(object, "Y", 6)
    show.vec(object, "Y.lvl", 6)
    show.df(object, "X", 6, 6)
    cat("@X:",
        nrow(object@X), "subjects by", ncol(object@X), "variables. \n")
  }
)

#################################
#### RegObj class

#' @describeIn RegObj Method to show \code{RegObj} object.
#'
#' @param object A \code{RegObj} object.
#'
#' @export
setMethod(
  "show",
  "RegObj",
  function(object) {

    show.vec(object, "Y", 6)
    show.vec(object, "Y.true", 6)
    show.df(object, "X", 6, 6)
    cat("@X:",
        nrow(object@X), "subjects by", ncol(object@X), "variables. \n")
  }
)

#################################
#### ModelObj class

#' @describeIn ModelObj Method to show \code{ModelObj} object.
#'
#' @param object A \code{ModelObj} object.
#'
#' @export
setMethod("show", "ModelObj",
          function(object) {

            show.vec(object, "model.type")

            cat("Model class:", class(object@model))
          }
)

#################################
#### ITRObj class

#' @describeIn ITRObj Method to show \code{ITRObj} object.
#'
#' @param object A \code{ITRObj} object.
#'
#' @export
setMethod("show", "ITRObj",
          function(object) {

            show.vec(object, "model.type")
            show.vec(object, "label")
            cat("@model: \n")
            show(object@model)
          }
)

#################################
#### PredObj class

#' @describeIn PredObj Method to show \code{PredObj} object.
#'
#' @param object A \code{PredObj} object.
#'
#' @export
setMethod("show", "PredObj",
          function(object) {

            cat("@pred: \n")
            print(object@pred)
          }
)

#################################
#### cvGrid class

#' @describeIn cvGrid Method to show \code{cvGrid} object.
#'
#' @param object A \code{cvGrid} object.
#'
#' @export
setMethod("show", "cvGrid",
          function(object) {

            print(object@performances)
          }
)

#################################
#### ModelEnsembleObj class

#' @describeIn ModelEnsembleObj Method to show \code{ModelEnsembleObj} object.
#'
#' @param object A \code{ModelEnsembleObj} object.
#'
#' @export
setMethod("show", "ModelEnsembleObj",
          function(object) {

            show.vec(object, "model.type")
            show.vec(object, "loss", 6)
            show.vec(object, "keep", 6)

            cat("Model class:", class(object@model.list[[1]]))
          }
)

#################################
#### cvKfold class

#' @describeIn cvKfold Method to show \code{cvKfold} object.
#'
#' @param object A \code{cvKfold} object.
#'
#' @export
setMethod("show", "cvKfold",
          function(object) {

            cat("@performances: \n")
            print(object@performances)

            show.df(object, "v.score", 6)
            show.df(object, "pred.error", 6)
          }
)

#################################
#### workflow class

#' @describeIn workflow Method to show \code{workflow} object.
#'
#' @param object A \code{workflow} object.
#'
#' @export
setMethod("show", "workflow",
          function(object) {

            cat("@performances: \n")
            print(object@performances)
          }
)



