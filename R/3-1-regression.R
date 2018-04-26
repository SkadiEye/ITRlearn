###########################################################
### Build a regression model

#' @name reg
#' @rdname reg
#'
#' @title Regression Methods
#'
#' @description A collection of functions to build regression models.
#'
#' @details
#'
#' These \code{reg} methods construct a single regression model given a \code{regObj}
#'  and a set of parameters. This function returns a \code{ModelObj}.
#'
#' @param object A \code{regObj} object, used as a training set.
#' @param valid A \code{wClsObj} object, used as a validation set.
#' @param ... Arguments passed to the function being called.
#'
#' @return Returns a \code{ModelObj} object.
#'
#' @seealso
#' \code{\link{reg}}\cr
#' \code{\link{wcls}}\cr
#' \code{\link{cvGrid}}\cr
#' \code{\link{cvKfold}}\cr
#' \code{\link{workflow}}\cr
NULL

#' @rdname reg
#' @export
setGeneric("regLASSO",
           function(object, valid = NULL, ...) standardGeneric("regLASSO")
)

#' @rdname reg
#' @export
setGeneric("regSVR",
           function(object, valid = NULL, ...) standardGeneric("regSVR")
)

#' @rdname reg
#' @export
setGeneric("regRF",
           function(object, valid = NULL, ...) standardGeneric("regRF")
)

#' @rdname reg
#' @export
setGeneric("regANN",
           function(object, valid = NULL, ...) standardGeneric("regANN")
)

#' @rdname reg
#' @export
setGeneric("regDNN",
           function(object, valid = NULL, ...) standardGeneric("regDNN")
)

###########################################################
### Build a regression model

#' Workhorse for reg Methods
#'
#' Used as a back-end wrapper for creating new reg methods.
#'
#' @inheritParams reg
#' @param regMethod A function call unique to that reg method.
#' @return Returns an \code{ModelObj} object.
#'
#' @export
REG_ <- function(object, valid, regMethod, ...) {

  train.X <- as.matrix(object@X)
  train.Y <- object@Y
  rownames(train.X) <- NULL

  model <- do.call("regMethod", list(train.X, train.Y, valid, ...))

  modelObj <- methods::new("ModelObj",
                           model = model,
                           model.type = "Continuous")

  return(modelObj)
}

#' @rdname reg
#' @section Methods (by generic):
#' \code{regLASSO:} Method to build a regression model using glmnet::glmnet.
#' @importFrom glmnet glmnet
#' @export
setMethod("regLASSO", "RegObj",
          function(object, valid, ...) {

            REG_(object, valid,
                regMethod = function(train.X, train.Y, valid, ...) {

                  args <- append(list("x" = train.X, "y" = train.Y), list(...))
                  args <- appendArg(args, "family", "gaussian", 1)
                  args <- appendArg(args, "lambda", 0.001, 0)
                  do.call(glmnet::glmnet, args)
                }, ...)
          }
)

#' @rdname reg
#' @section Methods (by generic):
#' \code{regSVR:} Method to build a regression model using e1071::svm.
#' @importFrom e1071 svm
#' @export
setMethod("regSVR", "RegObj",
          function(object, valid, ...) {

            REG_(object, valid,
                regMethod = function(train.X, train.Y, valid, ...) {

                  args <- append(list("x" = train.X, "y" = train.Y), list(...))
                  args <- appendArg(args, "cross", 0, 1)
                  do.call(e1071::svm, args)
                }, ...)
          }
)

#' @rdname reg
#' @section Methods (by generic):
#' \code{regRF:} Method to build a regression model using randomForest::randomForest.
#' @importFrom randomForest randomForest
#' @export
setMethod("regRF", "RegObj",
          function(object, valid, ...) {

            REG_(object, valid,
                regMethod = function(train.X, train.Y, valid, ...) {

                  args <- append(list("x" = train.X, "y" = train.Y), list(...))
                  do.call(randomForest::randomForest, args)
                }, ...)
          }
)

#' @rdname reg
#' @section Methods (by generic):
#' \code{regANN:} Method to build a regression model using nnet::nnet.
#' @importFrom nnet nnet
#' @export
setMethod("regANN", "RegObj",
          function(object, valid, ...) {

            REG_(object, valid,
                regMethod = function(train.X, train.Y, valid, ...) {

                  args <- append(list("x" = train.X, "y" = train.Y), list(...))
                  args <- appendArg(args, "linout", TRUE, 0)
                  args <- appendArg(args, "skip", TRUE, 0)
                  args <- appendArg(args, "size", 100, 0)
                  args <- appendArg(args, "MaxNWts", 10**5, 0)
                  args <- appendArg(args, "trace", FALSE, 0)
                  do.call(nnet::nnet, args)
                }, ...)
          }
)

#' @rdname reg
#' @section Methods (by generic):
#' \code{regDNN:} Method to build a deep neural network regression model using nn.regresser function.
#' @export
setMethod("regDNN", "RegObj",
          function(object, valid, ...) {

            REG_(object, valid,
                regMethod = function(train.X, train.Y, valid, ...) {

                  train <- dnnet::importDnnet(x = train.X, y = train.Y)
                  validate <- dnnet::importDnnet(x = valid@X, y = valid@Y)
                  # validate <- cbind(valid@Y, as.matrix(valid@X))
                  args <- append(list("train" = train, "validate" = validate), list(...))
                  do.call(dnnet::dnnet, args)
                }, ...)
          }
)

