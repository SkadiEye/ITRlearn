###########################################################
### Build a weighted classification model

#' @name wcls
#' @rdname wcls
#'
#' @title Weighted Classification Methods
#'
#' @description A collection of functions to build weighted classification models.
#'
#' @details
#'
#' These \code{wcls} methods construct a single weighted classification model given a \code{wClsObj}
#'  and a set of parameters. This function returns a \code{ModelObj}.
#'
#' @param object A \code{wClsObj} object, used as a training set.
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
#'
#'  @examples
#'
#'
NULL

#' @rdname wcls
#' @export
setGeneric("wclskSVM",
           function(object, valid = NULL, ...) standardGeneric("wclskSVM")
)

#' @rdname wcls
#' @export
setGeneric("wclslSVM",
           function(object, valid = NULL, ...) standardGeneric("wclslSVM")
)

#' @rdname wcls
#' @export
setGeneric("wclsLASSO",
           function(object, valid = NULL, ...) standardGeneric("wclsLASSO")
)

#' @rdname wcls
#' @export
setGeneric("wclsANN",
           function(object, valid = NULL, ...) standardGeneric("wclsANN")
)

#' @rdname wcls
#' @export
setGeneric("wclsDNN",
           function(object, valid = NULL, ...) standardGeneric("wclsDNN")
)

###########################################################
### Build a weighted classification model

#' Workhorse for wcls Methods
#'
#' Used as a back-end wrapper for creating new wcls methods.
#'
#' @inheritParams wcls
#' @param clsMethod A function call unique to that wcls method.
#' @return Returns an \code{ModelObj} object.
#'
#' @export
wCLS_ <- function(object, valid, clsMethod, ...) {

    train.X <- as.matrix(object@X)
    train.Y <- factor(object@Y, levels = object@Y.lvl)
    weight <- object@weight
    rownames(train.X) <- NULL

    model <- do.call("clsMethod", list(train.X, train.Y, valid, weight, ...))

    modelObj <- methods::new("ModelObj",
                             model = model,
                             model.type = "Binary")

    return(modelObj)
}

#' @rdname wcls
#' @section Methods (by generic):
#' \code{wclskSVM:} Method to build a (non-linear) kernel SVM weighted classification model using w.k.svm function.
#' @export
setMethod("wclskSVM", "wClsObj",
          function(object, valid, ...) {

            wCLS_(object, valid,
                 clsMethod = function(train.X, train.Y, valid, weight, ...) {

                   args <- append(list("X" = train.X, "Y" = train.Y, "W" = weight), list(...))
                   do.call(w.k.svm, args)
                 }, ...)
          }
)

#' @rdname wcls
#' @section Methods (by generic):
#' \code{wclslSVM:} Method to build a linear SVM weighted classification model using w.l.svm function.
#' @export
setMethod("wclslSVM", "wClsObj",
          function(object, valid, ...) {

            wCLS_(object, valid,
                 clsMethod = function(train.X, train.Y, valid, weight, ...) {

                   args <- append(list("X" = train.X, "Y" = train.Y, "W" = weight), list(...))
                   do.call(w.l.svm, args)
                 }, ...)
          }
)

#' @rdname wcls
#' @section Methods (by generic):
#' \code{wclsLASSO:} Method to build a weighted classification model using glmnet::glmnet.
#' @importFrom glmnet glmnet
#' @export
setMethod("wclsLASSO", "wClsObj",
          function(object, valid, ...) {

            wCLS_(object, valid,
                 clsMethod = function(train.X, train.Y, valid, weight, ...) {

                   args <- append(list("x" = train.X, "y" = train.Y), list(...))
                   args <- appendArg(args, "family", ifelse(length(unique(train.Y)) <= 2, "binomial", "multinomial"), 1)
                   args <- appendArg(args, "lambda", 0.001, 0)
                   args <- appendArg(args, "weights", weight, 1)
                   do.call(glmnet::glmnet, args)
                 }, ...)
          }
)

#' @rdname wcls
#' @section Methods (by generic):
#' \code{wclsANN:} Method to build a weighted classification model using nnet::nnet.
#' @importFrom nnet nnet
#' @export
setMethod("wclsANN", "wClsObj",
          function(object, valid, ...) {

            wCLS_(object, valid,
                 clsMethod = function(train.X, train.Y, valid, weight, ...) {

                   args <- append(list("x" = train.X, "y" = nnet::class.ind(train.Y)), list(...))
                   args <- appendArg(args, "size", 100, 0)
                   args <- appendArg(args, "MaxNWts", 10**5, 0)
                   args <- appendArg(args, "trace", FALSE, 0)
                   args <- appendArg(args, "weights", weight, 1)
                   do.call(nnet::nnet, args)
                 }, ...)
          }
)

#' @rdname wcls
#' @section Methods (by generic):
#' \code{wclsDNN:} Method to build a deep neural network weighted classification model using nn.classifier function.
#' @export
setMethod("wclsDNN", "wClsObj",
          function(object, valid, ...) {

            wCLS_(object, valid,
                clsMethod = function(train.X, train.Y, valid, weight, ...) {

                  train <- dnnet::importDnnet(x = train.X, y = train.Y, w = weight)
                  validate <- dnnet::importDnnet(x = valid@X, y = valid@Y, w = valid@weight)
                  # validate <- data.frame(valid@Y, as.matrix(valid@X))
                  # w.validate <- valid@weight
                  args <- append(list("train" = train, "validate" = validate), list(...))
                  do.call(dnnet::dnnet, args)
                }, ...)
          }
)

