getSplit <- function(split, n) {

  if(is.numeric(split) && length(split) == 1 && split < 1)
    split <- sample(n, floor(n * split))

  if(is.numeric(split) && length(split) == 1 && split > 1)
    split <- 1:split

  if(is.character(split) && length(split) == 1 && split == "bootstrap")
    split <- sample(n, replace = TRUE)

  split
}

###########################################################
### Define generic functions: splitData

#' @name splitData
#' @rdname splitData
#'
#' @title Split Data
#'
#' @description Split an object into a training set and a validation set.
#'
#' @details
#'
#' A function to split the objects into a list of two objects of the same type:
#'  one named trainObj and the other named validObj.
#'
#' @param object An object to be split.
#' @param split A character, numeric variable or a numeric vector declaring a way to split
#' the object. If it's number between 0 and 1, all samples will be split into two subsets
#' randomly, with the trainObj containing such proportion of all samples abd validObj containing
#' the rest. If split is a character and is "bootstrap", the trainObj will be a bootstrap sample
#' of the original data set and the validObj will contain out-of-bag samples. If split is a vector
#' of integers, the trainObj will contain samples whose indice are in the vector, and validObj will
#' contain the rest.
#'
#' @return Returns a list of two objects of the input type, trainObj and validObj, and the indice of the trainObj.
#'
#' @seealso
#' \code{\link{TrtDataObj-class}}
#' \code{\link{TrtDataIdeal-class}}
#' \code{\link{wClsObj-class}}
#' \code{\link{RegObj-class}}
NULL

#' @rdname splitData
#' @export
setGeneric("splitData",
           function(object, split) standardGeneric("splitData")
)

#' @rdname splitData
#' @export
setMethod("splitData", "TrtDataObj",
          function(object, split) {

            split <- getSplit(split, dim(object@X)[1])

            trainObj <- object
            trainObj@X <- object@X[split, ]
            trainObj@trtResp <- object@trtResp[split]
            trainObj@trtLabl <- object@trtLabl[split]
            trainObj@trtTrue <- object@trtTrue[split]
            trainObj@sample.weight <- object@sample.weight[split]
            trainObj@sample.inclsn <- object@sample.inclsn[split]

            validObj <- object
            validObj@X <- object@X[-split, ]
            validObj@trtResp <- object@trtResp[-split]
            validObj@trtLabl <- object@trtLabl[-split]
            validObj@trtTrue <- object@trtTrue[-split]
            validObj@sample.weight <- object@sample.weight[-split]
            validObj@sample.inclsn <- object@sample.inclsn[-split]

            return(list(trainObj = trainObj, validObj = validObj, split = split))
          }
)

#' @rdname splitData
#' @export
setMethod("splitData", "TrtDataIdeal",
          function(object, split) {

            split <- getSplit(split, dim(object@X)[1])

            trainObj <- object
            trainObj@X <- object@X[split, ]
            trainObj@trtResp1 <- object@trtResp1[split]
            trainObj@trtResp2 <- object@trtResp2[split]

            validObj <- object
            validObj@X <- object@X[-split, ]
            validObj@trtResp1 <- object@trtResp1[-split]
            validObj@trtResp2 <- object@trtResp2[-split]

            return(list(trainObj = trainObj, validObj = validObj, split = split))
          }
)

#' @rdname splitData
#' @export
setMethod("splitData", "wClsObj",
          function(object, split) {

            split <- getSplit(split, dim(object@X)[1])

            trainObj <- object
            trainObj@X <- object@X[split, ]
            trainObj@Y <- object@Y[split]
            trainObj@weight <- object@weight[split]

            validObj <- object
            validObj@X <- object@X[-split, ]
            validObj@Y <- object@Y[-split]
            validObj@weight <- object@weight[-split]

            return(list(trainObj = trainObj, validObj = validObj, split = split))
          }
)

#' @rdname splitData
#' @export
setMethod("splitData", "RegObj",
          function(object, split) {

            split <- getSplit(split, dim(object@X)[1])

            trainObj <- object
            trainObj@X <- object@X[split, ]
            trainObj@Y <- object@Y[split]

            validObj <- object
            validObj@X <- object@X[-split, ]
            validObj@Y <- object@Y[-split]

            return(list(trainObj = trainObj, validObj = validObj, split = split))
          }
)
