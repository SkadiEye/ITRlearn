###########################################################
#### Import data with treatment, covariates and response data

#' Import Data to create a \code{TrtDataObj} object.
#'
#' A user-friendly function to create a \code{TrtDataObj} object.
#'
#' This function takes data set and generate an \code{TrtDataObj} in a flexible way:
#'
#'  X can be either a \code{matrix} of a \code{data.frame}, of which each column is a numeric predictor
#'  of the data set and each row is all predictor values for a sample.
#'  X should contain all samples/variables.
#'
#' @param X A \code{matrix} or \code{data.frame} containing all samples/variables. It has to be \code{numeric}
#' and cannot be left blank. Any variable with missing value will be removed.
#' @param trtResp A \code{numeric} vector, indicating the response under assigned treatment. If it's left blank, t
#' his object could not be used to build models but could be used in \code{predict} function.
#' @param trtLabl A \code{factor} vector, the sample treatment labels.
#' @param trtLevl A \code{character} vector of length two, the unique treatment labels. The default is
#' \code{levels(trtLabl)}.
#' @param trtTrue A \code{factor} vector, true optimal treatment labels. Left missing if not provided.
#' @param sample.weight A \code{numeric} vector, sample weight used to evaluate the performance.
#' @param sample.inclsn A \code{logical} vector, indicating whether a sample is used to evaluate the performance.
#'
#' @return An \code{TrtDataObj} object.
#'
#' @importFrom methods new
#'
#' @seealso
#' \code{\link{TrtDataObj-class}}
#' @export
importData <- function(X,
                       trtResp = NULL,
                       trtLabl = NULL,
                       trtLevl = levels(trtLabl),
                       trtTrue = NULL,
                       sample.weight = rep(1, dim(X)[1]),
                       sample.inclsn = rep(TRUE, dim(X)[1])) {

  if (missing(X))
    stop("Please include input variables. ")

  if (!is.numeric(trtResp))
    stop("The treatment response vector must be numeric. ")
  if (!is.data.frame(X) && !is.matrix(X) && !is.numeric(X)) {
    stop("The input variables must be one of the following types: numeric, matrix or data.frame")
  } else {

    if (is.null(colnames(X)))
      colnames(X) <- paste("V", 1:dim(X)[2], sep='')

    if (is.null(rownames(X)))
      rownames(X) <- paste("S", 1:dim(X)[1], sep='')

    if (sum(duplicated(colnames(X))))
      stop("All variable names must be unique. ")

    X <- as.data.frame(X)
    na.check = colSums(is.na(as.matrix(X)))

    if(sum(na.check) > 0) {

      print("The following variables are deleted due to missing values, ")
      print(paste(colnames(X)[na.check > 0]))
    }
    X <- X[, na.check == 0]
  }

  if(!is.null(trtLabl) && levels(trtLabl) != trtLevl)
    trtLabl <- factor(trtLabl, levels = trtLevl)

  if(!is.null(trtTrue))
    levels(trtTrue) <- levels(trtLabl)

  methods::new("TrtDataObj", X = X, trtResp = trtResp, trtLabl = trtLabl, trtLevl = trtLevl,
               trtTrue = trtTrue, sample.weight = sample.weight, sample.inclsn = sample.inclsn)
}

###########################################################
#### Import data with covariates and responses under both treatments

#' Import Data to create a \code{TrtDataIdeal} object.
#'
#' A user-friendly function to create a \code{TrtDataIdeal} object.
#'
#' This function takes data set and generate an \code{TrtDataIdeal} in a flexible way:
#'
#'  X can be either a \code{matrix} of a \code{data.frame}, of which each column is a numeric predictor
#'  of the data set and each row is all predictor values for a sample.
#'  X should contain all samples/variables.
#'
#' @param X A \code{matrix} or \code{data.frame} containing all samples/variables. It has to be \code{numeric}
#' and cannot be left blank. Any variable with missing value will be removed.
#' @param trtResp1 A \code{numeric} vector, indicating the response under the first treatment.
#' @param trtResp2 A \code{numeric} vector, indicating the response under the second treatment.
#' @param label A \code{character} vector of length two, the unique treatment labels. The default is
#' \code{levels(trtLabl)}.
#'
#' @return An \code{TrtDataIdeal} object.
#'
#' @seealso
#' \code{\link{TrtDataIdeal-class}}
#' @export
impoorIdeal <- function(X, trtResp1, trtResp2, label) {

  if (!is.numeric(trtResp1) || !is.numeric(trtResp2))
    stop("The treatment response vectors must be numeric. ")
  if (!is.data.frame(X) && !is.matrix(X) && !is.numeric(X)) {
    stop("The input variables must be one of the following types: numeric, matrix or data.frame")
  } else {

    if (is.null(colnames(X)))
      colnames(X) <- paste("V", 1:dim(X)[2], sep='')

    if (is.null(rownames(X)))
      rownames(X) <- paste("S", 1:dim(X)[1], sep='')

    if (sum(duplicated(colnames(X))))
      stop("All variable names must be unique. ")

    X <- as.data.frame(X)
    na.check = colSums(is.na(as.matrix(X)))

    if(sum(na.check) > 0) {

      print("The following variables are deleted due to missing values, ")
      print(paste(colnames(X)[na.check > 0]))
    }
    X <- X[, na.check == 0]
  }

  methods::new("TrtDataIdeal", trtResp1 = trtResp1, trtResp2 = trtResp2, X = X, label = label)
}

###########################################################
#### Randomly assgin treatment and generate tratment data object

#' @name generateTrtData
#' @rdname generateTrtData
#'
#' @title Randomly assgin treatment and generate tratment data object
#'
#' @description From \code{TrtDataIdeal} object, randomly assign treatment labels to all samples and
#' generate a \code{TrtDataObj} object.
#'
#' @param object A \code{TrtDataIdeal} object.
#' @param p.case A number between 0 and 1, indicating the probability to assign the first treatment.
#' @param inc.cutoff A positive number, indicating the cutoff of inclusion.
#' @param epsilon A random variable with mean zero and variance epsilon^2 will be added to the treatment response.
#' @param ... Other possible parameters passed to this function.
#'
#' @return An \code{TrtDataObj} object.
#'
#' @seealso
#' \code{\link{TrtDataObj-class}}
#' \code{\link{TrtDataIdeal-class}}
#'
#' @export
setGeneric("generateTrtData",
           function(object, p.case = 0.5, inc.cutoff = 0, epsilon = 1, ...) standardGeneric("generateTrtData")
)

#' @rdname generateTrtData
#' @section Methods (by generic):
#' \code{generateTrtData:} Method to randomly assgin treatment and generate tratment data object
#'
#' @importFrom stats rbinom
#' @importFrom stats rnorm
#'
#' @export
setMethod("generateTrtData",
          "TrtDataIdeal",
          function(object, p.case, inc.cutoff, epsilon, ...) {

            A <- stats::rbinom(length(object@trtResp1), 1, p.case)
            trtResp <- ifelse(A, object@trtResp1, object@trtResp2) + stats::rnorm(length(object@trtResp1)) * epsilon
            trtTrue <- factor(object@label[(object@trtResp1 < object@trtResp2) + 1])
            trtLabl <- factor(object@label[2 - A])
            sample.weight <- abs(object@trtResp1 - object@trtResp2)
            sample.inclsn <- abs(object@trtResp1 - object@trtResp2) > inc.cutoff

            methods::new("TrtDataObj", X = object@X, trtResp = trtResp, trtLabl = trtLabl, trtTrue = trtTrue,
                         trtLevl = object@label, sample.weight = sample.weight, sample.inclsn = sample.inclsn)
          }
)

