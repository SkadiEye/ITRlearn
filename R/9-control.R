require(ROCR)
require(e1071)
require(randomForest)
require(glmnet)
require(MASS)
require(nnet)

#' Append an argument on a list
#'
#' Append an argument on a list of arguments
#'
#' @param args The original argument list.
#' @param argName The name of the argument to add.
#' @param argValue The value of the argument to add.
#' @param forced a \code{logical} value indicating if the argument with the same
#'  name already existed, whether it should be added forcedly.
#'
#' @return An argument list.
#'
#' @export
appendArg <- function(args, argName, argValue, forced) {

  if((!argName %in% names(args)) && (!forced)) {

    # cat("Default behavior: setting", argName, "to", argValue, "...\n")
    args[[argName]] <- argValue
  } else if(forced) {

    # cat("Forced behavior: setting", argName, "to", argValue, "(cannot override) ...\n")
    args[[argName]] <- argValue
  }

  return(args)
}

#' Remove an argument on a list
#'
#' Remove an argument on a list of arguments
#'
#' @param args The original argument list.
#' @param argName The name of the argument to be remmoved. If the name does not exist,
#'  no argument will be removed.
#'
#' @return An argument list.
#'
#' @export
removeArg <- function(args, argName) {

  if(argName %in% names(args))
    args[[which(names(args) == argName)]] <- NULL
  return(args)
}

#' Manage \code{cvGrid} Arguments
#'
#' This function organizes \code{cvGrid} arguments passed to \code{reg} or \code{wcls} functions.
#'
#' @param machine A character string. The \code{reg} or \code{wcls} function to call.
#' @param ... Additional arguments passed to the \code{reg} or \code{wcls} function.
#' @return A list of arguments.
#' @seealso
#' \code{\link{cvGrid}}\cr
#' @export
gsCtrlPanel <- function(machine, ...) {

  list(machine = machine, ...)
}

#' Manage \code{ensemble} Arguments
#'
#' This function organizes \code{ensemble} arguments passed to \code{reg}, \code{wcls} or \code{cvGrid} functions.
#'
#' @param machine A character string. The \code{reg}, \code{wcls} or \code{cvGrid} function to call.
#' @param ... Additional arguments passed to the \code{reg}, \code{wcls} or \code{cvGrid} function.
#' @return A list of arguments.
#' @seealso
#' \code{\link{ensemble}}\cr
#' @export
esCtrlPanel <- function(machine, ...) {

  list(machine = machine, ...)
}

#' Manage \code{itr} Arguments
#'
#' This function organizes \code{itr} arguments passed to \code{reg} functions, \code{cvGrid}
#'  or \code{ensemble}.
#'
#' @param machine A character string, the \code{reg} function, \code{ensemble} or \code{cvGrid} to call.
#' @param ... Additional arguments passed to the \code{reg} function, \code{ensemble} or \code{cvGrid} function.
#' @return A list of arguments.
#' @seealso
#' \code{\link{itr}}\cr
#' @export
regCtrlPanel <- function(machine, ...) {

  list(machine, ...)
}

#' Manage \code{itr} Arguments
#'
#' This function organizes \code{itr} arguments passed to \code{wcls} functions, \code{cvGrid}
#'  or \code{ensemble}.
#'
#' @param machine A character string, the \code{wcls} function, \code{ensemble} or \code{cvGrid} to call.
#' @param ... Additional arguments passed to the \code{wcls} function, \code{ensemble} or \code{cvGrid} function.
#' @return A list of arguments.
#' @seealso
#' \code{\link{itr}}\cr
#' @export
wclsCtrlPanel <- function(machine, ...) {

  list(machine, ...)
}

#' Manage \code{cvKfold} or \code{workflow} Arguments
#'
#' This function organizes \code{cvKfold} or \code{workflow} arguments passed to \code{itr} functions.
#'
#' @param ITR A character string, the \code{itr} to call.
#' @param ... Additional arguments passed to the \code{itr} function function.
#' @return A list of arguments.
#' @seealso
#' \code{\link{cvKfold}}\cr
#' \code{\link{workflow}}\cr
#' @export
itrCtrlPanel <- function(ITR, ...) {

  list(ITR = ITR, ...)
}



