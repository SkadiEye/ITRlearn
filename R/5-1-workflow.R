################################################
#### A workflow to Evaluate the Performance of a List of Methods

#' A workflow to Evaluate the Performance of a List of Methods
#'
#' In this procedure, if a validation set is provided, all methods in the list will be
#'  implemented once and the performance is evaluated using the validation set.
#'
#' If the validation set is missing, an n iterations of k-fold cross validation will
#'  be performed to evaluate each method.
#'
#' The final performance scores will be reported. And all final models will be saved.
#'
#' @param trainObj A \code{RegObj} or \code{wClsObj} containing the training set.
#' @param validObj A \code{RegObj} or \code{wClsObj} containing the validation set.
#' @param k.fold The number of folds in the cross validation.
#' @param n.iter The number of interations of k-fold cross validations.
#' @param cvCtrl.list A \code{list} of \code{list}s of arguments that specifies a regression or classification method.
#'  Each list of arguments is handled by \code{\link{itrCtrlPanel}}.
#' @param cutoff Cutoff used when predicting the current fold that not used in training the model, passed to \code{cutoff.calc} function.
#' @param unbalance.trt A character string that specifies if sampleing techniques should be applied to
#'  data with unbalanced numbers for each class.
#'
#' @return A \code{\link{workflow-class}} object.
#'
#' @seealso
#' \code{\link{cutoff.calc}}\cr
#' \code{\link{reg}}\cr
#' \code{\link{wcls}}\cr
#' \code{\link{cvGrid}}\cr
#' \code{\link{ensemble}}\cr
#' \code{\link{cvKfold}}\cr
#'
#' @export
workflow <- function(trainObj,
                     validObj = NULL,
                     k.fold = 10,
                     n.iter = 10,
                     cvCtrl.list,
                     unbalance.trt = c("None", "over", "under")[1],
                     cutoff = 0.5) {

  if(is.null(validObj)) {

    prob.array <- array(NA, dim = c(length(trainObj@trtLabl), 2, length(cvCtrl.list), n.iter))
    pred.array <- array(NA, dim = c(length(trainObj@trtLabl), length(cvCtrl.list), n.iter))
    v.score <- array(NA, dim = c(length(cvCtrl.list), dim(trainObj@X)[2], n.iter))
    pred.error <- array(NA, dim = c(length(cvCtrl.list), length(trainObj@trtLabl), n.iter))
    cutoff.list <- array(NA, dim = c(k.fold, length(cvCtrl.list), n.iter))

    for(i in 1:n.iter) {

      cat("-----------Iteration", i, "----------------------- \n")

      final.models = FALSE
      if(i == n.iter)
        final.models = TRUE

      cvmodl <- cvKfold(trainObj = trainObj,
                        k.fold = k.fold,
                        cvCtrl.list = cvCtrl.list,
                        unbalance.trt = unbalance.trt,
                        cutoff = cutoff,
                        final.models = final.models)
      cutoff.list[, , i] <- cvmodl@cutoff.list
      if(i == n.iter)
        models <- cvmodl@models

      if(i == 1) {

        performances <- cvmodl@performances
        label <- dimnames(cvmodl@prob.table)[[2]]
        dimnames(prob.array)[[2]] <- label
        colnames(v.score) <- colnames(cvmodl@v.score)
        colnames(pred.error) <- colnames(cvmodl@pred.error)
        perf.array <- array(0, dim = c(nrow(cvmodl@performances), ncol(cvmodl@performances)-3, n.iter))
      }

      prob.array[, , , i] <- cvmodl@prob.table
      pred.array[, , i] <- cvmodl@pred.table
      v.score[, , i] <- cvmodl@v.score
      pred.error[, , i] <- cvmodl@pred.error
      perf.array[, , i] <- as.matrix(cvmodl@performances[, 1:(ncol(cvmodl@performances)-3)])
    }

    performances$ITR      <- cvmodl@performances$ITR
    performances$regCtrl  <- cvmodl@performances$regCtrl
    performances$wclsCtrl <- cvmodl@performances$wclsCtrl
    performances[, 1:(ncol(performances)-3)] <- apply(perf.array, 1:2, mean, na.rm = TRUE)

    return(methods::new("workflow",
                        pred.array = pred.array, # pred.table,
                        prob.array = prob.array, # prob.table,
                        performance.array = perf.array,
                        performances = performances, # cbind(perf, performances),
                        v.score = apply(v.score, 1:2, mean),
                        cutoff.list = cutoff.list,
                        models = models,
                        pred.error = apply(pred.error, 1:2, mean)))
  } else {

    models <- list()
    v.score <- matrix(NA, length(cvCtrl.list), dim(trainObj@X)[2])
    performances <- data.frame()
    prob.array <- array(NA, c(length(validObj@trtLabl), 2, length(cvCtrl.list)))
    pred.array <- matrix(NA, length(validObj@trtLabl), length(cvCtrl.list))
    pred.error <- matrix(NA, length(cvCtrl.list), length(validObj@trtLabl))
    cutoff.list <- numeric(length(cvCtrl.list))

    for(i in 1:length(cvCtrl.list)) {

      ITR <- cvCtrl.list[[i]]$ITR
      cat("Performing --", itr.names(ITR), "(", i, "/", length(cvCtrl.list), ")", "\n")

      args <- append(list("object" = trainObj), cvCtrl.list[[i]])
      args <- removeArg(args, "ITR")

      start.time <- Sys.time()
      cvModel <- do.call(what = ITR, args = args)
      end.time <- Sys.time()
      models[[i]] <- cvModel

      cutoff.v <- cutoff
      if(class(trainObj) == "wClsObj") {

        cutoff.v <- cutoff.calc(cutoff, trainObj, cvModel)
        cutoff.list[i] <- cutoff.v
      }
      pred <- predict(cvModel, validObj, cutoff = cutoff)

      stat <- evalPred(pred,
                       validObj@trtTrue,
                       cutoff = cutoff.v,
                       type = "Binary",
                       weight = validObj@sample.weight,
                       inclusion = validObj@sample.inclsn,
                       Y.val = validObj@trtLabl,
                       Y.resp = validObj@trtResp)

      if(class(pred) != "ListPredObj") {

        # v.score[i, ] <- pred@v.score
        dimnames(prob.array)[[2]] <- colnames(pred@prob)
        prob.array[, , i] <- pred@prob
        pred.array[, i] <- as.character(pred@pred)
        pred.error[i, ] <- as.numeric(pred@pred != validObj@trtTrue)
      } else {

        dimnames(prob.array)[[2]] <- colnames(pred@listPred[[1]]@prob)
        prob.array[, , i] <- pred@listPred[[1]]@prob
        pred.array[, i] <- as.character(pred@listPred[[1]]@pred)
        pred.error[i, ] <- as.numeric(pred@listPred[[1]]@pred != validObj@trtTrue)
      }

      regCtrl.c <- wclsCtrl.c <- ""
      if("regCtrl"  %in% names(cvCtrl.list[[i]])) regCtrl.c  <- ctrl.names(cvCtrl.list[[i]]$regCtrl)
      if("wclsCtrl" %in% names(cvCtrl.list[[i]])) wclsCtrl.c <- ctrl.names(cvCtrl.list[[i]]$wclsCtrl)

      performances <- rbind(performances, cbind(stat, as.data.frame(list(runtime = -as.numeric(difftime(start.time, end.time, units = "secs")),
                                                                         ITR = itr.names(ITR),
                                                                         regCtrl = regCtrl.c,
                                                                         wclsCtrl = wclsCtrl.c))))
    }

    return(methods::new("workflow",
                        pred.array = pred.array,
                        prob.array = prob.array,
                        performances = performances,
                        v.score = v.score,
                        cutoff.list = cutoff.list,
                        models = models,
                        pred.error = pred.error))
  }
}
