################################################
#### K-fold Cross Validation

#' K-fold Cross Validation
#'
#' Randomly cut the training set into k-folds and Conduct a K-fold Cross Validation.
#'  A list of models can be evaluated in the same function. The performance for each model
#'  will be evaluated as required.
#'
#' @param trainObj A \code{RegObj} or \code{wClsObj} containing the training set.
#' @param k.fold The number of folds in the cross validation.
#' @param cvCtrl.list A \code{list} of \code{list}s of arguments that specifies a regression or classification method.
#'  Each list of arguments is handled by \code{\link{itrCtrlPanel}}.
#' @param cutoff Cutoff used when predicting the current fold that not used in training the model, passed to \code{cutoff.calc} function.
#' @param unbalance.trt A character string that specifies if sampleing techniques should be applied to
#'  data with unbalanced numbers for each class.
#' @param final.models A \code{logical} value indicating whether the final models are kept in the outout.
#'
#' @return A \code{\link{cvKfold-class}} object.
#'
#' @seealso
#' \code{\link{cutoff.calc}}\cr
#' \code{\link{reg}}\cr
#' \code{\link{wcls}}\cr
#' \code{\link{cvGrid}}\cr
#' \code{\link{ensemble}}\cr
#' \code{\link{workflow}}\cr
#'
#' @export
cvKfold <- function(trainObj,
                    k.fold = 10,
                    cvCtrl.list,
                    cutoff = 0.5,
                    unbalance.trt = c("None", "Over", "Under")[1],
                    final.models = FALSE) {

  n.sample <- dim(trainObj@X)[1]
  sample.dat <- sample(n.sample)
  models <- list()
  pred.table <- matrix(NA, n.sample, length(cvCtrl.list))
  pred.error <- matrix(NA, length(cvCtrl.list), n.sample)
  colnames(pred.error) <- rownames(trainObj@X)
  cutoff.list <- matrix(NA, k.fold, length(cvCtrl.list))
  prob.table <- array(NA, dim=c(n.sample, 2, length(cvCtrl.list)))
  performances <- data.frame()

  all.v.score <- array(0, dim = c(length(cvCtrl.list), dim(trainObj@X)[2], k.fold))
  v.score <- matrix(0, length(cvCtrl.list), dim(trainObj@X)[2])
  colnames(v.score) <- colnames(trainObj@X)

  if (k.fold < 1 || k.fold > n.sample) {

    k.fold <- n.sample
    cat("K.fold cannot be less than 1 or greater than sample size. Performing leave-one-out cross validation. ")
  }

  for(k in 1:k.fold) {

    cat("------- Fold", k, "-------------------------- \n")
    print(Sys.time())

    if(class(trainObj) == "RegObj") {

      valid.ind <- sample.dat[(floor(n.sample*(k-1)/k.fold)+1):floor(n.sample*k/k.fold)]
    } else {

      unique.Y <- unique(trainObj@trtLabl)
      valid.ind <- c()
      for(i in 1:length(unique.Y)) {

        ind.i <- which(trainObj@trtLabl[sample.dat] == unique.Y[i])
        n.sample.i <- sum(trainObj@trtLabl[sample.dat] == unique.Y[i])
        valid.ind <- c(valid.ind, sample.dat[ind.i[(floor(n.sample.i*(k-1)/k.fold)+1):floor(n.sample.i*k/k.fold)]])
      }
    }

    train.ind <- (1:n.sample)[-valid.ind]

    if(class(trainObj) != "RegObj" && unbalance.trt != "None") {

      y.table <- table(trainObj@trtLabl[train.ind])
      train.ind2 <- c()
      if(unbalance.trt == "Over") {

        for(i in 1:length(unique(trainObj@trtLabl))) {

          if(i != which.max(y.table)) {

            train.ind2 <- c(train.ind2, rep(train.ind[which(trainObj@trtLabl[train.ind] == names(y.table)[i])], floor(max(y.table)/y.table[i])))
            train.ind2 <- c(train.ind2, sample(train.ind[which(trainObj@trtLabl[train.ind] == names(y.table)[i])], max(y.table) %% y.table[i]))
          } else
            train.ind2 <- c(train.ind2, train.ind[which(trainObj@trtLabl[train.ind] == names(y.table)[i])])
        }
      }

      if(unbalance.trt == "Under") {

        for(i in 1:length(unique(trainObj@trtLabl))) {

          if(i != which.min(y.table))
            train.ind2 <- c(train.ind2, sample(train.ind[which(trainObj@trtLabl[train.ind] == names(y.table)[i])], min(y.table)))
          else
            train.ind2 <- c(train.ind2, train.ind[which(trainObj@trtLabl[train.ind] == names(y.table)[i])])
        }
      }

      train.ind <- train.ind2
    }

    trainingSet <- splitData(trainObj, train.ind)$trainObj
    validSet <- splitData(trainObj, valid.ind)$trainObj

    for(i in 1:length(cvCtrl.list)) {

      ITR <- cvCtrl.list[[i]]$ITR
      cat("Performing --", itr.names(ITR), "\n")

      if(final.models && k == k.fold) {

        args <- append(list("object" = trainObj), cvCtrl.list[[i]])
        args <- removeArg(args, "ITR")
        cvModel <- do.call(what = ITR, args = args)
        models[[i]] <- cvModel
      }

      args <- append(list("object" = trainingSet), cvCtrl.list[[i]])
      args <- removeArg(args, "ITR")

      start.time <- Sys.time()
      cvModel <- do.call(what = ITR, args = args)
      end.time <- Sys.time()

      cutoff.v <- cutoff
      if(class(trainObj) == "wClsObj") {

        cutoff.v <- cutoff.calc(cutoff, trainingSet, cvModel)
        cutoff.list[k, i] <- cutoff.v
      }

      pred <- predict(cvModel, validSet, cutoff = cutoff.v)
      all.v.score[i, match(pred@v.score$v.names, colnames(trainObj@X)), k] <- pred@v.score$score

      prob.table[valid.ind, , i] <- pred@prob
      pred.table[valid.ind, i] <- as.character(pred@pred)
      pred.error[i, valid.ind] <- as.numeric(pred@pred != trainObj@trtTrue[valid.ind])

      if(k == k.fold) {

        v.score[i, ] <- rowMeans(all.v.score[i, , ])

        dimnames(prob.table)[[2]] <- colnames(pred@prob)
        stat <- evalPred(methods::new("PredDiscreteObj",
                                      prob = prob.table[, , i],
                                      pred = factor(pred.table[, i], levels = colnames(pred@prob))),
                         trainObj@trtTrue,
                         cutoff = cutoff.v,
                         type = "Binary",
                         weight = trainObj@sample.weight,
                         inclusion = trainObj@sample.inclsn,
                         Y.val = trainObj@trtLabl,
                         Y.resp = validObj@trtResp)

        regCtrl.c <- wclsCtrl.c <- ""
        if("regCtrl"  %in% names(cvCtrl.list[[i]])) regCtrl.c  <- ctrl.names(cvCtrl.list[[i]]$regCtrl)
        if("wclsCtrl" %in% names(cvCtrl.list[[i]])) wclsCtrl.c <- ctrl.names(cvCtrl.list[[i]]$wclsCtrl)

        performances <- rbind(performances, cbind(stat, as.data.frame(list(runtime = -as.numeric(difftime(start.time, end.time, units = "secs")),
                                                                           ITR = itr.names(ITR),
                                                                           regCtrl = regCtrl.c,
                                                                           wclsCtrl = wclsCtrl.c))))
      }
    }
  }

  return(new("cvKfold",
             pred.table = pred.table,
             prob.table = prob.table,
             models = models,
             cutoff.list = cutoff.list,
             performances = performances,
             v.score = v.score,
             pred.error = pred.error))
}



