####
getGrid <- function(machine, list.p) {

  args <- list.p
  grid <- expand.grid(args, stringsAsFactors = FALSE)

  # if (machine %in% c("wclsSVM", "regSVM")) {
  if (machine %in% c("regSVM")) {

    if(!"kernel" %in% names(args)) grid$kernel <- "radial"
    if(!"cost" %in% names(args)) grid$cost <- 1

    if("radial" %in% grid$kernel) {

      if("gamma" %in% names(args))
        grid$gamma[grid$kernel == "linear"] <- NA
    }

    if("polynomial" %in% names(args) || "sigmoid" %in% names(args)) {

      if("degree" %in% names(args))
        grid$degree[grid$kernel != "polynomial"] <- NA
      if("coef0" %in% names(args))
        grid$coef0[grid$kernel %in% c("linear", "kernel")] <- NA
    }
  }

  # if (machine %in% c("wclsLASSO", "regLASSO") && "lambda" %in% names(grid)) {
  if (machine %in% c("regLASSO") && "lambda" %in% names(grid)) {

    grid$lambda <- NULL
    grid$nlambda <- NULL
  }

  grid <- unique(grid)
  return(grid)
}

################################################
#### Grid Search via K-fold Cross Validation

#' Grid Search via K-fold Cross Validation
#'
#' Grid Search for hyper-parameters in a \code{reg} or \code{wcls} model via K-fold Cross
#'  Validation.
#'
#' @param trainObj A \code{RegObj} or \code{wClsObj} containing the training set.
#' @param k.fold The number of folds in the cross validation.
#' @param gsCtrl A \code{list} of arguments that specifies a regression or classification method, handled by
#'  \code{\link{gsCtrlPanel}}.
#' @param cutoff A \code{numeric} value used as a cutoff to calculate accuracy when the outcome is bianry.
#' @param criteria A criteria to decide for the best tuning parameters. If NA, then the best model will not be picked.
#' @param weight A \code{numeric} vector indicating the weight of all samples. Used for the 'w.acc' criteria.
#' @param inclusion A \code{logical} vector indicating whether a sample shoule be included in the performance evaluation.
#' @param print.fold A \code{logical} value indicating whether each fold should be reported while model fitting.
#' @param progress A \code{logical} value indicating whether a progress bar would be used to show the progress.
#' @param all.models A \code{logical} value indicating whether to save all fitted models.
#'
#' @return A \code{\link{cvGrid-class}} object.
#'
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#'
#' @seealso
#' \code{\link{reg}}\cr
#' \code{\link{wcls}}\cr
#' \code{\link{ensemble}}\cr
#' \code{\link{cvKfold}}\cr
#' \code{\link{workflow}}\cr
#'
#' @export
cvGrid <- function(trainObj, k.fold = 10, gsCtrl, cutoff = 0.5,
                   criteria = c("acc", "sens", "spec", "auc", "ppv", "npv", "mse", "w.acc", "a.acc", "val.f", NA)[ifelse("RegObj" %in% class(trainObj), 7, 4)],
                   weight = rep(1, length(trainObj@X)[1]),
                   inclusion = rep(TRUE, length(trainObj@X)[1]),
                   print.fold = FALSE,
                   progress = TRUE,
                   all.models = FALSE) {

  # if(gsCtrl$machine %in% c("wclsLASSO", "regLASSO") && length(gsCtrl) == 1)
  if(gsCtrl$machine %in% c("regLASSO") && length(gsCtrl) == 1)
    gsCtrl <- append(gsCtrl, list("alpha" = 1))
  grid <- getGrid(gsCtrl$machine, gsCtrl)

  if(length(gsCtrl$machine) > 1)
    stop("Grid search can be done with only one machine. ")
  if(length(gsCtrl) < 2)
    stop("Please include at least one hyper-parameter in grid search. ")

  n.sample <- length(trainObj@Y)
  sample.dat <- sample(n.sample)
  if(all.models) models <- list()
  pred.table <- matrix(NA, n.sample, dim(grid)[1])
  if (class(trainObj) != "RegObj")
    prob.table <- array(NA, dim=c(n.sample, length(unique(trainObj@Y)), dim(grid)[1]))
  performances <- data.frame()
  lambda <- numeric(dim(grid)[1])
  cutoff.list <- c()

  if (k.fold <= 1 || k.fold > n.sample) {

    k.fold <- n.sample
    cat("K.fold cannot be less than 2 or greater than sample size. Performing leave-one-out cross validation... ")
  }

  if(progress) pb <- utils::txtProgressBar(min = 0, max = k.fold*dim(grid)[1], style = 3)

  for(k in 1:k.fold) {

    if(print.fold)
      cat("-----Fold: ", k, "------------------- \n")

    if(class(trainObj) == "RegObj") {

      valid.ind <- sample.dat[(floor(n.sample*(k-1)/k.fold)+1):floor(n.sample*k/k.fold)]
    } else {

      unique.Y <- unique(trainObj@Y)
      valid.ind <- c()
      for(i in 1:length(unique.Y)) {

        ind.i <- which(trainObj@Y[sample.dat] == unique.Y[i])
        n.sample.i <- sum(trainObj@Y[sample.dat] == unique.Y[i])
        valid.ind <- c(valid.ind, sample.dat[ind.i[(floor(n.sample.i*(k-1)/k.fold)+1):floor(n.sample.i*k/k.fold)]])
      }
    }

    train.ind <- (1:n.sample)[-valid.ind]
    # data.k <- splitData(trainObj, train.ind)
    # trainingSet <- data.k$trainObj
    # validSet <- data.k$validObj

    trainingSet <- splitData(trainObj, train.ind)$trainObj
    validSet <- splitData(trainObj, valid.ind)$trainObj

    # if(!is.null(vsCtrl)) {
    #
    #   vsMachine <- vsCtrl$vsMachine
    #   args <- append(list("object" = trainingSet), vsCtrl)
    #   trainingSet <- do.call(what = vsMachine, args = removeArg(args, "vsMachine"))
    # }

    for(i in 1:dim(grid)[1]) {

      machine <- gsCtrl$machine
      grid.i <- as.list(grid[i, ])
      names(grid.i) <- colnames(grid)

      # if(machine %in% c("wclsLASSO", "regLASSO")) {
      if(machine %in% c("regLASSO")) {

        if(k == 1) {

          # if("v.keep" %in% colnames(grid))
          #   trainObjSlim <- vsNull(trainObj, v.keep = grid$v.keep[i])
          # else trainObjSlim <- trainObj

          args <- append(list("x" = as.matrix(trainObj@X),
                              "y" = trainObj@Y,
                              "family" = ifelse(class(trainObj) == "wClsObj",
                                                "binomial", "gaussian")), grid.i)
          args <- append(args, list("nfolds" = k.fold))

          # if("v.keep" %in% names(args))
          #   args <- removeArg(args, "v.keep")
          if("lambda" %in% names(gsCtrl))
            args <- append(args, list("lambda" = gsCtrl$lambda))
          if("criteria" %in% names(gsCtrl)) {

            if(gsCtrl$criteria == "auc")
              args <- append(args, list("type.measure" = "auc"))
            if(gsCtrl$criteria == "acc")
              args <- append(args, list("type.measure" = "class"))
            if(gsCtrl$criteria == "dev")
              args <- append(args, list("type.measure" = "deviance"))
          }

          cvModel <- do.call(what = glmnet::cv.glmnet, args = removeArg(args[!is.na(args)], "machine"))
          lambda[i] <- cvModel$lambda[which.min(cvModel$cvm)]
        }
      }

      args <- append(list("object" = trainingSet), grid.i)
      # if(machine %in% c("wclsLASSO", "regLASSO"))
      if(machine %in% c("regLASSO"))
        args$lambda <- lambda[i]
      args <- removeArg(args, "machine")
      cvModel <- do.call(what = machine, args = args[!is.na(args)])
      pred <- predict(cvModel, validSet)

      if(class(trainObj) != "RegObj") {
        prob.table[valid.ind, , i] <- pred@prob
        pred.table[valid.ind, i] <- as.character(pred@pred)
      } else
        pred.table[valid.ind, i] <- pred@pred

      if(all.models) models <- append(models, list(cvModel))

      if(k == k.fold) {

        cutoff.v <- cutoff
        if(class(trainObj) == "wClsObj") {

          cutoff.v <- cutoff.calc(cutoff, trainingSet, cvModel)
          cutoff.list <- c(cutoff.list, cutoff.v)
        }

        if(class(trainObj) != "RegObj")
          dimnames(prob.table)[[2]] <- colnames(pred@prob)

        if(!sum(is.na(pred.table[, i]))) {

          if(class(trainObj) != "RegObj")
            stat <- evalPred(methods::new("PredDiscreteObj",
                                          prob = prob.table[, , i],
                                          pred = factor(pred.table[, i], levels = colnames(pred@prob))),
                             trainObj@Y,
                             # trainObj@Y.true,
                             cutoff = cutoff.v,
                             type = "Binary",
                             weight = weight,
                             inclusion = inclusion)
          else
            stat <- evalPred(new("PredContinuousObj",
                                 pred = pred.table[, i]),
                             trainObj@Y,
                             type = "Continuous")
        } else {

          stat <- NA
        }
        performances <- rbind(performances, stat)
      }

      if(progress) utils::setTxtProgressBar(pb, (k-1)*dim(grid)[1]+i)
    }
  }

  if(progress) close(pb)

  # if(machine %in% c("wclsLASSO", "regLASSO"))
  if(machine %in% c("regLASSO"))
    performances <- cbind(grid, lambda, performances)
  else performances <- cbind(grid, performances)

  if(!is.na(criteria)) {

    best.ind <- do.call(ifelse(criteria != "mse", "which.max", "which.min"),
                        args = list(x = performances[, which(criteria == colnames(performances))]))

    # if(!is.null(vsCtrl)) {
    #
    #   for(i in 1:length(vsCtrl)) {
    #
    #     vsMachine <- vsCtrl[[i]]$vsMachine
    #     args <- append(list("object" = trainObj), vsCtrl[[i]])
    #     trainObj <- do.call(what = vsMachine, args = removeArg(args, "vsMachine"))
    #   }
    # }

    machine <- gsCtrl$machine
    grid.i <- as.list(grid[best.ind, ])
    names(grid.i) <- colnames(grid)
    args <- append(list("object" = trainObj), grid.i)
    # if(machine %in% c("wclsLASSO", "regLASSO"))
    if(machine %in% c("regLASSO"))
      args$lambda <- lambda[best.ind] # lambda.best
    args <- removeArg(args, "machine")
    best.model <- do.call(what = machine, args = args[!is.na(args)])

  } else best.model <- NA

  return(new("cvGrid",
             best.model = best.model,
             performances = performances,
             models = ifelse(all.models, models, list()),
             cutoff.list = cutoff.list))

}









