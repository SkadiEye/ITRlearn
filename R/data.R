###############################
#### sim.dat

#' An example oracle data, high dimensional
#'
#' This dataset contains 1000 samples. Each sample has two responses (Y1 and Y2) under
#'  two treatments A and B, and a predictor matrix X of 1000 variables.
#'  The correlation between Xi and Xj is exp(-|i-j|).
#'
#' @format A \code{TrtDataIdeal-class} object
#' @details
#' The treatment outcomes Y1 and Y2 are generated as follows,
#' \deqn{Y1 = X \beta1 + X \beta2}
#' \deqn{Y2 = X \beta1 - X \beta2}
#' where \eqn{\beta1} and \eqn{\beta2} are both vectors of length-1000, and the first 20\%
#'  elements in \eqn{\beta1} and the last 80\% elements in \eqn{\beta2} are non-zero. The
#'  non-zero values are generated under N(0, 1) independently.
#'
#' @examples
#' data("sim.dat")
#' sim.dat
#'
#' #### randomly generate a treatment label for each subject
#' set.seed(19899891)
#' trt.dat <- generateTrtData(sim.dat, inc.cutoff = 1, epsilon = 1)
#'
#' #### split into training set and testing set
#' split.dat <- splitData(trt.dat, 0.5)
#' train.dat <- split.dat$trainObj
#' train.dat
#' test.dat <- split.dat$validObj
#' test.dat
#'
#' #### control panels for regressors/classifiers, including LASSO, SVM, random forest (RF) and deep neural network (DNN) ensemble
#' # "cvGrid" uses k-fold cross-validation grid search to search for hyper parameters for SVM and LASSO
#' # "ensemble" would apply ensemble learning on any method
#'
#' regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
#' wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:50)))
#' regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.5**(0:10)))
#' wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.5**(0:10)))
#' wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.5**(0:10)))
#' regCtrl.DNN    <- list(machine = "regDNN", N.hidden = c(100, 100, 100), n.batch = 50, n.epoch = 250,
#'                        g = elu, g.prime = elu.prime, mu=0.0001, lambda1 = 0.00001, lambda2 = 0)
#' wclsCtrl.DNN   <- list(machine = "wclsDNN", N.hidden = c(100, 100, 100), n.batch = 50, n.epoch = 1000,
#'                        g = elu, g.prime = elu.prime, mu=0.0001, lambda1 = 0.00001, lambda2 = 0)
#' regCtrl.RF     <- list(machine = "regRF", ntree = 2000)
#'
#' #### a list of methods to be performed within several frameworks:
#' # 1. outcome weighted learning
#' #      DNN Ensemble, SVM, LASSO
#' # 2. virtual twins
#' #      RF, LASSO, SVM
#' # 3. a simple method (Lu Tian 2014)
#' #      RF, LASSO, SVM, DNN Ensemble
#'
#' cvCtrl.list <- list(list(ITR = "itrDOWL",
#'                          regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE, esCtrl = regCtrl.DNN),
#'                          wclsCtrl = list(machine = "ensemble", n.ensemble = 200, prop.keep = 1, esCtrl = wclsCtrl.DNN)),
#'                     list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
#'                     list(ITR = "itrDOWL", regCtrl = regCtrl.svm, wclsCtrl = wclsCtrl.lsvm),
#'                     list(ITR = "itrOWL", wclsCtrl = wclsCtrl.lsvm),
#'                     list(ITR = "itrVT", regCtrl = regCtrl.RF),
#'                     list(ITR = "itrSimple", regCtrl = regCtrl.LASSO))
#'
#' \donttest{
#' #### one function pipeline workflow
#' set.seed(20170822)
#' (a <- Sys.time())
#' wfmodl <- workflow(train.dat, validObj = test.dat, cvCtrl.list = cvCtrl.list)
#' (b <- Sys.time() - a)
#' wfmodl
#' }
"sim.dat"

###############################
#### sim.dat2

#' An example oracle data, low dimensional
#'
#' This dataset contains 1000 samples. Each sample has two responses (Y1 and Y2) under
#'  two treatments A and B, and a predictor matrix X of 10 variables.
#'  The correlation between Xi and Xj is exp(-|i-j|).
#'
#' @format A \code{TrtDataIdeal-class} object
#' @details
#' The treatment outcomes Y1 and Y2 are generated as follows,
#' \deqn{Y1 = (X1 + X2 + X3 + X4 + X5) + (X6 + 4*X7*X10)}
#' \deqn{Y2 = (X1 + X2 + X3 + X4 + X5) - (X6 + 4*X7*X10)}
#'
#' @seealso
#' \code{sim.dat.mini}
#'
#' @examples
#' data("sim.dat2")
#' sim.dat2
#'
#' #### randomly generate a treatment label for each subject
#' set.seed(19899891)
#' trt.dat <- generateTrtData(sim.dat2, inc.cutoff = 1, epsilon = 1)
#'
#' #### split into training set and testing set
#' split.dat <- splitData(trt.dat, 0.5)
#' train.dat <- split.dat$trainObj
#' train.dat
#' test.dat <- split.dat$validObj
#' test.dat
#'
#' #### control panels for regressors/classifiers, including LASSO, SVM, random forest (RF) and deep neural network (DNN) ensemble
#' # "cvGrid" uses k-fold cross-validation grid search to search for hyper parameters for SVM and LASSO
#' # "ensemble" would apply ensemble learning on any method
#'
#' regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
#' wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:50)))
#' regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.5**(0:10)))
#' wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.5**(0:10)))
#' wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.5**(0:10)))
#' regCtrl.DNN    <- list(machine = "regDNN", N.hidden = c(100, 100, 100), n.batch = 50, n.epoch = 250,
#'                        g = elu, g.prime = elu.prime, mu=0.002, lambda1 = 0.00001, lambda2 = 0)
#' wclsCtrl.DNN   <- list(machine = "wclsDNN", N.hidden = c(100, 100, 100), n.batch = 50, n.epoch = 1000,
#'                        g = tanh, g.prime = tanh.prime, mu=0.005, lambda1 = 0.0001, lambda2 = 0)
#' regCtrl.RF     <- list(machine = "regRF", ntree = 2000)
#'
#' #### a list of methods to be performed within several frameworks:
#' # 1. outcome weighted learning
#' #      DNN Ensemble, SVM, LASSO
#' # 2. virtual twins
#' #      RF, LASSO, SVM
#' # 3. a simple method (Lu Tian 2014)
#' #      RF, LASSO, SVM, DNN Ensemble
#'
#' cvCtrl.list <- list(list(ITR = "itrDOWL",
#'                          regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE, esCtrl = regCtrl.DNN),
#'                          wclsCtrl = list(machine = "ensemble", n.ensemble = 200, prop.keep = 1, esCtrl = wclsCtrl.DNN)),
#'                     list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
#'                     list(ITR = "itrDOWL", regCtrl = regCtrl.svm, wclsCtrl = wclsCtrl.lsvm),
#'                     list(ITR = "itrOWL", wclsCtrl = wclsCtrl.lsvm),
#'                     list(ITR = "itrVT", regCtrl = regCtrl.RF),
#'                     list(ITR = "itrSimple", regCtrl = regCtrl.LASSO))
#'
#' \donttest{
#' #### one function pipeline workflow
#' set.seed(20170822)
#' (a <- Sys.time())
#' wfmodl <- workflow(train.dat, validObj = test.dat, cvCtrl.list = cvCtrl.list)
#' (b <- Sys.time() - a)
#' wfmodl
#' }
"sim.dat2"

###############################
#### sim.dat.mini

#' An example oracle data, low dimensional
#'
#' This dataset contains 100 samples. Each sample has two responses (Y1 and Y2) under
#'  two treatments A and B, and a predictor matrix X of 10 variables.
#'  The correlation between Xi and Xj is exp(-|i-j|).
#'
#' @format A \code{TrtDataIdeal-class} object
#' @details
#' The treatment outcomes Y1 and Y2 are generated as follows,
#' \deqn{Y1 = (X1 + X2 + X3 + X4 + X5) + (X6 + 4*X7*X10)}
#' \deqn{Y2 = (X1 + X2 + X3 + X4 + X5) - (X6 + 4*X7*X10)}
#' It is a mini version of \code{sim.dat2}.
#'
#' @seealso
#' \code{sim.dat2}
#'
#' @examples
#' data("sim.dat.mini")
#' sim.dat.mini
#'
#' #### randomly generate a treatment label for each subject
#' set.seed(19899891)
#' trt.dat <- generateTrtData(sim.dat.mini, inc.cutoff = 1, epsilon = 1)
#'
#' #### split into training set and testing set
#' split.dat <- splitData(trt.dat, 0.5)
#' train.dat <- split.dat$trainObj
#' train.dat
#' test.dat <- split.dat$validObj
#' test.dat
#'
#' #### control panels for regressors/classifiers, including LASSO, SVM, random forest (RF) and deep neural network (DNN) ensemble
#' # "cvGrid" uses k-fold cross-validation grid search to search for hyper parameters for SVM and LASSO
#' # "ensemble" would apply ensemble learning on any method
#'
#' regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
#' wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:50)))
#' regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.8**(-10:30)))
#' wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.8**(-10:30)))
#' wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.8**(-10:30)))
#' regCtrl.DNN    <- list(machine = "regDNN", N.hidden = c(10, 10), n.batch = 10, n.epoch = 250,
#'                        g = elu, g.prime = elu.prime, mu=0.002, lambda1 = 0.00001, lambda2 = 0)
#' wclsCtrl.DNN   <- list(machine = "wclsDNN", N.hidden = c(10), n.batch = 10, n.epoch = 1000,
#'                        g = elu, g.prime = elu.prime, mu=0.01, lambda1 = 0.0001, lambda2 = 0)
#' regCtrl.RF     <- list(machine = "regRF", ntree = 2000)
#'
#' #### a list of methods to be performed within several frameworks:
#' # 1. outcome weighted learning
#' #      DNN Ensemble, SVM, LASSO
#' # 2. virtual twins
#' #      RF, LASSO, SVM
#' # 3. a simple method (Lu Tian 2014)
#' #      RF, LASSO, SVM, DNN Ensemble
#'
#' cvCtrl.list <- list(list(ITR = "itrDOWL",
#'                          regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE, esCtrl = regCtrl.DNN),
#'                          wclsCtrl = list(machine = "ensemble", n.ensemble = 1000, prop.keep = 0.01, esCtrl = wclsCtrl.DNN)),
#'                     list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
#'                     list(ITR = "itrDOWL", regCtrl = regCtrl.svm, wclsCtrl = wclsCtrl.lsvm),
#'                     list(ITR = "itrOWL", wclsCtrl = wclsCtrl.lsvm),
#'                     list(ITR = "itrVT", regCtrl = regCtrl.RF),
#'                     list(ITR = "itrSimple", regCtrl = regCtrl.LASSO))
#'
#' #### one function pipeline workflow
#' \donttest{
#' set.seed(20170822)
#' (a <- Sys.time())
#' wfmodl <- workflow(train.dat, validObj = test.dat, cvCtrl.list = cvCtrl.list)
#' (b <- Sys.time() - a)
#' wfmodl
#' }
"sim.dat.mini"


