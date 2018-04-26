#!/usr/bin/env Rscript
library("optparse")

option_list = list(
  make_option(c("-s", "--seed"), type="character", default="out",
              help="seed", metavar="character")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

library(MASS)
library(ITRlearn)
library(methods)

regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:40)))
regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.5**(0:10)))
wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.5**(0:10)))
wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.5**(0:10)))
regCtrl.DNN    <- list(machine = "regDNN", n.batch = 100, n.epoch = 1000, activate = "elu", accel = "rcpp",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN   <- list(machine = "wclsDNN", n.batch = 100, n.epoch = 1000, activate = "elu", accel = "rcpp",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.RF     <- list(machine = "regRF", ntree = 2000)

cvCtrl.list <- list(list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(30, 30, 30)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(30, 30, 30))))),
                    list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
                    list(ITR = "itrVT", regCtrl = regCtrl.RF),
                    list(ITR = "itrSimple", regCtrl = regCtrl.LASSO))

set.seed(9807)
xbeta <- runif(5)/2 - 1/4

i = 0
for(n.p in c(10, 50, 100)) {
  
  i = i+1
  set.seed(opt$seed)
  n.sample = 1000
  # n.p = 10
  rho = 1
  sigma = matrix(0, n.p, n.p)
  for(i in 1:dim(sigma)[1]) for(j in 1:dim(sigma)[2]) sigma[i, j] = exp(-rho*abs(i-j))
  X <- mvrnorm(2*n.sample, rep(0, n.p), sigma)
  Y1 <- 0.5*X[, 2] + 0.5*X[, 4] - X[, 6] + X[, 8]*X[, 10] + (X[, 5] < -0.5 | X[, 9] < -0.5)
  Y2 <- 0.5*X[, 2] + 0.5*X[, 4] - X[, 6] + X[, 8]*X[, 10] + 1.5*(X[, 5] >= -0.5 & X[, 9] >= -0.5)
  label <- c("A", "B")
  sim.dat <- impoorIdeal(X = X, trtResp1 = Y1, trtResp2 = Y2, label = label)
  
  trt.dat <- generateTrtData(sim.dat, inc.cutoff = 1, epsilon = 1)
  split.dat <- splitData(trt.dat, 0.5)
  train.dat <- split.dat$trainObj
  test.dat <- split.dat$validObj
  
  wfmodl <- workflow(train.dat, validObj = test.dat, cvCtrl.list = cvCtrl.list)
  write.csv(wfmodl@performances, paste("sce7.", opt$seed, ".", i, ".performance.csv", sep=''))
}



