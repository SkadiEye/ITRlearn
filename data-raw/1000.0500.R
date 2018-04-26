#!/usr/bin/env Rscript
library("optparse")

option_list = list(
  make_option(c("-s", "--seed"), type="character", default="out",
              help="seed", metavar="character")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

library(ITRlearn)
library(dnnet)
library(methods)
library(MASS)
set.seed(1357)
n.effect <- 5
beta1 <- runif(n.effect)*2-1
beta2 <- runif(n.effect)*2-1

set.seed(opt$seed)
n.sample = 1000
n.p = 500
rho = 1
sigma = matrix(0, n.p, n.p)
for(i in 1:dim(sigma)[1]) for(j in 1:dim(sigma)[2]) sigma[i, j] = exp(-rho*abs(i-j))
X <- mvrnorm(2*n.sample, rep(0, n.p), sigma)
Y1 <- as.numeric(X[, 1:n.effect*5] %*% beta1 + X[, 6]**2*X[, 7]**2) +
  as.numeric(X[, 1:n.effect*5-1] %*% beta2 + X[, 8]*X[, 3]**3)
Y2 <- as.numeric(X[, 1:n.effect*5] %*% beta1 + X[, 6]**2*X[, 7]**2) -
  as.numeric(X[, 1:n.effect*5-1] %*% beta2 + X[, 8]*X[, 3]**3)

label <- c("A", "B")
sim.dat <- impoorIdeal(X = X, trtResp1 = Y1, trtResp2 = Y2, label = label)

trt.dat <- generateTrtData(sim.dat, inc.cutoff = 1, epsilon = 1)
split.dat <- splitData(trt.dat, 0.5)
train.dat <- split.dat$trainObj
test.dat <- split.dat$validObj

regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:30)))
regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.5**(0:10)))
wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.5**(0:10)))
wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.5**(0:10)))
regCtrl.DNN    <- list(machine = "regDNN", n.batch = 10, n.epoch = 100, activate = "elu",
                       l1.reg = 10**-4, plot = TRUE , learning.rate.adaptive = "adagrad", early.stop.det = 100, accel = "rcpp")
wclsCtrl.DNN   <- list(machine = "wclsDNN", n.batch = 10, n.epoch = 100, activate = "elu",
                       l1.reg = 10**-4, plot = TRUE, learning.rate.adaptive = "adagrad", early.stop.det = 100, accel = "rcpp")
regCtrl.RF     <- list(machine = "regRF", ntree = 2000)

cvCtrl.list <- list(list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(10, 10, 10)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(10, 10, 10))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(20, 20, 20)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(20, 20, 20))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(30, 30, 30)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(30, 30, 30))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(40, 40, 40)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(40, 40, 40))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(50, 50, 50)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(50, 50, 50))))),
                    list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
                    # list(ITR = "itrDOWL", regCtrl = regCtrl.svm, wclsCtrl = wclsCtrl.lsvm),
                    # list(ITR = "itrOWL", wclsCtrl = wclsCtrl.lsvm),
                    # list(ITR = "itrOWL", wclsCtrl = wclsCtrl.ksvm),
                    # list(ITR = "itrOWL", wclsCtrl = wclsCtrl.LASSO),
                    list(ITR = "itrVT", regCtrl = regCtrl.RF),
                    # list(ITR = "itrVT", regCtrl = regCtrl.LASSO),
                    # list(ITR = "itrVT", regCtrl = regCtrl.svm),
                    # list(ITR = "itrSimple", regCtrl = regCtrl.RF),
                    # list(ITR = "itrSimple", regCtrl = regCtrl.svm),
                    list(ITR = "itrSimple", regCtrl = regCtrl.LASSO))

(a <- Sys.time())
wfmodl <- workflow(train.dat, validObj = test.dat, cvCtrl.list = cvCtrl.list)
(b <- Sys.time() - a)
wfmodl

write.csv(wfmodl@performances, paste(opt$seed, ".performance.csv", sep=''))


