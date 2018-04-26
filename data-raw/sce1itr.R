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
set.seed(opt$seed)
n.sample = 1000
n.p = 100
rho = 1
sigma = matrix(0, n.p, n.p)
for(i in 1:dim(sigma)[1]) for(j in 1:dim(sigma)[2]) sigma[i, j] = exp(-rho*abs(i-j))
X <- mvrnorm(2*n.sample, rep(0, n.p), sigma)
Y1 <- as.numeric(rowSums(X[, 1:5]) + rowSums(X[, 6:10]))
Y2 <- as.numeric(rowSums(X[, 1:5]) - rowSums(X[, 6:10]))
label <- c("A", "B")
sim.dat <- impoorIdeal(X = X, trtResp1 = Y1, trtResp2 = Y2, label = label)

regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:40)))
regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.5**(0:10)))
wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.5**(0:10)))
wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.5**(0:10)))
regCtrl.DNN    <- list(machine = "regDNN", n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN   <- list(machine = "wclsDNN", n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.RF     <- list(machine = "regRF", ntree = 2000)

cvCtrl.list <- list(list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = 10))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = 10)))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(10, 10)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(10, 10))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(10, 10, 10)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(10, 10, 10))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(10, 10, 10, 10)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(10, 10, 10, 10))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = 20))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = 20)))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(20, 20)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(20, 20))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(20, 20, 20)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(20, 20, 20))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(20, 20, 20, 20)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(20, 20, 20, 20))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = 30))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = 30)))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(30, 30)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(30, 30))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(30, 30, 30)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(30, 30, 30))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(30, 30, 30, 30)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(30, 30, 30, 30))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = 40))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = 40)))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(40, 40)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(40, 40))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(40, 40, 40)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(40, 40, 40))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(40, 40, 40, 40)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(40, 40, 40, 40))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = 50))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = 50)))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(50, 50)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(50, 50))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(50, 50, 50)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(50, 50, 50))))),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(50, 50, 50, 50)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(50, 50, 50, 50))))),
                    list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
                    list(ITR = "itrVT", regCtrl = regCtrl.RF),
                    list(ITR = "itrSimple", regCtrl = regCtrl.LASSO))

# cl <- parallel::makeCluster(4)
# doParallel::registerDoParallel(cl)
#
# result <- foreach::foreach(i = 1:8, .packages = "ITRlearn") %dopar% {

trt.dat <- generateTrtData(sim.dat, inc.cutoff = 1, epsilon = 1)
split.dat <- splitData(trt.dat, 0.5)
train.dat <- split.dat$trainObj
train.dat
test.dat <- split.dat$validObj
test.dat

wfmodl <- workflow(train.dat, validObj = test.dat, cvCtrl.list = cvCtrl.list)
# }
#
# foreach::registerDoSEQ()
# parallel::stopCluster(cl)

write.csv(wfmodl@performances, paste(opt$seed, ".performance.csv", sep=''))



