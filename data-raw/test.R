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
Y1 <- as.numeric(rowSums(X[, 1:5]) + (4*X[, 7]*X[, 10] + X[, 6]**2 - X[, 9]**2))
Y2 <- as.numeric(rowSums(X[, 1:5]) - (4*X[, 7]*X[, 10] + X[, 6]**2 - X[, 9]**2))
label <- c("A", "B")
sim.dat <- impoorIdeal(X = X, trtResp1 = Y1, trtResp2 = Y2, label = label)

regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:50)))
regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.5**(0:10)))
wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.5**(0:10)))
wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.5**(0:10)))
regCtrl.DNN21  <- list(machine = "regDNN", n.hidden = c(10, 10), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN21 <- list(machine = "wclsDNN", n.hidden = c(10, 10), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.DNN31  <- list(machine = "regDNN", n.hidden = c(10, 10, 10), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN31 <- list(machine = "wclsDNN", n.hidden = c(10, 10, 10), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.DNN41  <- list(machine = "regDNN", n.hidden = c(10, 10, 10, 10), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN41 <- list(machine = "wclsDNN", n.hidden = c(10, 10, 10, 10), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.DNN25  <- list(machine = "regDNN", n.hidden = c(50, 50), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN25 <- list(machine = "wclsDNN", n.hidden = c(50, 50), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.DNN35  <- list(machine = "regDNN", n.hidden = c(50, 50, 50), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN35 <- list(machine = "wclsDNN", n.hidden = c(50, 50, 50), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.DNN45  <- list(machine = "regDNN", n.hidden = c(50, 50, 50, 50), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN45 <- list(machine = "wclsDNN", n.hidden = c(50, 50, 50, 50), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.DNN20  <- list(machine = "regDNN", n.hidden = c(100, 100), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN20 <- list(machine = "wclsDNN", n.hidden = c(100, 100), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.DNN30  <- list(machine = "regDNN", n.hidden = c(100, 100, 100), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN30 <- list(machine = "wclsDNN", n.hidden = c(100, 100, 100), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.DNN40  <- list(machine = "regDNN", n.hidden = c(100, 100, 100, 100), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN40 <- list(machine = "wclsDNN", n.hidden = c(100, 100, 100, 100), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = FALSE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.RF     <- list(machine = "regRF", ntree = 2000)

cvCtrl.list <- list(list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN21),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN21)),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN31),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN31)),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN41),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN41)),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN25),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN25)),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN35),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN35)),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN45),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN45)),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN20),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN20)),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN30),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN30)),
                    list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = regCtrl.DNN40),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 5, best.opti = TRUE, esCtrl = wclsCtrl.DNN40)),
                    # list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
                    # list(ITR = "itrDOWL", regCtrl = regCtrl.svm, wclsCtrl = wclsCtrl.lsvm),
                    # list(ITR = "itrOWL", wclsCtrl = wclsCtrl.lsvm),
                    # list(ITR = "itrOWL", wclsCtrl = wclsCtrl.LASSO),
                    list(ITR = "itrVT", regCtrl = regCtrl.RF),
                    # list(ITR = "itrVT", regCtrl = regCtrl.LASSO),
                    # list(ITR = "itrVT", regCtrl = regCtrl.svm),
                    # list(ITR = "itrSimple", regCtrl = regCtrl.RF),
                    # list(ITR = "itrSimple", regCtrl = regCtrl.svm),
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



