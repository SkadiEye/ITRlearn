# #!/usr/bin/env Rscript
# library("optparse")
# 
# option_list = list(
#   make_option(c("-s", "--seed"), type="character", default="out",
#               help="seed", metavar="character")
# );
# 
# opt_parser = OptionParser(option_list=option_list);
# opt = parse_args(opt_parser);

library(ITRlearn)
library(dnnet)
library(methods)
library(MASS)
set.seed(1357)
n.effect <- 6
beta1 <- runif(n.effect)*2-1
beta2 <- runif(n.effect)*2-1

# set.seed(opt$seed)
ind <- paste(rep(0:1, each = 100), rep(rep(0:9, each = 10), 2), rep(0:9, 20), sep = '')
setwd("C:/Users/xlmi/Dropbox/PersonalizedMedicine/simulationResults/iter/")

for(ind.i in ind) {
  
  set.seed(ind.i)
  
  n.sample = 1000
  n.p = 100
  rho = 1
  sigma = matrix(0, n.p, n.p)
  for(i in 1:dim(sigma)[1]) for(j in 1:dim(sigma)[2]) sigma[i, j] = exp(-rho*abs(i-j))
  X <- mvrnorm(2*n.sample, rep(0, n.p), sigma)
  Y1 <- as.numeric(rowSums(X[, 1:5]) + (X[, 6]**2 + X[, 7]**2 - X[, 9]**2 - X[, 10]**2))
  Y2 <- as.numeric(rowSums(X[, 1:5]) - (X[, 6]**2 + X[, 7]**2 - X[, 9]**2 - X[, 10]**2))
  label <- c("A", "B")
  sim.dat <- impoorIdeal(X = X, trtResp1 = Y1, trtResp2 = Y2, label = label)
  
  trt.dat <- generateTrtData(sim.dat, inc.cutoff = 1, epsilon = 1)
  split.dat <- splitData(trt.dat, 0.5)
  train.dat <- split.dat$trainObj
  test.dat <- split.dat$validObj
  
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
                           regCtrl = list(machine = "ensemble", n.ensemble = 3, best.opti = TRUE,
                                          esCtrl = append(regCtrl.DNN, list(n.hidden = c(30, 30, 30))), 
                                          parallel = FALSE),
                           wclsCtrl = list(machine = "ensemble", n.ensemble = 3, best.opti = TRUE,
                                           esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(30, 30, 30))), 
                                           parallel = FALSE)),
                      list(ITR = "itrDOWL",
                           regCtrl = list(machine = "ensemble", n.ensemble = 4, best.opti = TRUE,
                                          esCtrl = append(regCtrl.DNN, list(n.hidden = c(30, 30, 30))), 
                                          parallel = FALSE),
                           wclsCtrl = list(machine = "ensemble", n.ensemble = 4, best.opti = TRUE,
                                           esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(30, 30, 30))), 
                                           parallel = FALSE)),
                      list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
                      list(ITR = "itrSimple", regCtrl = regCtrl.LASSO))
  
  print(ind.i)
  (a <- Sys.time())
  wfmodl <- workflow(train.dat, validObj = test.dat, cvCtrl.list = cvCtrl.list)
  (b <- Sys.time() - a)
  print(wfmodl)
  
  write.csv(wfmodl@performances, paste(ind.i, ".34.performance.csv", sep=''))
}


