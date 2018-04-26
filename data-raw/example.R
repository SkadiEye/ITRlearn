library(MASS)
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

trt.dat <- generateTrtData(sim.dat, inc.cutoff = 1, epsilon = 1)
split.dat <- splitData(trt.dat, 0.5)
train.dat <- split.dat$trainObj
train.dat
test.dat <- split.dat$validObj
test.dat

regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:50)))
regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.5**(0:10)))
wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.5**(0:10)))
wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.5**(0:10)))
regCtrl.DNN    <- list(machine = "regDNN", n.hidden = c(10, 10, 10), n.batch = 100, n.epoch = 1000, activate = "elu",
                       l1.reg = 10**-4, plot = TRUE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN   <- list(machine = "wclsDNN", n.hidden = c(10, 10, 10), n.batch = 100, n.epoch = 500, activate = "elu",
                       l1.reg = 10**-4, plot = TRUE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.RF     <- list(machine = "regRF", ntree = 2000)

cvCtrl.list <- list(list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE, esCtrl = regCtrl.DNN),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 100, best.opti = TRUE, esCtrl = wclsCtrl.DNN)),
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

(a <- Sys.time())
wfmodl <- workflow(train.dat, validObj = test.dat, cvCtrl.list = cvCtrl.list)
(b <- Sys.time() - a)
wfmodl
