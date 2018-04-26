load("C:/Users/xlmi/Dropbox/PersonalizedMedicine/CCLEdata/Response/CCLE_response_v3.RData")
load("C:/Users/xlmi/Dropbox/PersonalizedMedicine/CCLEdata/geneExp/CCLE_exp.RData")

set.seed(10086)

resp07 = CCLE_response[[12]]
resp15 = CCLE_response[[19]]

resp = merge(resp07, resp15[, -c(2, 9:11)], all=FALSE, by='CCLE.Cell.Line.Name')
resp.full = merge(resp, CCLE_exp, all=FALSE, by.x='CCLE.Cell.Line.Name', by.y='CCLEcellline')
X = resp.full[, -c(1:8, 12:17)]

vars = apply(X[, -(1:3)], 2, var)
genes = order(vars, decreasing = TRUE)[1:500]
trainX = cbind(factor(X[, 1]), factor(X[, 2]), factor(X[, 3]), X[, genes+3])

gender = matrix(0, dim(resp.full)[1], length(levels(factor(X[, 1]))))
tissue = matrix(0, dim(resp.full)[1], length(levels(factor(X[, 2]))))
histology = matrix(0, dim(resp.full)[1], length(levels(factor(X[, 3]))))
for(i in 1:dim(resp.full)[1]) {
  gender[i, factor(X[, 1])[i]] = 1
  tissue[i, factor(X[, 2])[i]] = 1
  histology[i, factor(X[, 3])[i]] = 1
}

cv = function(x) {
  sd(x)/mean(x)
}

cv.X = apply(X[, -(1:3)], 2, cv)
cv.filter = which(cv.X > 0.3)
cv.filter.X = as.matrix(scale(cbind(gender, tissue, histology, X[, cv.filter + 3])))

# A = rbinom(dim(resp.full)[1], 1, 0.5)
# A.true = (resp.full$ActArea.x > resp.full$ActArea.y)
# Y = resp.full$ActArea.x * A + resp.full$ActArea.y * (1-A)
n.sample <- dim(cv.filter.X)[1]
resample <- sample(n.sample)
cv.filter.X <- cv.filter.X[resample, ]
# Y <- Y[resample]
# A <- A[resample]
# A.true <- A.true[re.sample]
colnames(cv.filter.X) <- NULL

label <- c("A", "B")
sim.dat <- impoorIdeal(X = cv.filter.X,
                       trtResp1 = resp.full$ActArea.x[resample],
                       trtResp2 = resp.full$ActArea.y[resample], label = label)
trt.dat <- generateTrtData(sim.dat, inc.cutoff = 1, epsilon = 0)


regCtrl.LASSO  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regLASSO"))
wclsCtrl.LASSO <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclsLASSO", lambda = 0.8**(0:40)))
regCtrl.svm    <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "regSVR", kernel = "linear", cost = 0.5**(0:10)))
wclsCtrl.ksvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclskSVM", kernel = "gaussian", C = 0.5**(0:10)))
wclsCtrl.lsvm  <- list(machine = "cvGrid", k.fold = 5, gsCtrl = list(machine = "wclslSVM", C = 0.5**(0:10)))
regCtrl.DNN    <- list(machine = "regDNN", n.batch = 10, n.epoch = 200, activate = "elu", accel = "rcpp", norm.x = FALSE,
                       l1.reg = 10**-4, plot = TRUE , learning.rate.adaptive = "adagrad", early.stop.det = 100)
wclsCtrl.DNN   <- list(machine = "wclsDNN", n.batch = 10, n.epoch = 200, activate = "elu", accel = "rcpp", norm.x = FALSE,
                       l1.reg = 10**-4, plot = TRUE, learning.rate.adaptive = "adagrad", early.stop.det = 100)
regCtrl.RF     <- list(machine = "regRF", ntree = 2000)

cvCtrl.list <- list(list(ITR = "itrDOWL",
                         regCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                        esCtrl = append(regCtrl.DNN, list(n.hidden = c(30, 30, 30)))),
                         wclsCtrl = list(machine = "ensemble", n.ensemble = 200, best.opti = TRUE,
                                         esCtrl = append(wclsCtrl.DNN, list(n.hidden = c(30, 30, 30))))),
                    list(ITR = "itrDOWL", regCtrl = regCtrl.LASSO, wclsCtrl = wclsCtrl.LASSO),
                    list(ITR = "itrVT", regCtrl = regCtrl.RF),
                    list(ITR = "itrSimple", regCtrl = regCtrl.LASSO))

wfmodl <- cvKfold(trt.dat, k.fold = 10, cvCtrl.list = cvCtrl.list)

wfmodl

pred.corr <- matrix(NA, length(cvCtrl.list), n.sample)
pred.corr[, resample] <- wfmodl@pred.error

write.csv(pred.corr, paste(opt$seed, ".pred.corr.csv", sep=''))





