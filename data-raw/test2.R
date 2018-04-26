xxxx <- function(object, n) {

  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  x <- foreach::foreach(i = 1:n) %dopar% {

    cat(i)
    1
  }
  foreach::registerDoSEQ()
  parallel::stopCluster(cl)

  x
}

cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)
parallel::clusterExport(cl, list("splitData", "getSplit"))
xxxx <- function(object) splitData(object, 0.5)
snow::parLapply(cl, list(trt.dat, trt.dat, trt.dat, trt.dat), xxxx)
foreach::registerDoSEQ()
parallel::stopCluster(cl)


library(doSNOW)
library(tcltk)
library(randomForest)
cl <- makeSOCKcluster(3)
registerDoSNOW(cl)

ntasks <- 100
pb <- tkProgressBar(max=ntasks)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)

x <- matrix(runif(500), 100)
y <- gl(2, 50)

rf <- foreach(ntree=rep(25, ntasks), .combine=combine,
              .multicombine=TRUE, .packages='randomForest',
              .options.snow=opts) %dopar% {
                randomForest(x, y, ntree=ntree)
              }
