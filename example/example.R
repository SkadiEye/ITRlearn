#### Install "dnnet" and "ITRlearn" ####
library(devtools)
install_github("SkadiEye/dnnet")
install_github("SkadiEye/ITRlearn")

#### Load packages required -- install from CRAN if necessary
library(ITRlearn)
library(dnnet)
library(MASS)
library(ROCR)
library(methods)

n_sample <- 1000
n_p <- 10
x <- mvrnorm(2*n_sample, rep(0, n_p), diag(n_p))
y1 <- as.numeric(rowSums(x[, 1:5]) + (x[, 6] + x[, 7]**2 - x[, 10]**2))
y0 <- as.numeric(rowSums(x[, 1:5]) - (x[, 6] + x[, 7]**2 - x[, 10]**2))
z <- factor(ifelse(rbinom(2*n_sample, 1, 0.5), "A", "B"), levels = c("A", "B"))
y <- (z == levels(z)[1])*y1 + (z == levels(z)[2])*y0
best_itr <- factor(ifelse(x[, 6] + x[, 7]**2 - x[, 10]**2 > 0, "A", "B"))

dat <- importData(X = x, trtResp = y, trtLabl = z)
dat_split <- splitData(dat, 0.5)
training <- dat_split$trainObj
testing <- dat_split$validObj

#### Endlot ####
endlot_mod <- EndLot(training, n.hidden = c(10, 10), n.ensemble = 10, n.epoch = 250,
                     learning.rate.adaptive = "adam", activate = "relu")
pred_endlot <- predict(endlot_mod, testing)
mean(best_itr[-dat_split$split] == pred_endlot@pred)

#### Owl-lasso ####
owl_lasso_mod <- OWL_LASSO(training)
pred_owl_lasso <- predict(owl_lasso_mod, testing)
mean(best_itr[-dat_split$split] == pred_owl_lasso@pred)

#### lasso ####
lasso_mod <- LASSO(training)
pred_lasso <- predict(lasso_mod, testing)
mean(best_itr[-dat_split$split] == pred_lasso@pred)

#### VT (E) ####
vt_mod <- VT_Ensemble(training)
pred_vt <- predict(vt_mod, testing)
mean(best_itr[-dat_split$split] == pred_vt@pred)


