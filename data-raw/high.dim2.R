library(MASS)
set.seed(10086)
n.sample = 500
n.p = 1000
rho = 1
sigma = matrix(0, n.p, n.p)
for(i in 1:dim(sigma)[1]) for(j in 1:dim(sigma)[2]) sigma[i, j] = exp(-rho*abs(i-j))
X <- mvrnorm(2*n.sample, rep(0, n.p), sigma)
beta1 <- runif(10)*2-1
beta2 <- runif(10)*2-1
Y1 <- as.numeric(X[, 1:10*5] %*% beta1 + X[, 6]**2*X[, 7]**2) +
  as.numeric(X[, 1:10*5-1] %*% beta2 + 2*(abs(X[, 12]) - X[, 1]**2)*X[, 3])
Y2 <- as.numeric(X[, 1:10*5] %*% beta1 + X[, 6]**2*X[, 7]**2) -
  as.numeric(X[, 1:10*5-1] %*% beta2 + 2*(abs(X[, 12]) - X[, 1]**2)*X[, 3])

A <- runif(2*n.sample) > 0.5
A.true <- Y1 > Y2
Y <- ifelse(A, Y1, Y2) + rnorm(n.sample*2)
dat <- importDnnet(X[1:n.sample, ], Y[1:n.sample])
split.dat <- splitDnnet(dat, "bootstrap")
test <- importDnnet(X[n.sample + 1:n.sample, ], Y[n.sample + 1:n.sample])


set.seed(1000)
(a <- Sys.time())
dnn.mod <- dnnet(split.dat$train, validate = split.dat$valid,
                 norm.x = TRUE, norm.y = TRUE,
                 activate = "elu", n.hidden = c(10, 10, 10),
                 l1.reg = 10**-4, l2.reg = 0, n.batch = 10, n.epoch = 250,
                 early.stop = TRUE, early.stop.det = 100, plot = TRUE, accel = "none",
                 learning.rate.adaptive = "adagrad")
(b <- Sys.time() - a)
(a <- Sys.time())
dnn.mod <- dnnet(split.dat$train, validate = split.dat$valid,
                 norm.x = FALSE, norm.y = FALSE,
                 activate = "elu", n.hidden = c(100, 100, 100), learning.rate = 10**-4,
                 l1.reg = 10**-5, l2.reg = 0, n.batch = 50, n.epoch = 100,
                 early.stop = TRUE, early.stop.det = 5, plot = TRUE, accel = "none",
                 learning.rate.adaptive = "constant")
(b <- Sys.time() - a)
dnn.mod@loss
var(split.dat$valid@y)

plot(predict(dnn.mod, split.dat$valid@x), split.dat$valid@y)

plot(predict(dnn.mod, split.dat$valid@x), split.dat$valid@y)
plot(predict(dnn.mod, test@x), test@y)
n.ensemble <- 10
pred.table <- matrix(NA, n.sample, n.ensemble)
ref <- numeric(n.ensemble)
for(i in 1:n.ensemble) {

  split.dat <- splitDnnet(dat, "bootstrap")
  dnn.mod <- dnnet(split.dat$train, validate = split.dat$valid,
                   norm.x = TRUE, norm.y = TRUE,
                   activate = "elu", n.hidden = c(10, 10, 10),
                   l1.reg = 10**-4, l2.reg = 0, n.batch = 10, n.epoch = 250,
                   early.stop = TRUE, early.stop.det = 100, plot = TRUE, accel = "none",
                   learning.rate.adaptive = "adagrad")
  pred.table[, i] <- predict(dnn.mod, dat@x)
  ref[i] <- mean((mean(split.dat$valid@y) - split.dat$valid@y)**2) -
    mean((predict(dnn.mod, split.dat$valid@x) - split.dat$valid@y)**2)
}

mse <- numeric(n.ensemble-1)
for(j in 1:(n.ensemble-1)) {

  mse[j] <- mean((rowMeans(pred.table[, ref >= sort(ref)[j]]) - dat@y)**2)
}
min(mse)
y.pred <- rowMeans(pred.table[, ref >= sort(ref)[which.min(mse)]])
plot(dat@y ~ y.pred)
abline(0, 1)
plot((Y1[1:n.sample] + Y2[1:n.sample])/2 ~ y.pred)
abline(0, 1)

E.Y <- abs(dat@y - y.pred)
A.f <- A[1:n.sample] * (dat@y > y.pred) + (1-A[1:n.sample]) * (dat@y <= y.pred)
table(A.true[1:n.sample], A.f)
mean(A.true[1:n.sample] == A.f)
lbl <- factor(ifelse(A.f, "A", "B"))
dat0 <- importDnnet(X[1:n.sample, ], lbl, E.Y)
split.dat0 <- splitDnnet(dat0, "bootstrap")
(a <- Sys.time())
dnn.mod <- dnnet(split.dat0$train, validate = split.dat0$valid,
                 norm.x = TRUE, norm.y = FALSE,
                 activate = "elu", n.hidden = c(10, 10, 10),
                 l1.reg = 10**-4, l2.reg = 0, n.batch = 10, n.epoch = 250,
                 early.stop = TRUE, early.stop.det = 100, plot = TRUE, accel = "none",
                 learning.rate.adaptive = "adagrad")
(b <- Sys.time() - a)
dnn.mod <- dnnet(split.dat0$train, validate = split.dat0$valid,
                 norm.x = FALSE, norm.y = FALSE,
                 activate = "elu", n.hidden = c(10, 10, 10), learning.rate = 0.01,
                 l1.reg = 10**-4, l2.reg = 0, n.batch = 10, n.epoch = 1000,
                 early.stop = TRUE, early.stop.det = 100, plot = TRUE, accel = "none",
                 learning.rate.adaptive = "constant")
hist(predict(dnn.mod, test@x)[, 1])
boxplot(predict(dnn.mod, test@x)[, 1])
table(predict(dnn.mod, test@x)[, 1] > 0.5, A.true[n.sample + 1:n.sample])
mean((predict(dnn.mod, test@x)[, 1] > 0.5) == A.true[n.sample + 1:n.sample])






