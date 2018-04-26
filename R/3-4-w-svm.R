require(quadprog)

#################################
#### wkSVMObj class
#' An S4 class containing a weighted kernel SVM classfication model
#'
#' @slot alpha Parameter alpha in the fitted model.
#' @slot bias The bias term in the kernel SVM model.
#' @slot fitted A numeric number, indicating the loss on the validation set
#' @slot kernel.f A character string that specifies the kernel function.
#' @slot X Predictors from the training set.
#' @slot Y Response from the training set.
#' @slot support Support vectors from the training set.
#' @slot n.support Number of support vectors.
#' @slot label A character vector of length two, binary class labels.
#'
#' @seealso
#' \code{\link{wClsObj-class}}\cr
#' \code{\link{RegObj-class}}\cr
#' \code{\link{ModelObj-class}}\cr
#' \code{\link{ModelEnsembleObj-class}}\cr
#' \code{\link{cvGrid-class}}\cr
#' \code{\link{cvKfold-class}}\cr
#' \code{\link{workflow-class}}\cr
#' @export
setClass(
  "wkSVMObj",
  slots = list(
    alpha = "numeric",
    bias = "numeric",
    fitted = "numeric",
    kernel.f = "ANY",
    X = "matrix",
    Y = "numeric",
    support = "numeric",
    n.support = "numeric",
    label = "character"
  )
)

#################################
#### wlSVMObj class
#' An S4 class containing a weighted linear SVM classfication model
#'
#' @slot beta Coefficients in the fitted linear SVM model.
#' @slot fitted The bias term in the kernel SVM model.
#' @slot label A character vector of length two, binary class labels.
#'
#' @seealso
#' \code{\link{wClsObj-class}}\cr
#' \code{\link{RegObj-class}}\cr
#' \code{\link{ModelObj-class}}\cr
#' \code{\link{ModelEnsembleObj-class}}\cr
#' \code{\link{cvGrid-class}}\cr
#' \code{\link{cvKfold-class}}\cr
#' \code{\link{workflow-class}}\cr
#' @export
setClass(
  "wlSVMObj",
  slots = list(
    beta = "numeric",
    fitted = "numeric",
    label = "character"
  )
)

###########################################################
### Weighted Linear SVM classification Model

#' Weighted Linear SVM classification Model
#'
#' Fit a Weighted Linear SVM classification Model
#'
#' @param Y A \code{factor} vector with two levels, a binary response.
#' @param X A \code{numeric} matrix, the predictors.
#' @param W A \code{numeric} matrix, the weights for all samples. The default is 1.
#' @param C The coefficient for the regularizer.
#' @param epsilon A minor value added to make the quadratic system solvable.
#'
#' @return Returns a \code{DnnModelObj} object.
#'
#' @seealso
#' \code{\link{DnnModelObj-class}}\cr
#' @export
w.l.svm = function(Y, X, W=NULL, C=1, epsilon=10**-10) {
  p = dim(X)[2]
  n = dim(X)[1]

  if(is.factor(Y)) {

    label <- levels(Y)
    Y <- (Y == label[1])*2 - 1
  }

  if(is.null(W)) {
    W = rep(1/n, n)
  }

  A = t(rbind(cbind(matrix(0, n, p+1), diag(n)),
              cbind(matrix(rep(Y, p+1), n, p+1)*cbind(rep(1, n), X), diag(n))))
  b0 = c(rep(0, n), rep(1, n))
  D = diag(c(0, rep(1, p), rep(0, n))) + epsilon*diag(1+n+p)
  d = -c(rep(0, p+1), W)*C
  qp = quadprog::solve.QP(D, d, A, b0)

  beta = qp$solution
  fit = (as.numeric(cbind(rep(1, n), X) %*% qp$solution[1:(p+1)])>0)*2-1

  # list(beta=beta, fitted=fit)
  methods::new("wlSVMObj", beta = beta, fitted = fit, label = label)
}

w.l.svm.predict = function(model, X) {
  n = dim(X)[1]
  p = dim(X)[2]
  (as.numeric(cbind(rep(1, n), X) %*% model$beta[1:(p+1)])>0)*2-1
}


#' @rdname dwnnel-predict
#' @section Methods (by signature):
#' \code{wlSVMObj}: newData should be a \code{matrix} or \code{data.frame} object. And a table of probabilities will
#'  be returned.
#' @export
setMethod("predict",
          "wlSVMObj",
          function(object, newData, cutoff, ...) {

            pred <- as.numeric(cbind(rep(1, dim(newData)[1]), newData) %*% object@beta[1+0:dim(newData)[2]])
            prob <- cbind(exp(pred), 1) / (exp(pred) + 1)
            colnames(prob) <- object@label
            prob
          }
)

#### weighted kernel svm ####
linear.kernel = function(X, X2=NULL) {
  if(is.null(X2)) {
    X2 = X
  }

  X %*% t(X2)
}

gaussian.kernel = function(X, gamma, X2=NULL) {
  if(is.null(X2)) {
    X2 = X
  }

  n = dim(X)[1]
  n2 = dim(X2)[1]
  out.mat = matrix(NA, n, n2)
  for(i in 1:n) {
    out.mat[i, ] = exp(-rowSums((matrix(rep(X[i, ], each=n2), nrow=n2) - X2)**2)*gamma)
  }
  out.mat
}

poly.kernel = function(X, c, d, X2=NULL) {
  if(is.null(X2)) {
    X2 = X
  }

  n = dim(X)[1]
  n2 = dim(X2)[1]
  out.mat = matrix(NA, n, n2)
  for(i in 1:n) {
    out.mat[i, ] = (rowSums(matrix(rep(X[i, ], each=n2), nrow=n2) * X2) + c)**d
  }
  out.mat
}

sigmoid.kernel = function(X, gamma, c, X2=NULL) {
  if(is.null(X2)) {
    X2 = X
  }

  n = dim(X)[1]
  n2 = dim(X2)[1]
  out.mat = matrix(NA, n, n2)
  for(i in 1:n) {
    out.mat[i, ] = tanh(rowSums(matrix(rep(X[i, ], each=n2), nrow=n2) * X2) * gamma + c)
  }
  out.mat
}


###########################################################
### Weighted Kernel SVM classification Model

#' Weighted Kernel SVM classification Model
#'
#' Fit a Weighted Kernel SVM classification Model
#'
#' @param Y A \code{factor} vector with two levels, a binary response.
#' @param X A \code{numeric} matrix, the predictors.
#' @param W A \code{numeric} matrix, the weights for all samples. The default is 1.
#' @param C The coefficient for the regularizer.
#' @param kernel The kernel function.
#'  gaussian: exp(-gamma*(u-v)^2)
#'  polynomial: (gamma*u*v + c)^d
#'  sigmoid: tanh(gamma*u*v + c)
#' @param gamma Parameter needed for all kernels.
#' @param c Parameter needed for polynomial and sigmoid kernels.
#' @param d Parameter needed for sigmoid kernel.
#'
#' @return Returns a \code{DnnModelObj} object.
#'
#' @seealso
#' \code{\link{DnnModelObj-class}}\cr
#' @export
w.k.svm = function(Y, X, W=NULL, C=1, # epsilon=10**-10,
                   kernel='gaussian', gamma=1/dim(X)[2], c=0, d=3) {
  if(kernel == 'gaussian') {
    kernel.f = function(X, X2=NULL) {gaussian.kernel(X, gamma, X2)}
  } else if(kernel == 'linear') {
    kernel.f = function(X, X2=NULL) {linear.kernel(X, X2)}
  } else if(kernel == 'polynomial') {
    kernel.f = function(X, X2=NULL) {poly.kernel(X, c, d, X2)}
  } else if(kernel == 'sigmoid') {
    kernel.f = function(X, X2=NULL) {sigmoid.kernel(X, gamma, c, X2)}
  } else {
    return('Please choose kernel from gaussian/linear/polynomial/sigmoid')
  }

  if(is.factor(Y)) {

    label <- levels(Y)
    Y <- (Y == label[1])*2 - 1
  }

  p = dim(X)[2]
  n = dim(X)[1]

  if(is.null(W)) {
    W = rep(1, n)
  }

  K = kernel.f(X)
  # Q = K*matrix(rep(Y, n), n, n)*matrix(rep(Y, each=n), n, n)
  # Y.s = Y[-n]/Y[n]
  # A = t(rbind(diag(n-1), -Y.s,
  #             -diag(n-1), Y.s))
  # d = 1 - Y.s
  # D = Q[-n, -n] - Q[n, -n] %*% t(Y.s) - Y.s %*% t(Q[n, -n]) +
  #   Y.s %*% t(Y.s) # + epsilon*diag(n-1) # epsilon*(diag(n-1) + Y.s %*% t(Y.s))
  # b0 = c(rep(0, n), -W)*C
  D <- K*matrix(rep(Y, n), n, n)*matrix(rep(Y, each=n), n, n)
  A <- t(rbind(Y, diag(n), -diag(n)))
  d <- rep(1, n)
  b0 <- c(rep(0, n+1), -W)*C

  qp.res = quadprog::solve.QP(D, d, A, b0, meq = 1)
  # alpha = c(qp.res$solution, -sum(qp.res$solution*Y.s))
  alpha = qp.res$solution

  support = which(abs(alpha-W*C)>10**-10)
  n.support = sum(abs(alpha-W*C)>10**-10)

  bias = mean((Y*alpha)[support])

  fit = (as.numeric(t(alpha*Y) %*% K) + bias)

  # list(alpha=alpha, bias=bias, fitted=fit, kernel.f=kernel.f,
  #      X=X, Y=Y, support=support, n.support=n.support)
  methods::new("wkSVMObj", alpha=alpha, bias=bias, fitted=fit, kernel.f=kernel.f,
               X=X, Y=Y, support=support, n.support=n.support, label = label)
}

w.k.svm.predict = function(model, X) {
  ((as.numeric(t(model$alpha*model$Y) %*% model$kernel.f(model$X, X)) + model$bias)>0)*2-1
}

#' @rdname dwnnel-predict
#' @section Methods (by signature):
#' \code{wkSVMObj}: newData should be a \code{matrix} or \code{data.frame} object. And a table of probabilities will
#'  be returned.
#' @export
setMethod("predict",
          "wkSVMObj",
          function(object, newData, cutoff, ...) {

            pred <- as.numeric(t(object@alpha*object@Y) %*% object@kernel.f(object@X, newData)) + object@bias
            prob <- cbind(exp(pred), 1) / (exp(pred) + 1)
            colnames(prob) <- object@label
            prob
          }
)

#### TEST Linear SVM ####
# n = 1000
# p = 10
# X = matrix(rnorm(n*p), n, p)
# Y = (rowSums(X**3) + rnorm(n)>0)*2-1
# W = rep(1/n, n)
# C = 1
# epsilon = 10**-10
# X.test = matrix(rnorm(n*p), n, p)
# Y.test = (rowSums(X.test**3) + rnorm(n)>0)*2-1
#
# for(i in 0:9) {
#   l.svm.mod = w.l.svm(Y, X, W, C=2**i)
#   print(mean(Y == l.svm.mod$fitted))
#   new.pred = w.l.svm.predict(l.svm.mod, X.test)
#   print(mean(Y.test == new.pred))
# }
#
# k.svm.mod = w.k.svm(Y, X, W=rep(1, n))
# pred = w.k.svm.predict(k.svm.mod, X.test)
# table(k.svm.mod$fitted>0, Y)
# table(pred>0, Y.test)
#
# k.svm.mod2 = k.svm.mod
# k.svm.mod2$bias = 0
# pred2 = w.k.svm.predict(k.svm.mod2, X.test)
# table(pred2>0, Y.test)
#
# library(e1071)
# k.svm.mod3 = svm(X, factor(Y), epsilon=0)
# table(k.svm.mod3$fitted, Y)
# pred3 = predict(k.svm.mod3, X.test)
# table(pred3, Y.test)




