#' @title Estimate the ADF statistic
#'
#' @description \code{ADF} calculates the augmented Dickey-Fuller (ADF) test
#'   statistic with lag order set fixed or selected by AIC or BIC.
#'
#' @param y   A vector, The data.
#' @param IC  A positive integer. 0 for fixed lag order 1 for AIC and 2 for BIC
#'   (default = 0).
#' @param adflag  A positive integer. Lag order when IC=0; maximum number of
#'   lags when IC>0 (default = 0).
#'
#' @return Numeric, ADF test statistic.
#'
#' @references Said, S. E., & Dickey, D. A. (1984). Testing for Unit Roots in
#'   ARMA Models of Unknown Order. \emph{Biometrika}, 71(1984), 599--607.
#'
#' @export
#'
#'
#' @examples
#' \donttest{
#' y <- rnorm(100)
#' ADFstat <- ADF(y,  IC = 0, adflag = 1)
#' }

ADF <- function(y, IC=0, adflag=0) {
  T0    <- length(y)
  T1    <- length(y) - 1
  const <- rep(1,T1)

  y1 <- as.matrix(y[1:T1])
  dy <- as.matrix(y[2:T0] - y[1:T1])
  x1 <- as.matrix(cbind(y1,const))

  t   <- T1 - adflag
  dof <- t - 2

  if (IC > 0) {
    ICC <- matrix(0,nrow = adflag + 1,ncol = 1)
    ADF <- matrix(0,nrow = adflag + 1,ncol = 1)
    for (k in 0:adflag) {
      # Model specification
      xx   <- x1[(k + 1):T1,]  #@-from k+1 to the end (including y1 and x)-@
      dy01 <- as.matrix(dy[(k + 1):T1,]) #@-from k+1 to the end (including dy0)-@

      if (k > 0) {
        x2 <- cbind(xx, matrix(0,nrow = T1 - k,ncol = k))
        for (j in 1:k) {
          x2[,ncol(xx) + j] <- dy[(k + 1 - j):(T1 - j)] #@-including k lag variables of dy in x2-@
        }
      } else x2 <- xx

      # OLS regression
      beta <- solve(t(x2) %*% x2) %*% (t(x2) %*% dy01)   #@-model A-@ ### need work
      eps  <- dy01 - x2 %*% beta
      # Information Criteria
      npdf <- sum(-1/2*log(2*pi) - 1/2*(eps^2))
      if (IC == 1) {       #@ AIC @
        ICC[k + 1,] <- -2*npdf/t + 2*nrow(beta)/t
      } else if (IC == 2) {  #@ BIC @
        ICC[k + 1,] <- -2*npdf/t + nrow(beta)*log(t)/t
      }
      se <- t(eps) %*% eps/dof
      sig <- sqrt(diag(matrix(se,nrow(beta),nrow(beta))*solve(t(x2) %*% x2)))
      ADF[k + 1,] <- beta[1,]/sig[1]
    }
    lag0 <- which.min(ICC)
    ADFlag <- ADF[lag0,]
    #lag=lag0-1;
  } else if (IC == 0) {
    # Model Specification
    xx <- x1[(adflag + 1):T1,]        #@-from k+1 to the end (including y1 and x)-@
    dy01 <- as.matrix(dy[(adflag + 1):T1,]) #@-from k+1 to the end (including dy0)-@

    if (adflag > 0) {
      x2 <- cbind(xx,matrix(0,t,adflag))
      for (j in 1:adflag) {
        x2[,ncol(xx) + j] <- dy[(adflag + 1 - j):(T1 - j)] #@-including k lag variables of dy in x2-@
      }
    } else x2 <- xx

    # OLS regression
    beta <- solve(t(x2) %*% x2) %*% (t(x2) %*% dy01)   #@-model A-@
    eps <- dy01 - x2 %*% beta
    se <- t(eps) %*% eps/dof
    sig <- sqrt(diag(matrix(se,nrow(beta),nrow(beta))*solve(t(x2) %*% x2)))
    ADFlag <- beta[1,]/sig[1]
  }

  if (IC == 0) {
    result <- list('fixed lag of order 1' = ADFlag)
  }
  if (IC == 1) {
    result <- list('ADF Statistic using AIC' = ADFlag)
  }
  if (IC == 2) {
    result <- list('ADF Statistic using BIC' = ADFlag)
  }
  return(result)
}
