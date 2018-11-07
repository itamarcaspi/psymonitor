#' @title Conduct the new composite bootstrapping for the PSY test.
#'
#' @description  \code{cvPSYwmboot} implements the new bootstrap procedure designed
#'   to detect bubbles and crisis periods while mitigating the potential impact
#'   of heteroskedasticity and to effect family-wise size control in recursive
#'   testing algorithms (Phillips and Shi, forthcoming).
#'
#' @param y   A vector. The data.
#' @param swindow0 A positive integer. Minimum window size (default = \eqn{T
#'   (0.01 + 1.8/\sqrt{T})}, where \eqn{T} denotes the sample size),
#' @param IC  An integer. 0 for fixed lag order (default), 1 for AIC and 2 for
#'   BIC (default = 0).
#' @param adflag  An integer, lag order when IC=0; maximum number of
#'   lags when IC>0 (default = 0).
#' @param Tb A positive integer. The simulated sample size (swindow0+
#'   controlling).
#' @param nboot A positive integer. Number of bootstrap replications (default =
#'   199).
#' @param useParallel Logical. If \code{useParallel=TRUE}, use multi core
#'   computation.
#' @param nCores A positive integer. Optional. If \code{useParallel=TRUE}, the
#'   number of cores defaults to all but one.
#'
#' @return A matrix. BSADF bootstrap critical value sequence at the 90, 95 and
#'   99 percent level.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015a). Testing for
#'   multiple bubbles: Historical episodes of exuberance and collapse in the S&P
#'   500. \emph{International Economic Review}, 56(4), 1034--1078.
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015b). Testing for
#'   multiple bubbles: Limit Theory for Real-Time Detectors. \emph{International
#'   Economic Review}, 56(4), 1079--1134.
#' @references Phillips, P. C. B., & Shi, S.(forthcoming). Real time
#'   monitoring of asset markets: Bubbles and crisis. In Hrishikesh D. Vinod and
#'   C.R. Rao (Eds.), \emph{Handbook of Statistics Volume 41 - Econometrics
#'   Using R}.
#'
#' @export
#'
#' @import doParallel
#' @import parallel
#' @import foreach
#'
#' @examples
#' y <- rnorm(80)
#' cv <- cvPSYwmboot(y, IC = 0, adflag = 1, Tb = 30, nboot = 99, nCores = 1)
#'

cvPSYwmboot <- function(y, swindow0, IC=0, adflag=0, Tb, nboot=199,
                   useParallel=TRUE, nCores) {
  qe    <- as.matrix(c(0.90, 0.95, 0.99))
  nboot <- nboot

  result <- ADFres(y, IC, adflag)
  beta   <- result[[1]]
  eps    <- result[[2]]
  lag    <- result[[3]]

  T0 <- length(eps)
  t  <- length(y)
  dy <- as.matrix(y[2:t] - y[1:(t - 1)])
  g  <- length(beta)

  if (missing(swindow0)) {
    swindow0 <- floor(t * (0.01 + 1.8 / sqrt(t)))
  }

  if (missing(Tb)) {
    stop("Missing a value for 'Tb'", call. = FALSE)
  }

  # The DGP
  set.seed(101)
  rN <- matrix(sample(1:T0, Tb * nboot, replace = TRUE),
               nrow = Tb, ncol = nboot)
  wn <- matrix(rnorm(1), nrow = Tb, ncol = nboot)

  dyb <- matrix(0, nrow = Tb - 1, ncol = nboot)
  dyb[1:lag, ] <- rep(dy[1:lag], times = nboot)

  for (j in 1:nboot) {
    if (lag == 0) {
      for (i in (lag + 1):(Tb - 1)) {
        dyb[i, j] <- wn[i - lag, j] * eps[rN[i - lag, j]]
      }
    }
    else if (lag > 0) {
      x <- matrix(0, nrow = Tb - 1, ncol = lag)
      for (i in (lag + 1):(Tb - 1)) {
        x <- matrix(0, nrow = Tb - 1, ncol = lag)
        for (k in 1:lag) {
          x[i, (k)] <- dyb[(i - k), j]
        }
        dyb[i, j] <- x[i, ] %*% beta[2:g, 1] + wn[i - lag, j] * eps[rN[i - lag, j]]
      }
    }
  }

  yb0  <- rep(y[1], times = nboot)
  dyb0 <- rbind(yb0, dyb)
  yb   <- apply(dyb0, 2, cumsum)


  # The PSY Test ------------------------------------------------------------

  # setup parallel backend to use many processors
  if (useParallel == TRUE && missing(nCores)) {
    nCores <- detectCores() - 1
  } else {
    nCores <- 1
  }
  cl <- makeCluster(nCores)
  registerDoParallel(cl)

  #----------------------------------
  dim  <- Tb - swindow0 + 1
  i <- 0
  MPSY <- foreach(i = 1:nboot, .inorder = FALSE, .combine = rbind) %dopar% {
    PSY(yb[, i], swindow0, IC, adflag)
  }
  #----------------------------------
  stopCluster(cl)

  SPSY   <- as.matrix(apply(MPSY, 1, max))
  Q_SPSY <- as.matrix(quantile(SPSY, qe))
  return(Q_SPSY)
}
