#' @title Locate bubble/crisis periods
#'
#' @description \code{locate} locate bubble/crisis periods with non-zero bubble indicator
#'
#' @param ind A vector. A dummy variable that equals 1 for a bubble/crisis
#'   period and 0 otherwise.
#' @param index A vector. Dates of the time series.
#'
#' @return A vector. Dates identified as bubbles or crisis.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015a). Testing for
#'   multiple bubbles: Historical episodes of exuberance and collapse in the S&P
#'   500. \emph{International Economic Review}, 56(4), 1034--1078.
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015b). Testing for
#'   multiple bubbles: Limit Theory for Real-Time Detectors. \emph{International
#'   Economic Review}, 56(4), 1079--1134.
#' @references * Phillips, P. C. B., & Shi, S.(forthcoming). Real time
#'   monitoring of asset markets: Bubbles and crisis. In Hrishikesh D. Vinod and
#'   C.R. Rao (Eds.), \emph{Handbook of Statistics Volume 41 - Econometrics
#'   Using R}.
#'
#' @export
#'
#'
#' @examples
#' \donttest{
#' data(spread)
#'
#' y        <- spread$value
#' obs      <- length(y)
#' swindow0 <- floor(obs*(0.01 + 1.8/sqrt(obs)))
#' dim      <- obs - swindow0 + 1
#' Tb       <- 24 + swindow0 - 1
#'
#' # Estimate PSY statistics and CVs
#' bsadf          <- PSY(y, swindow0)
#' quantilesBsadf <- wmboot(y, swindow0, Tb=Tb)
#'
#' #' monitorDates <- spread$date[swindow0:obs]
#' quantile95 <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = dim)
#' ind95      <- (bsadf > t(quantile95[2, ])) * 1
#'
#' # locate bubble/crisis dates
#' monitorDates <- spread$date[swindow0:obs]
#'
#'
#' locate(ind95, monitorDates)
#'
#'
#' }


locate <- function(ind, date) {
  maxi <- max(ind)
  lc <- which.max(ind)

  if (maxi == 1) { # there is at least one episode
    count <- 1
    EP <- matrix(0, nrow = 30, ncol = 2)
    # maximum 20 episodes: col1 origination date col2 termination date
    i <- lc + 1
    EP[count, 1] <- date[lc]
    while (i <= length(ind)) {
      if (ind[i - 1] == 1 && ind[i] == 0) {
        EP[count, 2] <- date[i - 1]
        i <- i + 1
      } else if (ind[i - 1] == 0 && ind[i] == 1) {
        count <- count + 1
        EP[count, 1] <- date[i]
        i <- i + 1
      } else {
        i <- i + 1
      }
    }
    OT <- EP[1:count, ]
    v <- nrow(OT)
    if (OT[v, 2] == 0) {
      OT[v, 2] <- date[length(date)]
    }
    OT <- as.Date(OT, origin = "1970-01-01")
  } else if (maxi == 0) {
    OT <- NULL
  }

  return(OT)
}
