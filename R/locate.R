#' @title Locate periods with non-zero bubble indicator
#'
#' @description
#'
#' @param ind A vector. A dummy variabel that equals 1 for a bubble/crisis
#'   period and 0 otherwise.
#' @param index A vector. Dates of the time series.
#'
#' @return A vector. Dates identified as bubbles or crisis.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015a). Testing for
#'   multiple bubbles: Historical episodes of exuberance and collapse in the S&P
#'   500. \emph{International Economic Review}, 56(4), 1034--1078. Phillips, P.
#'   C. B., Shi, S., & Yu, J. (2015b). Testing for multiple bubbles: Limit
#'   Theory for Real-Time Detectors. \emph{International Economic Review},
#'   56(4), 1079--1134.
#'
#' @export
#'
#'
#' @examples
#' \donttest{
#' y <- rnorm(100)
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
