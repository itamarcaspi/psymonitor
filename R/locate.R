#' @title Locate bubble/crisis periods
#'
#' @description \code{locate} locate bubble/crisis periods with non-zero bubble indicator
#'
#' @param index A vector. A dummy variable that equals 1 for a bubble/crisis
#'   period and 0 otherwise.
#' @param dates A vector. Dates of the time series.
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


locate <- function(index, dates) {
  maxi <- max(index)
  lc <- which.max(index)

  if (maxi == 1) { # there is at least one episode
    count <- 1
    EP <- matrix(0, nrow = 30, ncol = 2)
    # maximum 20 episodes: col1 origination date col2 termination date
    i <- lc + 1
    EP[count, 1] <- dates[lc]
    while (i <= length(index)) {
      if (index[i - 1] == 1 && index[i] == 0) {
        EP[count, 2] <- dates[i - 1]
        i <- i + 1
      } else if (index[i - 1] == 0 && index[i] == 1) {
        count <- count + 1
        EP[count, 1] <- dates[i]
        i <- i + 1
      } else {
        i <- i + 1
      }
    }
    OT <- EP[1:count, ]
    
      if(length(OT) == 2){
 OT <- matrix()
 OT <- rbind(EP[1:count,])
    }
    
    v <- nrow(OT)
    if (OT[v, 2] == 0) {
      OT[v, 2] <- dates[length(dates)]
    }
    OT <- as.Date(OT, origin = "1970-01-01")
  } else if (maxi == 0) {
    OT <- NULL
    warning("No bubble or crisis periods found", call. = FALSE)
  }

  return(OT)
}
