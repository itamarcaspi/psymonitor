#' @title Generate a table with identified bubble/crisis periods
#'
#' @description \code{disp} generates a data frame with bubble/crisis
#'   periods identified by the PSY procedure
#'
#' @param obs   A positive integer. The number of observations.
#' @param OT A date vector. Bubbles/crisis periods identified by the
#'   \code{spymonitor::locate} function.
#'
#' @return A vector of strings with bubble/crisis periods.
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
#' quantilesBsadf <- cvPSYwmboot(y, swindow0, Tb=Tb, nCores = 2)
#' quantile95     <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = dim)
#'
#' # locate bubble/crisis dates
#' ind95        <- (bsadf > t(quantile95[2, ])) * 1
#' monitorDates <- spread$date[swindow0:obs]
#' OT           <- locate(ind95, monitorDates)
#'
#' # Show bubble/crisis periods
#' disp(OT, obs)
#'
#' }

disp <- function(OT, obs) {
  if (is.null(OT)) {
    stop("No bubble periods were found.", call. = FALSE)
  }

  v <- nrow(OT)
  dateStamps <- data.frame(start = NULL, end = NULL)
  rN <- sample(1:obs, v, replace = TRUE)
  for (j in 1:v) {
    if (OT[j, 1] == OT[j, 2]) {
      newEntry <- data.frame(start = OT[j, 1],
                             end = OT[j, 1])
      dateStamps <- rbind(dateStamps, newEntry)

    } else {
      newEntry <- data.frame(start = OT[j, 1],
                             end = OT[j, 2])
      dateStamps <- rbind(dateStamps, newEntry)

    }
  }
  dateStamps
}
