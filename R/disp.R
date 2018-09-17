#' @title Display bubble/crisis periods
#'
#' @description TBA
#'
#' @param OT A vector. Dates identified as bubbles/crisis by the
#'   \code{spymonitor::locate} function.
#'
#' @return A vector of strings with bubble/crisis periods.
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
#' y  <- rnorm(100)
#' }

disp <- function(OT) {
  t <- length(y)
  v <- nrow(OT)
  rN <- sample(1:t, v, replace = T)
  for (j in 1:v) {
    if (OT[j, 1] == OT[j, 2]) {
      print(strtrim(OT[j, 1], 7))
    } else {
      strs <- c(strtrim(OT[j, 1], 7), strtrim(OT[j, 2], 7))
      print(strs)
    }
  }
}
