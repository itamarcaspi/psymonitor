#' @title Estimate the ADF model under the null
#'
#' @description \code{ADFres} estimates the ADF model under the null with lag
#'   order selected by AIC or BIC
#'
#' @param y   A Vector. Data.
#' @param IC  An integer, 0 for fixed lag order (default), 1 for AIC and 2 for
#'   BIC.
#' @param adflag  An integer. Lag order when IC=0; maximum number of lags when
#'   IC>0 (default = 0).
#'
#' @return Numeric, ADF test statistic.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015a). Testing for
#'   multiple bubbles: Historical episodes of exuberance and collapse in the S&P
#'   500. \emph{International Economic Review}, 56(4), 1034--1078. Phillips, P.
#'   C. B., Shi, S., & Yu, J. (2015b). Testing for multiple bubbles: Limit
#'   Theory for Real-Time Detectors. \emph{International Economic Review},
#'   56(4), 1079--1134.
#'
#'


ADFres <- function(y, IC, adflag) {
  T0    <- length(y)
  T1    <- length(y) - 1
  const <- rep(1,T1)

  dy <- y[2:T0] - y[1:T1]
  x1 <- data.frame(const)

  t <- T1 - adflag
  if (IC > 0) {
    ICC   <- matrix(0,nrow = adflag+1,ncol=1)
    betaM <- matrix(list(), nrow=adflag+1,ncol=1)
    epsM  <- matrix(list(), nrow=adflag+1,ncol=1)

    for (k in 0:adflag){
      # model Specification
      xx<-matrix(x1[(k+1):T1,])  #@-from k+1 to the end (including y1 and x)-@
      dy01<-matrix(dy[(k+1):T1]) #@-from k+1 to the end (including dy0)-@
      if (k>0){
        x2<-cbind(xx,matrix(0,nrow=T1-k,ncol=k))
        for (j in 1:k){
          x2[,ncol(xx)+j]<-dy[(k+1-j):(T1-j)] #@-including k lag variables of dy in x2-@
        }
      }else x2<-xx
      #OLS regression
      betaM[k+1] <- list(solve(t(x2)%*%x2) %*% (t(x2)%*%dy01)) #@-model A-@
      epsM[[k+1]] <- dy01-x2%*%as.matrix(betaM[[k+1]])
      # Information Criteria
      npdf <- sum(-1/2*log(2*pi)-1/2*(epsM[[k+1]]^2))
      if (IC==1){       #@ AIC @
        ICC[k+1] <- -2*npdf/t+2*length(betaM[[k+1]])/t
      }else if(IC==2){ #@ BIC @
        ICC[k+1] <- -2*npdf/t+length(betaM[[k+1]])*log(t)/t
      }
    }
    lag0 <- which.min(ICC)
    beta<-betaM[[lag0]]
    eps<-epsM[[lag0]]
    lag<-lag0-1

  }else if(IC==0){
    # Model Specification
    xx <- matrix(x1[(adflag+1):T1,])    #@-from k+1 to the end (including y1 and x)-@
    dy01 <- matrix(dy[(adflag+1):T1])   #@-from k+1 to the end (including dy0)-@

    if (adflag>0){
      x2<-cbind(xx, matrix(0,nrow=t,ncol=adflag))
      for (j in 1:adflag){
        x2[,ncol(xx)+j]<-dy[(adflag+1-j):(T1-j)] # @-including k lag variables of dy in x2-@
      }
    }else x2 <- xx
    # OLS Regression
    beta <- solve(t(x2)%*%x2) %*% (t(x2)%*%dy01) #@-model A-@
    eps <- dy01-x2%*%beta
    lag<-adflag
  }
  result<-list(beta=beta,eps=eps,lag=lag)
  return(result)
}


