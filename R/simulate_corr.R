#' simulate_corr
#'
#' Simulate variance-covariance matrix and correlation matrix
#' @param pattern a character of desired variance-covariance pattern name
#' @param M a vector of visit points
#' @return a list of variance-covariance matrix and correlation matrix
#' @references Zhai, S., Shen, J. and Mehrotra, D.V. (2016). CTLDA: A Combined Test for Longitudinal Data Analysis in Randomized Clinical Trials.
#' @author Song Zhai
#' @export
#'
simulate_corr <- function(pattern, M){
  m <- length(M)
  if(pattern=="Unstruc"){
    cov_adni <- structure(c(sort(runif(m, min = 40, max = 80)),
                            sort(runif(m, min = 40, max = 80)),
                            sort(runif(m, min = 50, max = 120)),
                            sort(runif(m, min = 50, max = 120)),
                            sort(runif(m, min = 50, max = 190)), .Dim = c(m, m)))

    cor_adni <- cov2cor(cov_adni) %>% round(3)
  }
  if(pattern=="CS"){
    rho <- 0.6
    cor_adni <- rho*matrix(1, m, m) + (1-rho)*diag(m)
    cov_adni <- 80*cor_adni
  }
  if(pattern=="AR1"){
    rho <- 0.8
    cor_adni <- toeplitz(rho^(0:(m-1))) %>% round(3)
    cov_adni <- 80*cor_adni
  }
  if(pattern=="CAR1"){
    a <- 60
    b <- 0.5
    var <- a+b*M^2; var <- sort(var, decreasing = T)

    cov_adni <- toeplitz(var) %>% round(3)
    cor_adni <- cov2cor(cov_adni) %>% round(3)
  }
  if(pattern=="CAR1Exp"){
    a <- 15
    b <- 0.05
    var <- a*exp(2*b*M); var <- sort(var, decreasing = T)

    cov_adni <- toeplitz(var) %>% round(3)
    cor_adni <- cov2cor(cov_adni) %>% round(3)
  }

  re <- list(cov=cov_adni, cor=cor_adni)
  re
}

