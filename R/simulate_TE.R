#' simulate_TE
#'
#' Simulate treatment effect pattern
#' @param f time-to-response function in the placebo arm
#' @param M a numerical vector of visit points
#' @param pattern a character of desired treatment effect pattern
#' @return a numerical vector of mean responses at visit points in the active arm
#' @references Zhai, S., Shen, J. and Mehrotra, D.V. (2016). CTLDA: A Combined Test for Longitudinal Data Analysis in Randomized Clinical Trials.
#' @author Song Zhai
#' @export
#'
simulate_TE <- function(f, M, pattern){
  if(pattern == 0){ # null hypothesis
    mean_trt <- f(M)
  }

  if(pattern == 1){ # proportional delay
    mean_trt <- f(0.6*M)
  }
  if(pattern == 2){ # constant delay
    mean_trt <- f(M-4)
  }
  if(pattern == 3){ # increasing delay
    delay0 <- rep(2, sum(M <= 4))
    delay1 <- linspace(2, 4, sum(M > 4))
    delay <- c(delay0, delay1)

    mean_trt <- f(M-delay)
  }
  if(pattern == 4){ # fading delay
    delay0 <- rep(5, sum(M <= 5))
    delay1 <- linspace(5, 3, sum(M > 5))
    delay <- c(delay0, delay1)

    mean_trt <- f(M-delay)
  }

  if(pattern == 5){ # proportional decline
    mean_trt <- 0.6*(f(M) - f(0))+f(0)
  }
  if(pattern == 6){ # constant decline
    reduction0 <- linspace(0, 0.4, sum(M <= 6))
    reduction1 <- rep(0.4, sum(M > 6))
    reduction <- c(reduction0, reduction1)

    mean_trt <- f(M) - reduction
  }
  if(pattern == 7){ # increasing decline
    reduction0 <- linspace(0, 0.4, sum(M <= 6))
    reduction1 <- linspace(0.4, 0.45, sum(M > 6))
    reduction <- c(reduction0, reduction1)

    mean_trt <- f(M) - reduction
  }
  if(pattern == 8){ # fading decline
    reduction0 <- linspace(0, 0.45, sum(M <= 6))
    reduction1 <- linspace(0.45, 0.3, sum(M > 6))
    reduction <- c(reduction0, reduction1)

    mean_trt <- f(M) - reduction
  }

  mean_trt
}
