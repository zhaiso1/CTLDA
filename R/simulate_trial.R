#' simulate_trial
#'
#' Simulate trial
#' @param n_arm a numerical value of sample size per arm
#' @param M a numerical vector of visit points
#' @param mean_pbo a numerical vector of mean responses at visit points in placebo arm
#' @param mean_act a numerical vector of mean responses at visit points in active arm
#' @param cov a matrix of variance-covariance matrix
#' @param sd a numerical value of standard deviation
#' @param adjust an indicator of whether or not to adjust mean responses
#' @return a data frame of simulated longitudinal data
#' @references Zhai, S., Shen, J. and Mehrotra, D.V. (2016). CTLDA: A Combined Test for Longitudinal Data Analysis in Randomized Clinical Trials.
#' @author Song Zhai
#' @export
#'
simulate_trial <- function(n_arm = 200,
                           M = c(0, 3, 6, 9, 12),
                           mean_pbo,
                           mean_act,
                           cov,
                           sd,
                           adjust = TRUE) {

  ## Simulate data
  cov_sqrt <- t(chol(cov)) # square root of covariance
  m <- length(M)
  y_pbo <- cov_sqrt %*% matrix(rnorm(m * n_arm, sd = sd), nrow = m) + mean_pbo
  y_act <- cov_sqrt %*% matrix(rnorm(m * n_arm, sd = sd), nrow = m) + mean_act

  dat <- data.frame(id = rep(1:(2 * n_arm), each = m), # Patient IDs
                    visit = rep(1:m, 2 * n_arm), # Visits
                    M = rep(M, n_arm), # Months since baseline
                    y = c(as.numeric(y_pbo), as.numeric(y_act)), # Outcome measure
                    trt = factor(rep(c('pbo', 'act'), each = n_arm * m)), # Treatment arm
                    act = rep(c(0, 1), each = n_arm * m)) # Dummy treatment arm
  dat$mod_trt <- dat$trt
  dat$mod_trt[dat$M == 0] <- 'pbo' # Cast
  dat$act <- as.numeric(dat$trt == 'act')
  dat$act_vis <- with(dat, interaction(mod_trt, M))
  dat$id <- factor(dat$id, levels = unique(dat$id))

  ## Adjustment
  if(adjust == TRUE){
    for (i in c("act","pbo")) {
      for (j in 1:length(M)) {
        index <- which(dat$trt == i & dat$visit == j & is.na(dat$y) == FALSE)

        if(i == "act"){target <- mean_act[j]}
        if(i == "pbo"){target <- mean_pbo[j]}

        dat$y[index] <- dat$y[index] - mean(dat$y[index]) + target
      }
    }
  }

  dat$visit <- factor(dat$visit)
  return(dat)
}










