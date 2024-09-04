#' ncs_ranslp_df2
#'
#' Natural Cubic Spline
#' @param dat a data frame of standard longitudinal data
#' @param intercept an indicator whether or not to include intercept
#' @return a numeric list including fitted natural cubic spline model and Wald test results
#' @references Donohue, M.C., Langford, O., Insel, P.S., et al. (2023). Natural cubic splines for the analysis of Alzheimer's clinical trials. Pharmaceutical statistics, 22(3), 508-519
#' @author Song Zhai
#' @export
#'
ncs_ranslp_df2 <- function(dat, intercept = TRUE){
  ## spline functions
  ns21 <- function(t){
    as.numeric(predict(splines::ns(dat$M, df=2), t)[,1])
  }
  ns22 <- function(t){
    as.numeric(predict(splines::ns(dat$M, df=2), t)[,2])
  }

  if(intercept == TRUE){fit <- lmer(y ~ I(ns21(M)) + I(ns22(M)) + (I(ns21(M)) + I(ns22(M))):act + (M | id), dat)}
  if(intercept == FALSE){fit <- lmer(y ~ I(ns21(M)) + I(ns22(M)) + (I(ns21(M)) + I(ns22(M))):act - 1 + (M | id), dat)}

  ## Wald test
  robust_cov <- vcovCR(fit, cluster = dat$id, type = "CR2")
  if(intercept == TRUE){cons <- matrix(c(0,0,0,1,0,
                                         0,0,0,0,1), 2, 5, byrow = T)}
  if(intercept == FALSE){cons <- matrix(c(0,0,1,0,
                                          0,0,0,1), 2, 4, byrow = T)}
  ss = Wald_test(fit, constraints = cons, vcov = robust_cov)

  re <- list(mod=fit, wald.test=ss)
  re
}

