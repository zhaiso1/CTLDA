#' f.unstruct.delay.m12
#'
#' PMRM (unstructured delay) core function
#' @param t time
#' @param v0 mean response at visit 0 in the placebo arm
#' @param v1 mean response at visit 1 in the placebo arm
#' @param v2 mean response at visit 2 in the placebo arm
#' @param v3 mean response at visit 3 in the placebo arm
#' @param v4 mean response at visit 4 in the placebo arm
#' @param b1 parameter of interest
#' @param b2 parameter of interest
#' @param b3 parameter of interest
#' @param b4 parameter of interest
#' @return mean response in the active arm
#' @references Raket, L.L. (2022). Progression models for repeated measures: Estimating novel treatment effects in progressive diseases. Statistics in Medicine, 41(28), 5537-5557.
#' @author Song Zhai
#' @export
#'
f.unstruct.delay.m12 <- function(t, v0, v1, v2, v3, v4, b1, b2, b3, b4) {
  months <- seq(0, 12, 3)

  b <- cbind(0, b1, b2, b3, b4)
  t_out <- t - b[cbind(1:length(t), match(t, months))]

  spline(x = months, y = c(v0[1], v1[1], v2[1], v3[1], v4[1]), method = 'natural', xout = t_out)$y
}
