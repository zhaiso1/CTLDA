#' spline_nonpara
#'
#' Spline function
#' @param ys.train mean responses in the training set
#' @param xs.train visit points in the trauning set
#' @param xs.test visit points in the testing set
#' @return mean responses in the testing set
#' @references Raket, L.L. (2022). Progression models for repeated measures: Estimating novel treatment effects in progressive diseases. Statistics in Medicine, 41(28), 5537-5557.
#' @author Song Zhai
#' @export
#'
spline_nonpara <- function(ys.train, xs.train, xs.test){
  fit <- smooth.spline(x = xs.train, y = ys.train)
  ypred = predict(fit, xs.test)$y
  ypred
}
