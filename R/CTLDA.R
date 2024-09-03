#' CTLDA
#'
#' CTLDA: A Combined Test for Longitudinal Data Analysis in Randomized Clinical Trials using Cauchy Combination Test
#' @param pvec a vector a p-values to be combined
#' @return a numerical value of p-value from Cauchy Combination Test
#' @references Zhai, S., Shen, J. and Mehrotra, D.V. (2016). CTLDA: A Combined Test for Longitudinal Data Analysis in Randomized Clinical Trials.
#' @author Song Zhai
#' @export
#'
CTLDA <- function(pvec){
  ACAT(Pvals = pvec)
}

