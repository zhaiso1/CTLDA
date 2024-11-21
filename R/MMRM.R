#' MMRM
#'
#' MMRM (last-visit)
#' @param dat a data frame of standard longitudinal data
#' @param duration a numerical value of study duration
#' @return a numeric list including fitted linear mixed effects model and contrast test results
#' @references Detry, M.A. and Ma, Y. (2016). Analyzing repeated measurements using mixed models. Jama, 315(4), 407-408.
#' @author Song Zhai
#' @export
#'
MMRM <- function(dat, duration){
  mmrm <- lme(y ~ 0 + act_vis,
              random = ~ 0 + M | id,
              data = dat,
              correlation = corSymm(form = ~ as.numeric(visit) | id),
              control=lmeControl(maxIter=300, msMaxIter=300, niterEM=100))

  mod = contrast::contrast(mmrm,
                           a = list('act_vis' = paste0("pbo.",duration)),
                           b = list('act_vis' = paste0("act.",duration)))


  p <- mod$Pvalue; names(p) <- NULL
  re <- list(mmrm=mmrm, contrast=mod)
  re
}
