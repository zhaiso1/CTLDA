#' const.delay.m12
#'
#' PMRM (constant delay)
#' @param dat a data frame of standard longitudinal data
#' @return a list of fitted progression model for repeated measures and p-value
#' @references Raket, L.L. (2022). Progression models for repeated measures: Estimating novel treatment effects in progressive diseases. Statistics in Medicine, 41(28), 5537-5557.
#' @author Song Zhai
#' @export
#'
const.delay.m12 <- function(dat){
  months <- unique(dat$M)

  xx = dat %>% filter(trt=="pbo") %>% group_by(visit) %>% summarise(y_mean=mean(y))
  y.pbo <- xx$y_mean
  xx = dat %>% filter(trt=="act") %>% group_by(visit) %>% summarise(y_mean=mean(y))
  y.trt <- xx$y_mean

  y.pbo.x <- spline_nonpara(ys.train = y.pbo, xs.train = months, xs.test = months)
  y.trt.x <- spline_nonpara(ys.train = y.trt, xs.train = months, xs.test = months)

  # pull out placebo arm parameters
  start_vec <- c(y.pbo.x, b = 0)

  # fit model
  t_pmrm <- tryCatch({
    gnls(model = y ~ f.const.delay.m12(M, v0, v1, v2, v3, v4, b),
         data = dat,
         params = list(v0 + v1 + v2 + v3 + v4 ~ 1,
                       b ~ act + 0),
         correlation = corSymm(form = ~ 1 | id),
         weights = varIdent(form = ~ 1 | visit),
         start = start_vec,
         control = gnlsControl(nlsTol = 1, maxIter = 500, tolerance = 1))}, error=function(e){
           "error"
         })

  # test delay
  if(t_pmrm[1] != "error"){
    test = anova(t_pmrm)
    re <- list(mod=t_pmrm, p=test$`p-value`[length(test$`p-value`)])
  } else{
    re <- list(mod=t_pmrm, p=NA)
  }

  re
}


