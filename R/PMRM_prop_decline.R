#' prop.decline.m12
#'
#' PMRM (proportional decline)
#' @param dat a data frame of standard longitudinal data
#' @return a list of fitted progression model for repeated measures and p-value
#' @references Raket, L.L. (2022). Progression models for repeated measures: Estimating novel treatment effects in progressive diseases. Statistics in Medicine, 41(28), 5537-5557.
#' @author Song Zhai
#' @export
#'
prop.decline.m12 <- function(dat){
  months <- unique(dat$M)

  xx = dat %>% filter(trt=="pbo") %>% group_by(visit) %>% dplyr::summarize(y_mean=mean(y))
  y.pbo <- xx$y_mean
  xx = dat %>% filter(trt=="act") %>% group_by(visit) %>% dplyr::summarize(y_mean=mean(y))
  y.trt <- xx$y_mean

  y.pbo.x <- spline_nonpara(ys.train = y.pbo, xs.train = months, xs.test = months)
  y.trt.x <- spline_nonpara(ys.train = y.trt, xs.train = months, xs.test = months)

  ## initialize reduced decline to average observed
  b_init <- 1 - mean(y.trt.x[-1]/y.pbo.x[-1])

  ## pull out placebo arm parameters
  start_vec <- c(y.pbo.x, b = b_init)

  ## fit model
  pd_pmrm <- tryCatch({
    gnls(model = y ~ f.prop.decline.m12(M, v0, v1, v2, v3, v4, b),
         data = dat,
         params = list(v0 + v1 + v2 + v3 + v4 ~ 1,
                       b ~ act + 0),
         correlation = corSymm(form = ~ 1 | id),
         weights = varIdent(form = ~ 1 | visit),
         start = start_vec,
         control = gnlsControl(nlsTol = 1, maxIter = 500, tolerance = 1))}, error=function(e){
           "error"
         })

  # test decline
  if(pd_pmrm[1] != "error"){
    test = anova(pd_pmrm)
    re <- list(mod=pd_pmrm, p=test$`p-value`[length(test$`p-value`)])
  } else{
    re <- list(mod=pd_pmrm, p=NA)
  }

  re
}

f.prop.decline.m12 <- function(t, v0, v1, v2, v3, v4, b) {
  months <- seq(0, 12, 3)

  v <- c(v0[1], v1[1], v2[1], v3[1], v4[1])
  bx <- cbind(0, b, b, b, b)

  (1 - bx[cbind(1:length(t), match(t, months))]) * (v[match(t, months)] - v[1]) + v[1]
}




