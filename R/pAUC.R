#' pAUC
#'
#' pAUC (partial AUC): pAUC measures the treatment effect as the area between active and placebo time-to-response curves over the post-baseline visits
#' @param dat a data frame of standard longitudinal data
#' @param start a numerical value of starting visit point to calculate the area
#' @param nboot number of bootstraps
#' @return a list of the observed area, null distribution of the area, and the p-value
#' @references Atri, A., Hendrix, S.B., Pejovic, V., et al. (2015). Cumulative, additive benefits of memantine-donepezil combination over component monotherapies in moderate to severe Alzheimer’s dementia: a pooled area under the curve analysis. Alzheimer's Research & Therapy, 7, 1-12.
#' @author Song Zhai
#' @export
#'
pAUC <- function(dat, start, nboot = 1000){
  dat1 <- dat; dat1$id <- as.numeric(dat1$id)
  months <- unique(dat1$M)

  xx = dat1 %>% filter(trt=="pbo") %>% group_by(visit) %>% dplyr::summarize(y_mean=mean(y))
  y.pbo <- xx$y_mean
  xx = dat1 %>% filter(trt=="act") %>% group_by(visit) %>% dplyr::summarize(y_mean=mean(y))
  y.trt <- xx$y_mean

  area.obs = area(months, y.pbo, y.trt, start = start)

  areax <- double()
  for (k in 1:nboot) {
    id_trt <- sample(unique(dat1$id), length(unique(dat1$id))/2, replace = FALSE)

    datx <- dat1; datx$trt <- "pbo"

    datx$trt[which(datx$id %in% id_trt)] <- "act"

    xx = datx %>% filter(trt=="pbo") %>% group_by(visit) %>% dplyr::summarize(y_mean=mean(y))
    y.pbo <- xx$y_mean

    xx = datx %>% filter(trt=="act") %>% group_by(visit) %>% dplyr::summarize(y_mean=mean(y))
    y.trt <- xx$y_mean

    areax = c(areax, area(months, y.pbo, y.trt, start = start))
  }

  pxx=mean(areax > area.obs); if(pxx==0){pxx <- 1/nboot}

  re <- list(obs = area.obs, dist=areax, p=pxx)
  re
}

area <- function(months, y1, y2, start){
  K <- length(months)-1
  SA <- 0
  for (k in start:K) {
    h <- months[k+1] - months[k]
    lwidth <- abs(y1[k]-y2[k])
    rwidth <- abs(y1[k+1]-y2[k+1])
    ss <- (lwidth + rwidth)*h/2
    SA <- SA + ss
  }
  SA
}


