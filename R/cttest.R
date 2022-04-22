#' DID Regression
#'
#' @description Make a DID regression conditional on staggered treatment based on Wooldridge (2021).
#'
#' @inheritParams didWD
#'
#' @import magrittr
#'
#' @return A list including 4 elements, the first is results of regression with Two ways fixed effect,
#' the second is the data frame \eqn{f_t}, the third is the data frame \eqn{d_q},
#' and the fourth is the F statistics testing HO: there is a common trend.
#'
#' @export
#' @examples
#' library(plm)
#' data(stg6, package = 'didWD')
#' fit <- cttest(stg6, id = 'id', year = 'year', y = 'logy', w = 'w')
#' fit$Fstats
#'
cttest <- function(dt, id, year, y, w, wcontinuous = NULL){
  stgyr <- dt$year[dt$w!=0] %>% unique() %>% sort()
  allyr <- unique(dt[,year]) %>% sort() %>% `[`(-1)

  # generate fq
  ft <- gen_fst(dt, year, allyr)

  # generate dq
  dq <- gen_dq(dt, id, w, stgyr)
  dt <- cbind(dt, ft, dq)

  # generate formula
  calendar <- allyr[allyr >= min(stgyr)]
  ans <- NULL
  for (fyr in allyr) {
    for (dyr in calendar) {
      ans <- c(ans, paste(paste('d',dyr, sep = ''), paste('f', fyr, sep = ''),sep = ':'))
    }
  }
  if (!is.null(wcontinuous)) ans <- paste(wcontinuous, ':', ans, sep = '')
  fml <- eval(parse(text = paste(y, paste(ans, collapse = '+'), sep = '~')))
  # browser()

  # TWFE
  fit <- plm::plm(fml, index = c(id, year), effect = 'twoways',model = 'within', data = dt)
  # common trend test
  ctcoef <- NULL
  xname <- names(stats::coef(fit))
  for (i in names(dq)) {
    for (j in names(ft)) {
      dyear <- stringr::str_remove(i,'d') %>% as.numeric()
      fyear <- stringr::str_remove(j, 'f') %>% as.numeric()
      if (fyear < dyear){
        ans <- xname[stringr::str_detect(xname, i)]
        ctcoef <- c(ctcoef, ans[stringr::str_detect(ans,j)])
      }
    }
  }
  Fstats <- car::linearHypothesis(fit, ctcoef, vcov. = plm::vcovHC, test = 'F')
  return(list(fit = fit, ft = ft, dq = dq, Fstats = Fstats))
}
