#' Common Trend Test
#'
#'
#' @param didfit the object returned by \code{didWD} whose parameters \code{pretime} is \code{TRUE}.
#' @import magrittr
#'
#' @return F statistics testing HO: there is a common trend.
#'
#' @export
#' @examples
#' library(plm)
#' data(stg6, package = 'didWD')
#' fit <- didWD(stg6, id = 'id', year = 'year', y = 'logy', w = 'w', pretime = TRUE)
#' cttest(fit)
#'
cttest <- function(didfit){
  fit <- didfit$fit
  dq <- didfit$dq
  ft <- didfit$ft

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
  return(Fstats)
}
