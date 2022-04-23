#' Estimate Dynamic Effect
#'
#'
#' @param didfit The object returned by \code{didWD} whose parameters \code{pretime} is \code{TRUE}.
#' @param se \code{'white1','white2','arellano'}. See the help of \code{plm::vcovHC}.
#' @param type \code{"HC0", "sss", "HC1", "HC2", "HC3", "HC4"}. See the help of \code{plm::vcovHC}.
#'
#' @return a list. The 1st element is dynamice effect statistics, and the 2nd element is
#' independent variables collected.
#' @export
#'
dyneff <- function(didfit, se = 'white2', type = 'sss'){
  fit <- didfit$fit
  dq <- didfit$dq
  ft <- didfit$ft

  # dynamic effect
  dyncoef <- vector('list')
  xname <- names(stats::coef(fit))
  for (i in names(dq)) {
    for (j in names(ft)) {
      dyear <- stringr::str_remove(i,'d') %>% as.numeric()
      fyear <- stringr::str_remove(j, 'f') %>% as.numeric()
      invdf <- fyear - dyear
      ans <- xname[stringr::str_detect(xname, i)]
      dyncoef[[as.character(invdf)]] <- c(dyncoef[[as.character(invdf)]],ans[stringr::str_detect(ans,j)])
    }
  }
  # browser()
  dynstats <- lapply(dyncoef, aggeff, fit = fit, se = se, type = type) %>% dplyr::bind_rows()
  # sort
  dynstats$time <- names(dyncoef) %>% as.numeric()
  dynstats <- dplyr::select(dynstats, .data$time, dplyr::everything()) %>% dplyr::arrange(.data$time)
  return(list(dynstats = dynstats, dyncoef = dyncoef))
}



