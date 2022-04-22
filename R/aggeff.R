#' Aggrgate Treatment Effects
#'
#' @param fit A object returned by \code{plm::plm}.
#' @param xname a character vector including names of independent variables.
#' @param se methods computing standard error. \code{'white2'} indicates s.e. clustered in
#' individuals, \code{'white1'} indicate common White s.e., and \code{'arellano'} means
#' the vcov matrix is block diagonal. See the help of \code{plm::vcovHC}.
#' @export
#'
#' @return a numeric vector including mean, standard error, t value and p value.
#'
aggeff <- function(fit, xname, se = 'white2'){
  # get mean
  ans <- stats::coef(fit)[names(stats::coef(fit)) %in% xname]
  mu <- mean(ans)

  # get se
  covmt <- plm::vcovHC(fit, method = se)
  ans <- colnames(covmt) %in% xname
  covmt <- covmt[ans,ans]
  a <- matlab::ones(nrow(covmt),1)/nrow(covmt)
  se <- sqrt((t(a) %*% covmt %*% a))
  tvalue <- mu/se
  prob <- 2*(1 - stats::pt(abs(tvalue), fit$df.residual))
  return(c(mu = mu, se = se, tvalue = tvalue, prob = prob))
}
