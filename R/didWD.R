#' DID Regression
#'
#' @description Make a DID regression conditional on staggered treatment based on Wooldridge (2021).
#'
#' @param dt A data.frame including \code{id}, \code{year}, \code{y} and \code{w}.
#' @param id A coloumn name denotes individuals.
#' @param year A coloumn name denotes times. \code{dt$year} is numeric.
#' @param y A coloumn name denotes the dependent variable.
#' @param w A dummy variable which equals \eqn{D_i\cdot T_i} where \eqn{D_i=1} indicates treated group,
#' and \eqn{T-i=1} indicates in the treated periods.
#'
#' @import magrittr
#'
#' @return A list including 2 elements, one is results of regression with Two ways fixed effect,
#' another is data frame added \eqn{d_q} and \eqn{f_t}.
#'
#' @export
#' @examples
#' library(plm)
#' data(stg6, package = 'didWD')
#' fit <- didWD(stg6, id = 'id', year = 'year', y = 'logy', w = 'w')
#' lmtest::coeftest(fit$fit, vcov. = vcovHC, method = 'white2')
#'
didWD <- function(dt, id, year, y, w){
  stgyr <- dt$year[dt$w!=0] %>% unique()

  # generate fq
  dt <- gen_fst(dt, year, stgyr)

  # generate dq
  dt <- gen_dq(dt, id, w, stgyr)

  # generate formula
  allyr <- unique(dt[,year])
  calendar <- allyr[allyr >= min(stgyr)]
  ans <- NULL
  for (fyr in stgyr) {
    for (dyr in calendar) {
      if (fyr >= dyr){
        ans <- c(ans, paste(paste('d',dyr, sep = ''), paste('f', fyr, sep = ''),sep = ':'))
      }
    }
  }
  fml <- eval(parse(text = paste(y, paste(ans, collapse = '+'), sep = '~')))

  # TWFE
  fit <- plm::plm(fml, index = c(id, year), effect = 'twoways',model = 'within', data = dt)
  return(list(fit = fit, data = dt))
}
