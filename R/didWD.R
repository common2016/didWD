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
#' @param wcontinuous A character, the continuous treatment variable's name. Default is \code{NULL}.
#' @param pretime Wheather add \eqn{dq\cdot ft, t < q} as independent variables. Default is \code{FALSE}.
#' @import magrittr
#'
#' @return A list including 3 elements, the first is results of regression with Two ways fixed effect,
#' the second is the data frame \eqn{ft}, and the third is the data frame \eqn{dq}.
#'
#' @export
#' @examples
#' library(plm)
#' data(stg6, package = 'didWD')
#' fit <- didWD(stg6, id = 'id', year = 'year', y = 'logy', w = 'w')
#' lmtest::coeftest(fit$fit, vcov. = vcovHC, method = 'white2')
#' # aggregate treat effect
#' xname <- names(coef(fit$fit))
#' aggeff(fit$fit, xname)
#'
didWD <- function(dt, id, year, y, w, wcontinuous = NULL, pretime = FALSE){
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
      if (pretime){# include periods before treatments
        if (!is.null(wcontinuous)){# a continue treatment variable
          ifelse(fyr >= dyr,
                 ans <- c(ans, paste(wcontinuous,paste('d',dyr, sep = ''), paste('f', fyr, sep = ''),sep = ':')),
                 ans <- c(ans, paste(paste('d',dyr, sep = ''), paste('f', fyr, sep = ''),sep = ':')))
        } else {
          ans <- c(ans, paste(paste('d',dyr, sep = ''), paste('f', fyr, sep = ''),sep = ':'))
        }
      } else {# after treatment
        if (fyr >= dyr){
          if (!is.null(wcontinuous)){# a continue treatment variable
            ans <- c(ans, paste(wcontinuous,paste('d',dyr, sep = ''), paste('f', fyr, sep = ''),sep = ':'))
          } else {
            ans <- c(ans, paste(paste('d',dyr, sep = ''), paste('f', fyr, sep = ''),sep = ':'))
          }
        }
      }
    }
  }
  fml <- eval(parse(text = paste(y, paste(ans, collapse = '+'), sep = '~')))
  # browser()

  # TWFE
  fit <- plm::plm(fml, index = c(id, year), effect = 'twoways',model = 'within', data = dt)
  return(list(fit = fit, ft = ft, dq = dq, pretime = pretime))
}
