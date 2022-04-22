#' Generate \eqn{fs_t}
#'
#'
#' @param dt A data.frame.
#' @param tm A coloumn name of the time variable.
#' @param stgyr A numerica vector about staggered years. For example, \code{2014:2016},
#' the function would generate \code{f2014,f2015,f2016} three dummy variables.
#'
#' @return A data.frame including time dummy variables.
#' @export
#'
#' @examples
#' data(stg6)
#' gen_fst(stg6, tm = 'year', stgyr = 2014:2016)
#'
gen_fst <- function(dt, tm, stgyr = 2014:2016){
  ans <- lapply(stgyr, function(year) as.numeric(dt[,tm] == year)) %>%
    dplyr::bind_cols() %>% suppressMessages()
  names(ans) <- paste('f', as.character(stgyr), sep = '')
  # dt <- cbind(dt, ans)
  return(ans)
}
