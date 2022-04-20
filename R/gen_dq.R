#' Generate \eqn{d_q}
#'
#'
#' @param dt A data.frame.
#' @param id A index of indivuals.
#' @param w A dummy variable which equals \eqn{D_i\cdot T_i} where \eqn{D_i=1} indicates treated group,
#' and \eqn{T-i=1} indicates in the treated periods.
#' @param stgyr A numerica vector about staggered years.
#'
#' @return A data.frame includes \eqn{d_q} dummy variables.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data(stg6)
#' gen_dq(stg6, id, w, stgyr = 2013:2016)
#'
gen_dq <- function(dt, id, w, stgyr){
  dt <- dplyr::group_by(dt, id) %>% dplyr::summarise(wsum = sum(w)) %>% dplyr::left_join(dt,.data, by = 'id')
  n <- max(stgyr) - min(stgyr) + 1
  ans <- lapply(n:1, function(i) as.integer(dt$wsum == i)) %>% dplyr::bind_cols() %>% suppressMessages()
  # rename
  # entry_yr <- stgyr - min(dt$year) + 1
  names(ans) <- paste('d', as.character(stgyr), sep = '')
  dt <- cbind(dt, ans)
  return(dt)
}
