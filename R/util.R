#' Special Relational Operator
#'
#' @param lhs left hand value
#' @param rhs right hand value
#'
#' @return TRUE if values are equal, FALSE otherwise including treating NA as
#' a value
`%==%` <- function(lhs, rhs) {
  if (is.na(lhs)) {
    return(FALSE)
  } else {
    return(lhs == rhs)
  }
}
