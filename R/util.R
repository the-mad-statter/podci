#' Special Relational Operator
#'
#' @param lhs left hand value
#' @param rhs right hand value
#'
#' @return TRUE if values are equal, FALSE otherwise (treats `NA` as a value)
`%==%` <- function(lhs, rhs) {
  if (is.na(lhs) && !is.na(rhs)) {
    FALSE
  } else if (!is.na(lhs) && is.na(rhs)) {
    FALSE
  } else if (is.na(lhs) && is.na(rhs)) {
    TRUE
  } else {
    return(lhs == rhs)
  }
}

#' Round PODCI Output for Tests
#'
#' @param data a [dplyr::tibble] of test data
#'
#' @inheritParams base::round
#'
#' @seealso [round()]
#'
#' @return a [dplyr::tibble] of rounded PODCI outputs
round_podci_output_for_tests <- function(data, digits = 2) {
  data %>%
    dplyr::select(dplyr::starts_with("podci")) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ round(., digits = digits)
      )
    )
}
