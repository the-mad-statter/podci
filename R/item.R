#' PODCI Scale Item Numbers
#'
#' @param scale requested scale
#' @param type the type of item (ignored if not sport scale):
#' * primary are items from which the scale score is directly computed
#' * secondary are items for which the scoring of primary items depends
#' * both gives both primary and secondary item numbers
#'
#' @return a vector of scale items
#' @export
#'
#' @examples
#' ## upper extremity item numbers
#' podci_items("upex", "both")
#'
#' ## sport primary and secondary item numbers
#' podci_items("sprt", "both")
#'
podci_items <- function(
    scale = c("upex", "tran", "sprt", "pain", "hapy"),
    type = c("both", "primary", "secondary")) {
  scale <- match.arg(scale)
  type <- match.arg(type)

  if (scale == "sprt" && type == "both") {
    return(sort(c(
      podci_items("sprt", "primary"),
      podci_items("sprt", "secondary")
    )))
  } else {
    dplyr::case_when(
      scale == "upex" ~
        list(c(1:6, 8, 32)),
      scale == "tran" ~
        list(c(7, 21, 24:25, 28:31, 33:35)),
      scale == "sprt" && type == "primary" ~
        list(c(18:20, 22:23, 26:27, 36, 44, 52, 60, 66)),
      scale == "sprt" && type == "secondary" ~
        list(c(42:43, 50:51, 58:59, 65, 72:73)),
      scale == "pain" ~
        list(c(17, 75:76)),
      scale == "hapy" ~
        list(c(10:14)),
      TRUE ~
        list(c(NA_real_))
    ) %>%
      unlist()
  }
}
