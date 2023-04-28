# nolint start: cyclocomp_linter.

#' PODCI Scale Item Numbers
#'
#' @param scale requested scale
#' @param patient subject of the assessment (pediatric vs adolescent)
#' @param reporter person providing the responses (parent vs self)
#' @param type the type of item (ignored if not sport scale):
#' * primary are items from which the scale score is directly computed
#' * secondary are items for which the scoring of primary items depends
#' * both gives both primary and secondary item numbers
#'
#' @return a vector of scale items
#' @export
#'
#' @examples
#' ## all scales have the same item numbers except:
#'
#' ### the sport scale items differ depending on reporter
#' ### and has primary and secondary items
#'
#' podci_items("sprt", reporter = "prnt", type = "both")
#'
#' podci_items("sprt", reporter = "prnt", type = "primary")
#'
#' podci_items("sprt", reporter = "prnt", type = "secondary")
#'
#' podci_items("sprt", reporter = "self", type = "both")
#'
#' podci_items("sprt", reporter = "self", type = "primary")
#'
#' podci_items("sprt", reporter = "self", type = "secondary")
#'
#' ### the pain scale items differ depending on reporter
#'
#' podci_items("pain", reporter = "prnt")
#'
#' podci_items("pain", reporter = "self")
#'
#' ### the global scale items differ depending on reporter
#'
#' podci_items("glob", reporter = "prnt")
#'
#' podci_items("glob", reporter = "self")
#'
podci_items <- function(
    scale = c("upex", "tran", "sprt", "pain", "hapy", "glob"),
    patient = c("ped", "ado"),
    reporter = c("prnt", "self"),
    type = c("both", "primary", "secondary")) {
  scale <- match.arg(scale)
  patient <- match.arg(patient)
  reporter <- match.arg(reporter)
  type <- match.arg(type)

  if (scale == "sprt" && type == "both") {
    return(sort(c(
      podci_items("sprt", patient, reporter, "primary"),
      podci_items("sprt", patient, reporter, "secondary")
    )))
  } else {
    dplyr::case_when(
      scale == "upex" ~
        list(c(1:6, 8, 32)),
      scale == "tran" ~
        list(c(7, 21, 24:25, 28:31, 33:35)),
      scale == "sprt" && reporter == "prnt" && type == "primary" ~
        list(c(18:20, 22:23, 26:27, 36, 44, 52, 60, 66)),
      scale == "sprt" && reporter == "prnt" && type == "secondary" ~
        list(c(42:43, 50:51, 58:59, 65, 72:73)),
      scale == "sprt" && reporter == "self" && type == "primary" ~
        list(c(18:20, 22:23, 26:27, 36, 43, 50, 57, 63)),
      scale == "sprt" && reporter == "self" && type == "secondary" ~
        list(c(42, 49, 56, 62, 69:70)),
      scale == "pain" && reporter == "prnt" ~
        list(c(17, 75:76)),
      scale == "pain" && reporter == "self" ~
        list(c(17, 72:73)),
      scale == "hapy" ~
        list(c(10:14)),
      scale == "glob" && reporter == "prnt" ~
        list(c(1:86)),
      scale == "glob" && reporter == "self" ~
        list(c(1:83)),
      TRUE ~
        list(c(NA_real_))
    ) %>%
      unlist()
  }
}

# nolint end
