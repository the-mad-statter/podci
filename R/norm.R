# nolint start: line_length_linter.

#' PODCI Normative Values
#'
#' @param scale requested scale
#' @param patient subject of the assessment (pediatric vs adolescent)
#' @param reporter person providing the responses (parent vs self)
#' @param stat normative mean or standard deviation
#'
#' @return the requested normative value
#' @export
#'
#' @examples
#' ## adolescent happiness parent-report normative mean
#' podci_norms("hapy", "ado")
#'
#' ## pediatric sports normative standard deviation
#' podci_norms("sprt", stat = "s")
#'
#' ## adolescent pain self-report normative standard deviation
#' podci_norms("pain", "ado", "self", "s")
#'
podci_norms <- function(
    scale = c("upex", "tran", "sprt", "pain", "hapy", "glob"),
    patient = c("ped", "ado"),
    reporter = c("prnt", "self"),
    stat = c("m", "s")) {
  scale <- match.arg(scale)
  patient <- match.arg(patient)
  reporter <- match.arg(reporter)
  stat <- match.arg(stat)

  dplyr::case_when(
    scale == "upex" && patient == "ped" && stat == "m" ~ 91.91380,
    scale == "upex" && patient == "ped" && stat == "s" ~ 11.49680,
    scale == "upex" && patient == "ado" && reporter == "prnt" && stat == "m" ~ 98.73970,
    scale == "upex" && patient == "ado" && reporter == "prnt" && stat == "s" ~ 5.03207,
    scale == "upex" && patient == "ado" && reporter == "self" && stat == "m" ~ 98.73970,
    scale == "upex" && patient == "ado" && reporter == "self" && stat == "s" ~ 5.03207,
    scale == "tran" && patient == "ped" && stat == "m" ~ 98.33460,
    scale == "tran" && patient == "ped" && stat == "s" ~ 5.69935,
    scale == "tran" && patient == "ado" && reporter == "prnt" && stat == "m" ~ 99.12600,
    scale == "tran" && patient == "ado" && reporter == "prnt" && stat == "s" ~ 4.64180,
    scale == "tran" && patient == "ado" && reporter == "self" && stat == "m" ~ 99.12600,
    scale == "tran" && patient == "ado" && reporter == "self" && stat == "s" ~ 4.64180,
    scale == "sprt" && patient == "ped" && stat == "m" ~ 92.44210,
    scale == "sprt" && patient == "ped" && stat == "s" ~ 10.21742,
    scale == "sprt" && patient == "ado" && reporter == "prnt" && stat == "m" ~ 93.66770,
    scale == "sprt" && patient == "ado" && reporter == "prnt" && stat == "s" ~ 10.93022,
    scale == "sprt" && patient == "ado" && reporter == "self" && stat == "m" ~ 93.66770,
    scale == "sprt" && patient == "ado" && reporter == "self" && stat == "s" ~ 10.93022,
    scale == "pain" && patient == "ped" && stat == "m" ~ 92.45370,
    scale == "pain" && patient == "ped" && stat == "s" ~ 13.76303,
    scale == "pain" && patient == "ado" && reporter == "prnt" && stat == "m" ~ 87.59010,
    scale == "pain" && patient == "ado" && reporter == "prnt" && stat == "s" ~ 17.19338,
    scale == "pain" && patient == "ado" && reporter == "self" && stat == "m" ~ 87.59010,
    scale == "pain" && patient == "ado" && reporter == "self" && stat == "s" ~ 17.19338,
    scale == "hapy" && patient == "ped" && stat == "m" ~ 89.79410,
    scale == "hapy" && patient == "ped" && stat == "s" ~ 14.10834,
    scale == "hapy" && patient == "ado" && reporter == "prnt" && stat == "m" ~ 81.64890,
    scale == "hapy" && patient == "ado" && reporter == "prnt" && stat == "s" ~ 17.80298,
    scale == "hapy" && patient == "ado" && reporter == "self" && stat == "m" ~ 81.64890,
    scale == "hapy" && patient == "ado" && reporter == "self" && stat == "s" ~ 17.80298,
    scale == "glob" && patient == "ped" && stat == "m" ~ 93.87860,
    scale == "glob" && patient == "ped" && stat == "s" ~ 7.39539,
    scale == "glob" && patient == "ado" && reporter == "prnt" && stat == "m" ~ 94.77970,
    scale == "glob" && patient == "ado" && reporter == "prnt" && stat == "s" ~ 7.37844,
    scale == "glob" && patient == "ado" && reporter == "self" && stat == "m" ~ 94.77970,
    scale == "glob" && patient == "ado" && reporter == "self" && stat == "s" ~ 7.37844,
    TRUE ~ NA_real_
  )
}

# nolint end
