# nolint start: cyclocomp_linter.

#' PODCI Normative Values
#'
#' @param pop population
#' @param scale requested scale
#' @param stat normative mean or standard deviation
#'
#' @return the requested normative value
#' @export
#'
#' @examples
#' ## adolescent happiness normative mean
#' podci_norms("ado", "hapy", "m")
#'
#' ## pediatric sports normative standard deviation
#' podci_norms("ped", "sprt", "s")
#'
podci_norms <- function(
    pop = c("ped", "ado"),
    scale = c("upex", "tran", "sprt", "pain", "hapy", "glob"),
    stat = c("m", "s")) {
  pop <- match.arg(pop)
  scale <- match.arg(scale)
  stat <- match.arg(stat)

  dplyr::case_when(
    pop == "ped" && scale == "upex" && stat == "m" ~ 91.91380,
    pop == "ped" && scale == "upex" && stat == "s" ~ 11.49680,
    pop == "ado" && scale == "upex" && stat == "m" ~ 98.73970,
    pop == "ado" && scale == "upex" && stat == "s" ~ 5.03207,
    pop == "ped" && scale == "tran" && stat == "m" ~ 98.33460,
    pop == "ped" && scale == "tran" && stat == "s" ~ 5.69935,
    pop == "ado" && scale == "tran" && stat == "m" ~ 99.00000,
    pop == "ado" && scale == "tran" && stat == "s" ~ 5.00000,
    pop == "ped" && scale == "sprt" && stat == "m" ~ 92.44210,
    pop == "ped" && scale == "sprt" && stat == "s" ~ 10.21742,
    pop == "ado" && scale == "sprt" && stat == "m" ~ 94.00000,
    pop == "ado" && scale == "sprt" && stat == "s" ~ 11.00000,
    pop == "ped" && scale == "pain" && stat == "m" ~ 92.45370,
    pop == "ped" && scale == "pain" && stat == "s" ~ 13.76303,
    pop == "ado" && scale == "pain" && stat == "m" ~ 88.00000,
    pop == "ado" && scale == "pain" && stat == "s" ~ 17.00000,
    pop == "ped" && scale == "hapy" && stat == "m" ~ 89.79410,
    pop == "ped" && scale == "hapy" && stat == "s" ~ 14.10834,
    pop == "ado" && scale == "hapy" && stat == "m" ~ 82.00000,
    pop == "ado" && scale == "hapy" && stat == "s" ~ 18.00000,
    pop == "ped" && scale == "glob" && stat == "m" ~ 93.87860,
    pop == "ped" && scale == "glob" && stat == "s" ~ 7.39539,
    pop == "ado" && scale == "glob" && stat == "m" ~ 95.00000,
    pop == "ado" && scale == "glob" && stat == "s" ~ 7.00000,
    TRUE ~ NA_real_
  )
}

# nolint end
