#' PODCI Transfer Scores
#'
#' @param data a [dplyr::tibble] containing the PODCI transfer item responses
#' to be scored
#' @param ... <[dplyr::dplyr_tidy_select]> columns of the transfer items in
#' order (See Note).
#' @param score requested scale class
#' * raw `[11, 44]`
#' * mean `[1, 4]`
#' * standard `[0, 100]`
#' * normative
#'     * pediatric `[-123, 53]`
#'     * adolescent: `[-164, 52]`
#' @param patient subject of the assessment (pediatric vs adolescent)
#' @param reporter person providing the responses (parent vs self)
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @note
#' Pediatric
#' 1. Q7	Put on his/her coat?
#' 1. Q21	Climb one flight of stairs?
#' 1. Q24	Walk one block?
#' 1. Q25	Get on and off a bus?
#' 1. Q28	Stand while washing his/her hands and face at a sink?
#' 1. Q29	Sit in a regular chair without holding on?
#' 1. Q30	Get on and off a toilet or chair?
#' 1. Q31	Get in and out of bed?
#' 1. Q33	Bend over from a standing position and pick up something off the
#' floor?
#' 1. Q34	How often does your child need help from another person for sitting
#' and standing?
#' 1. Q35	How often does your child use assistive devices (such as braces,
#' crutches, or wheelchair) for sitting and standing?
#'
#' Any item rated "5" (Too young for this activity) is considered missing and
#' is not added into the scale.
#'
#' A minimum of 7 items must have valid answers to score this scale (including
#' those marked "too young" as missing).
#'
#' Adolescent (Parent-Report)
#' 1. Q7	Put on his/her coat?
#' 1. Q21	Climb one flight of stairs?
#' 1. Q24	Walk one block?
#' 1. Q25	Get on and off a bus?
#' 1. Q28	Stand while washing his/her hands and face at a sink?
#' 1. Q29	Sit in a regular chair without holding on?
#' 1. Q30	Get on and off a toilet or chair?
#' 1. Q31	Get in and out of bed?
#' 1. Q33	Bend over from a standing position and pick up something off the
#' floor?
#' 1. Q34	How often does your child need help from another person for sitting
#' and standing?
#' 1. Q35	How often does your child use assistive devices (such as braces,
#' crutches,  or wheelchair) for sitting and standing?
#'
#' Any item rated "5" (Too young for this activity) is considered missing and
#' is not added into the scale.
#'
#' A minimum of 7 items must have valid answers to score this scale (including
#' those marked "too young" as missing).
#'
#' Adolescent (Self-Report)
#' 1. Q7	Put on your coat?
#' 1. Q21	Climb one flight of stairs?
#' 1. Q24	Walk one block?
#' 1. Q25	Get on and off a bus?
#' 1. Q28	Stand while washing your hands and face at a sink?
#' 1. Q29	Sit in a regular chair without holding on?
#' 1. Q30	Get on and off a toilet or chair?
#' 1. Q31	Get in and out of bed?
#' 1. Q33	Bend over from a standing position and pick up something off the
#' floor?
#' 1. Q34	How often do you need help from another person for sitting and
#' standing?
#' 1. Q35	How often do you use assistive devices (such as braces,  crutches,
#' or wheelchair) for sitting and standing?
#'
#' A minimum of 7 items must have valid answers to score this scale.
#'
#' @return data augmented with the requested score column
#' @export
podci_tran <- function(
    data,
    ...,
    score = c("raw", "mean", "stnd", "norm"),
    patient = c("ped", "ado"),
    reporter = c("prnt", "self"),
    norm_m,
    norm_s) {
  score <- match.arg(score)
  patient <- match.arg(patient)
  reporter <- match.arg(reporter)

  data <- data %>%
    dplyr::select(...) %>%
    dplyr::rename_with(
      ~ paste0("Q", podci_items("tran", patient, reporter))
    ) %>%
    dplyr::rowwise()

  if (reporter == "prnt") {
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(), ~ dplyr::if_else(. == 5, NA_real_, .)
        ),
      )
  }

  data <- data %>%
    dplyr::mutate(
      n_obs = sum(!is.na(dplyr::c_across(dplyr::everything()))),
      dplyr::across(c(.data[["Q34"]], .data[["Q35"]]), ~ ((. - 1) * 3 / 4) + 1),
      raw = dplyr::if_else(
        .data[["n_obs"]] >= 7,
        sum(dplyr::across(-.data[["n_obs"]]), na.rm = TRUE),
        NA_real_
      )
    )

  if (score %in% c("mean", "stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(
        mean = dplyr::if_else(
          .data[["n_obs"]] >= 7,
          mean(
            dplyr::c_across(-c(.data[["n_obs"]], .data[["raw"]])),
            na.rm = TRUE
          ),
          NA_real_
        )
      )
  }

  if (score %in% c("stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(stnd = ((4 - .data[["mean"]]) / 3) * 100)
  }

  if (score == "norm") {
    data <- data %>%
      dplyr::mutate(norm = 10 * ((.data[["stnd"]] - norm_m) / norm_s) + 50)
  }

  data %>%
    dplyr::pull(!!score)
}

#' @describeIn podci_tran Transfer Raw Pediatric Parent Score
#' @export
#' @examples
#' podci_tran_raw_ped_prnt(podci_ped_prnt, podci_items("tran", "ped", "prnt"))
#'
podci_tran_raw_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_raw_ped_prnt = podci_tran(
        data, ...,
        score = "raw", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_tran Transfer Mean Pediatric Parent Score
#' @export
#' @examples
#' podci_tran_mean_ped_prnt(podci_ped_prnt, podci_items("tran", "ped", "prnt"))
#'
podci_tran_mean_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_mean_ped_prnt = podci_tran(
        data, ...,
        score = "mean", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_tran Transfer Standard Pediatric Parent Score
#' @export
#' @examples
#' podci_tran_stnd_ped_prnt(podci_ped_prnt, podci_items("tran", "ped", "prnt"))
#'
podci_tran_stnd_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_stnd_ped_prnt = podci_tran(
        data, ...,
        score = "stnd", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_tran Transfer Normal Pediatric Parent Score
#' @export
#' @examples
#' podci_tran_norm_ped_prnt(podci_ped_prnt, podci_items("tran", "ped", "prnt"))
#'
podci_tran_norm_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_norm_ped_prnt = podci_tran(
        data,
        ...,
        score = "norm",
        patient = "ped",
        reporter = "prnt",
        norm_m = podci_norms("tran", "ped", "prnt", "m"),
        norm_s = podci_norms("tran", "ped", "prnt", "s")
      )
    )
}

#' @describeIn podci_tran Transfer Raw Adolescent Parent Score
#' @export
#' @examples
#' podci_tran_raw_ado_prnt(podci_ado_prnt, podci_items("tran", "ado", "prnt"))
#'
podci_tran_raw_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_raw_ado_prnt = podci_tran(
        data, ...,
        score = "raw", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_tran Transfer Mean Adolescent Parent Score
#' @export
#' @examples
#' podci_tran_mean_ado_prnt(podci_ado_prnt, podci_items("tran", "ado", "prnt"))
#'
podci_tran_mean_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_mean_ado_prnt = podci_tran(
        data, ...,
        score = "mean", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_tran Transfer Standard Adolescent Parent Score
#' @export
#' @examples
#' podci_tran_stnd_ado_prnt(podci_ado_prnt, podci_items("tran", "ado", "prnt"))
#'
podci_tran_stnd_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_stnd_ado_prnt = podci_tran(
        data, ...,
        score = "stnd", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_tran Transfer Normal Adolescent Parent Score
#' @export
#' @examples
#' podci_tran_norm_ado_prnt(podci_ado_prnt, podci_items("tran", "ado", "prnt"))
#'
podci_tran_norm_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_norm_ado_prnt = podci_tran(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "prnt",
        norm_m = podci_norms("tran", "ado", "prnt", "m"),
        norm_s = podci_norms("tran", "ado", "prnt", "s")
      )
    )
}

#' @describeIn podci_tran Transfer Raw Adolescent Self Score
#' @export
#' @examples
#' podci_tran_raw_ado_self(podci_ado_self, podci_items("tran", "ado", "self"))
#'
podci_tran_raw_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_raw_ado_self = podci_tran(
        data, ...,
        score = "raw", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_tran Transfer Mean Adolescent Self Score
#' @export
#' @examples
#' podci_tran_mean_ado_self(podci_ado_self, podci_items("tran", "ado", "self"))
#'
podci_tran_mean_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_mean_ado_self = podci_tran(
        data, ...,
        score = "mean", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_tran Transfer Standard Adolescent Self Score
#' @export
#' @examples
#' podci_tran_stnd_ado_self(podci_ado_self, podci_items("tran", "ado", "self"))
#'
podci_tran_stnd_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_stnd_ado_self = podci_tran(
        data, ...,
        score = "stnd", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_tran Transfer Normal Adolescent Self Score
#' @export
#' @examples
#' podci_tran_norm_ado_self(podci_ado_self, podci_items("tran", "ado", "self"))
#'
podci_tran_norm_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_norm_ado_self = podci_tran(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "self",
        norm_m = podci_norms("tran", "ado", "self", "m"),
        norm_s = podci_norms("tran", "ado", "self", "s")
      )
    )
}
