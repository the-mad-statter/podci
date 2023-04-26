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
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @note
#' The scale names listed in `...` are expected to be in this order:
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
#' @return data augmented with the requested score column
#' @export
podci_tran <- function(
    data,
    ...,
    score = c("raw", "mean", "stnd", "norm"),
    norm_m,
    norm_s) {
  score <- match.arg(score)

  data <- data %>%
    dplyr::select(...) %>%
    dplyr::rename_with(~ paste0("Q", podci_items("tran"))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ dplyr::if_else(. == 5, NA_real_, .)),
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

#' @describeIn podci_tran PODCI Transfer Raw Pediatric Score
#' @export
#' @examples
#' podci_tran_raw_ped(podci, podci_items("tran"))
#'
podci_tran_raw_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_raw_ped = podci_tran(data, ..., score = "raw")
    )
}

#' @describeIn podci_tran PODCI Transfer Mean Pediatric Score
#' @export
#' @examples
#' podci_tran_mean_ped(podci, podci_items("tran"))
#'
podci_tran_mean_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_mean_ped = podci_tran(data, ..., score = "mean")
    )
}

#' @describeIn podci_tran PODCI Transfer Standard Pediatric Score
#' @export
#' @examples
#' podci_tran_stnd_ped(podci, podci_items("tran"))
#'
podci_tran_stnd_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_stnd_ped = podci_tran(data, ..., score = "stnd")
    )
}

#' @describeIn podci_tran PODCI Transfer Normal Pediatric Score
#' @export
#' @examples
#' podci_tran_norm_ped(podci, podci_items("tran"))
#'
podci_tran_norm_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_norm_ped = podci_tran(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ped", "tran", "m"),
        norm_s = podci_norms("ped", "tran", "s")
      )
    )
}

#' @describeIn podci_tran PODCI Transfer Raw Adolescent Score
#' @export
#' @examples
#' podci_tran_raw_ado(podci, podci_items("tran"))
#'
podci_tran_raw_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_raw_ado = podci_tran(data, ..., score = "raw")
    )
}

#' @describeIn podci_tran PODCI Transfer Mean Adolescent Score
#' @export
#' @examples
#' podci_tran_mean_ado(podci, podci_items("tran"))
#'
podci_tran_mean_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_mean_ado = podci_tran(data, ..., score = "mean")
    )
}

#' @describeIn podci_tran PODCI Transfer Standard Adolescent Score
#' @export
#' @examples
#' podci_tran_stnd_ado(podci, podci_items("tran"))
#'
podci_tran_stnd_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_stnd_ado = podci_tran(data, ..., score = "stnd")
    )
}

#' @describeIn podci_tran PODCI Transfer Normal Adolescent Score
#' @export
#' @examples
#' podci_tran_norm_ado(podci, podci_items("tran"))
#'
podci_tran_norm_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_tran_norm_ado = podci_tran(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ado", "tran", "m"),
        norm_s = podci_norms("ado", "tran", "s")
      )
    )
}
