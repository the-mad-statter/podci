#' PODCI Pain Scores
#'
#' @param data a [dplyr::tibble] containing the PODCI pain item responses
#' to be scored
#' @param ... <[dplyr::dplyr_tidy_select]> columns of the pain items in
#' order (See Note).
#' @param score requested scale class
#' * raw `[3, 15]`
#' * mean `[1, 5]`
#' * standard `[0, 100]`
#' * normative
#'     * pediatric `[-17, 56]`
#'     * adolescent: `[-1, 57]`
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @note
#' The scale names listed in `...` are expected to be in this order:
#' 1. Q17	Did pain or discomfort interfere with your child’s activities?
#' 1. Q75	How much pain has your child had during the last week?
#' 1. Q76	During the last week,  how much did pain interfere with your child’s
#' normal activities (including at home,  outside of the home,  and at school)?
#'
#' A minimum of 2 items must have valid answers to score this scale.
#'
#' @return data augmented with the requested score column
#' @export
podci_pain <- function(
    data,
    ...,
    score = c("raw", "mean", "stnd", "norm"),
    norm_m,
    norm_s) {
  score <- match.arg(score)

  data <- data %>%
    dplyr::select(...) %>%
    dplyr::rename_with(~ paste0("Q", podci_items("pain"))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n_obs = sum(!is.na(dplyr::c_across(dplyr::everything()))),
      Q17 = ((4 - .data[["Q17"]]) * 4 / 3) + 1,
      Q75 = ((.data[["Q75"]] - 1) * 4 / 5) + 1,
      raw = dplyr::if_else(
        .data[["n_obs"]] >= 2,
        sum(dplyr::c_across(-.data[["n_obs"]]), na.rm = TRUE),
        NA_real_
      )
    )

  if (score %in% c("mean", "stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(
        mean = dplyr::if_else(
          .data[["n_obs"]] >= 2,
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

#' @describeIn podci_pain PODCI Pain Raw Pediatric Score
#' @export
#' @examples
#' podci_pain_raw_ped(podci, podci_items("pain"))
#'
podci_pain_raw_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_raw_ped = podci_pain(data, ..., score = "raw")
    )
}

#' @describeIn podci_pain PODCI Pain Mean Pediatric Score
#' @export
#' @examples
#' podci_pain_mean_ped(podci, podci_items("pain"))
#'
podci_pain_mean_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_mean_ped = podci_pain(data, ..., score = "mean")
    )
}

#' @describeIn podci_pain PODCI Pain Standard Pediatric Score
#' @export
#' @examples
#' podci_pain_stnd_ped(podci, podci_items("pain"))
#'
podci_pain_stnd_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_stnd_ped = podci_pain(data, ..., score = "stnd")
    )
}

#' @describeIn podci_pain PODCI Pain Normal Pediatric Score
#' @export
#' @examples
#' podci_pain_norm_ped(podci, podci_items("pain"))
#'
podci_pain_norm_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_norm_ped = podci_pain(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ped", "pain", "m"),
        norm_s = podci_norms("ped", "pain", "s")
      )
    )
}

#' @describeIn podci_pain PODCI Pain Raw Adolescent Score
#' @export
#' @examples
#' podci_pain_raw_ado(podci, podci_items("pain"))
#'
podci_pain_raw_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_raw_ado = podci_pain(data, ..., score = "raw")
    )
}

#' @describeIn podci_pain PODCI Pain Mean Adolescent Score
#' @export
#' @examples
#' podci_pain_mean_ped(podci, podci_items("pain"))
#'
podci_pain_mean_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_mean_ado = podci_pain(data, ..., score = "mean")
    )
}

#' @describeIn podci_pain PODCI Pain Standard Adolescent Score
#' @export
#' @examples
#' podci_pain_stnd_ped(podci, podci_items("pain"))
#'
podci_pain_stnd_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_stnd_ado = podci_pain(data, ..., score = "stnd")
    )
}

#' @describeIn podci_pain PODCI Pain Normal Adolescent Score
#' @export
#' @examples
#' podci_pain_norm_ped(podci, podci_items("pain"))
#'
podci_pain_norm_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_norm_ado = podci_pain(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ado", "pain", "m"),
        norm_s = podci_norms("ado", "pain", "s")
      )
    )
}
