#' PODCI Upper Extremity Scores
#'
#' @param data a [dplyr::tibble] containing the PODCI upper extremity item
#' responses to be scored
#' @param ... <[dplyr::dplyr_tidy_select]> columns of the upper extremity items
#' in order (See Note).
#' @param score requested scale class
#' * raw `[8, 32]`
#' * mean `[1, 4]`
#' * standard `[0, 100]`
#' * normal
#'     * pediatric `[-30, 57]`
#'     * adolescent `[-146, 53]`
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @note
#' The scale names listed in `...` are expected to be in this order:
#' 1. Q1	Lift heavy books?
#' 1. Q2	Pour a half gallon of milk?
#' 1. Q3	Open a jar that has been opened before?
#' 1. Q4	Use a fork and spoon?
#' 1. Q5	Comb his/her hair?
#' 1. Q6	Button buttons?
#' 1. Q8	Write with a pencil?
#' 1. Q32 Turn door knobs?
#'
#' Any item rated "5" (Too young for this activity) is considered missing and
#' is not added into the scale.
#'
#' A minimum of 4 items must have valid answers to score this scale (including
#' those marked "too young" as missing).
#'
#' @return data augmented with the requested score column
#' @export
podci_upex <- function(
    data,
    ...,
    score = c("raw", "mean", "stnd", "norm"),
    norm_m,
    norm_s) {
  score <- match.arg(score)

  data <- data %>%
    dplyr::select(...) %>%
    dplyr::rename_with(~ paste0("Q", podci_items("upex"))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ dplyr::if_else(. == 5, NA_real_, .)),
      n_obs = sum(!is.na(dplyr::c_across(dplyr::everything()))),
      raw = dplyr::if_else(
        .data[["n_obs"]] >= 4,
        sum(dplyr::c_across(-.data[["n_obs"]]), na.rm = TRUE),
        NA_real_
      )
    )

  if (score %in% c("mean", "stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(
        mean = dplyr::if_else(
          .data[["n_obs"]] >= 4,
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
      dplyr::mutate(stnd = ((4 - mean) / 3) * 100)
  }

  if (score == "norm") {
    data <- data %>%
      dplyr::mutate(norm = 10 * ((.data[["stnd"]] - norm_m) / norm_s) + 50)
  }

  data %>%
    dplyr::pull(!!score)
}

#' @describeIn podci_upex PODCI Upper Extremity Raw Pediatric Score
#' @export
#' @examples
#' podci_upex_raw_ped(podci, podci_items("upex"))
#'
podci_upex_raw_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_raw_ped = podci_upex(data, ..., score = "raw")
    )
}

#' @describeIn podci_upex PODCI Upper Extremity Mean Pediatric Score
#' @export
#' @examples
#' podci_upex_mean_ped(podci, podci_items("upex"))
#'
podci_upex_mean_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_mean_ped = podci_upex(data, ..., score = "mean")
    )
}

#' @describeIn podci_upex PODCI Upper Extremity Standard Pediatric Score
#' @export
#' @examples
#' podci_upex_stnd_ped(podci, podci_items("upex"))
#'
podci_upex_stnd_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_stnd_ped = podci_upex(data, ..., score = "stnd")
    )
}

#' @describeIn podci_upex PODCI Upper Extremity Normal Pediatric Score
#' @export
#' @examples
#' podci_upex_norm_ped(podci, podci_items("upex"))
#'
podci_upex_norm_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_norm_ped = podci_upex(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ped", "upex", "m"),
        norm_s = podci_norms("ped", "upex", "s")
      )
    )
}

#' @describeIn podci_upex PODCI Upper Extremity Raw Adolescent Score
#' @export
#' @examples
#' podci_upex_raw_ado(podci, podci_items("upex"))
#'
podci_upex_raw_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_raw_ado = podci_upex(data, ..., score = "raw")
    )
}

#' @describeIn podci_upex PODCI Upper Extremity Mean Adolescent Score
#' @export
#' @examples
#' podci_upex_mean_ado(podci, podci_items("upex"))
#'
podci_upex_mean_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_mean_ado = podci_upex(data, ..., score = "mean")
    )
}

#' @describeIn podci_upex PODCI Upper Extremity Standard Adolescent Score
#' @export
#' @examples
#' podci_upex_stnd_ado(podci, podci_items("upex"))
#'
podci_upex_stnd_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_stnd_ado = podci_upex(data, ..., score = "stnd")
    )
}

#' @describeIn podci_upex PODCI Upper Extremity Normal Adolescent Score
#' @export
#' @examples
#' podci_upex_norm_ado(podci, podci_items("upex"))
#'
podci_upex_norm_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_norm_ado = podci_upex(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ado", "upex", "m"),
        norm_s = podci_norms("ado", "upex", "s")
      )
    )
}
