#' PODCI Happy Scores
#'
#' @param data a [dplyr::tibble] containing the PODCI happy item responses
#' to be scored
#' @param ... <[dplyr::dplyr_tidy_select]> columns of the happy items in
#' order (See Note).
#' @param score requested scale class:
#' * raw `[5, 25]`
#' * mean `[1, 5]`
#' * standard `[0, 100]`
#' * normative
#'     * pediatric `[-14, 57]`
#'     * adolescent `[4, 60]`
#' @param norm_m mean value to use when computing normative scores
#' @param norm_s standard deviation value to use when computing normative scores
#'
#' @note
#' The scale names listed in `...` are expected to be in this order:
#' 1. Q10	How he/she looks?
#' 1. Q11	His/her body?
#' 1. Q12	What clothes or shoes he/she can wear?
#' 1. Q13	His/her ability to do the same things his/her friends do?
#' 1. Q14	His/her health in general?
#'
#' @return data augmented with the requested score column
podci_hapy <- function(
    data,
    ...,
    score = c("raw", "mean", "stnd", "norm"),
    norm_m,
    norm_s) {
  score <- match.arg(score)

  data <- data %>%
    dplyr::select(...) %>%
    dplyr::rename_with(~ paste0("Q", podci_items("hapy"))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), ~ dplyr::if_else(. == 6, NA_real_, .)
      ),
      n_obs = sum(!is.na(dplyr::c_across(dplyr::everything()))),
      raw = dplyr::if_else(
        .data[["n_obs"]] >= 3,
        sum(dplyr::c_across(-.data[["n_obs"]]), na.rm = TRUE),
        NA_real_
      )
    )

  if (score %in% c("mean", "stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(
        mean = dplyr::if_else(
          .data[["n_obs"]] >= 3,
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
      dplyr::mutate(stnd = ((5 - .data[["mean"]]) / 4) * 100)
  }

  if (score == "norm") {
    data <- data %>%
      dplyr::mutate(norm = 10 * ((.data[["stnd"]] - norm_m) / norm_s) + 50)
  }

  data %>%
    dplyr::pull(!!score)
}

#' @describeIn podci_hapy PODCI Happy Mean Pediatric Score
#' @export
#' @examples
#' podci_hapy_raw_ped(podci, podci_items("hapy"))
podci_hapy_raw_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_raw_ped = podci_hapy(data, ..., score = "raw")
    )
}

#' @describeIn podci_hapy PODCI Happy Mean Pediatric Score
#' @export
#' @examples
#' podci_hapy_mean_ped(podci, podci_items("hapy"))
podci_hapy_mean_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_mean_ped = podci_hapy(data, ..., score = "mean")
    )
}

#' @describeIn podci_hapy PODCI Happy Standard Pediatric Score
#' @export
#' @examples
#' podci_hapy_stnd_ped(podci, podci_items("hapy"))
podci_hapy_stnd_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_stnd_ped = podci_hapy(data, ..., score = "stnd")
    )
}

#' @describeIn podci_hapy PODCI Happy Normal Pediatric Score
#' @export
#' @examples
#' podci_hapy_norm_ped(podci, podci_items("hapy"))
podci_hapy_norm_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_norm_ped = podci_hapy(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ped", "hapy", "m"),
        norm_s = podci_norms("ped", "hapy", "s")
      )
    )
}

#' @describeIn podci_hapy PODCI Happy Mean Adolescent Score
#' @export
#' @examples
#' podci_hapy_raw_ado(podci, podci_items("hapy"))
podci_hapy_raw_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_raw_ado = podci_hapy(data, ..., score = "raw")
    )
}

#' @describeIn podci_hapy PODCI Happy Mean Adolescent Score
#' @export
#' @examples
#' podci_hapy_mean_ado(podci, podci_items("hapy"))
podci_hapy_mean_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_mean_ado = podci_hapy(data, ..., score = "mean")
    )
}

#' @describeIn podci_hapy PODCI Happy Standard Adolescent Score
#' @export
#' @examples
#' podci_hapy_stnd_ado(podci, podci_items("hapy"))
podci_hapy_stnd_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_stnd_ado = podci_hapy(data, ..., score = "stnd")
    )
}

#' @describeIn podci_hapy PODCI Happy Normal Adolescent Score
#' @export
#' @examples
#' podci_hapy_norm_ado(podci, podci_items("hapy"))
podci_hapy_norm_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_norm_ado = podci_hapy(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ado", "hapy", "m"),
        norm_s = podci_norms("ado", "hapy", "s")
      )
    )
}
