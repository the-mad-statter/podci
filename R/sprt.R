#' PODCI Sport Scores
#'
#' @param data a [dplyr::tibble] containing the PODCI sport item responses
#' to be scored
#' @param ... <[dplyr::dplyr_tidy_select]> columns of the sport items in
#' order (See Note).
#' @param score requested scale class
#' * raw `[12, 48]`
#' * mean `[1, 4]`
#' * standard `[0, 100]`
#' * normative
#'     * pediatric `[-40, 57]`
#'     * adolscent: `[-36, 56]`
#' @param norm_m mean value to use when computing normative scores
#' @param norm_s standard deviation value to use when computing normative scores
#'
#' @note
#' The scale names listed in `...` are expected to be in this order:
#' 1. Q18	Run short distances?
#' 1. Q19	Bicycle or tricycle?
#' 1. Q20	Climb three flights of stairs?
#' 1. Q22	Walk more than a mile?
#' 1. Q23	Walk three blocks?
#' 1. Q26	How often does your child need help from another person for walking
#' and climbing?
#' 1. Q27	How often does your child use assistive devices (such as braces,
#' crutches, or wheelchair) for walking and climbing?
#' 1. Q36	Can your child participate in recreational outdoor activities with
#' other children the same age?
#' 1. Q42 Too young?
#' 1. Q43 Activity not in season?
#' 1. Q44	Can your child participate in pickup games or sports with other
#' children the same age?
#' 1. Q50 Too young?
#' 1. Q51 Activity not in season?
#' 1. Q52	Can your child participate in competitive level sports with other
#' children the same age?
#' 1. Q58 Too young?
#' 1. Q59 Activity not in season?
#' 1. Q60	How often in the last week did your child get together and do things
#' with friends?
#' 1. Q65 Friends not around?
#' 1. Q66	How often in the last week did your child participate in gym/recess?
#' 1. Q72 School not in session?
#' 1. Q73 Does not attend school?
#'
#' Any item rated "5" (Too young for this activity) is considered missing and
#' is not added into the scale.
#'
#' A minimum of 6 items must have valid answers to score this scale (including
#' those marked "too young" as missing).
#'
#' @return data augmented with the requested score column
podci_sprt <- function(
    data,
    ...,
    score = c("raw", "mean", "stnd", "norm"),
    norm_m,
    norm_s) {
  score <- match.arg(score)

  data <- data %>%
    dplyr::select(...) %>%
    dplyr::rename_with(~ paste0("Q", podci_items("sprt"))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(paste0("Q", podci_items("sprt", "primary"))),
        ~ dplyr::if_else(. == 5, NA_real_, .)
      ),
      n_obs = sum(
        !is.na(
          dplyr::c_across(
            dplyr::all_of(paste0("Q", podci_items("sprt", "primary")))
          )
        )
      ),
      dplyr::across(c(.data[["Q26"]], .data[["Q27"]]), ~ ((. - 1) * 3 / 4) + 1),
      Q36 = dplyr::if_else(
        .data[["Q36"]] == 4 & (.data[["Q42"]] %==% 1 | .data[["Q43"]] %==% 1),
        NA_real_,
        .data[["Q36"]]
      ),
      Q44 = dplyr::if_else(
        .data[["Q44"]] == 4 & (.data[["Q50"]] %==% 1 | .data[["Q51"]] %==% 1),
        NA_real_,
        .data[["Q44"]]
      ),
      Q52 = dplyr::if_else(
        .data[["Q52"]] == 4 & (.data[["Q58"]] %==% 1 | .data[["Q59"]] %==% 1),
        NA_real_,
        .data[["Q52"]]
      ),
      Q60 = dplyr::if_else(
        .data[["Q60"]] == 3 & .data[["Q65"]] %==% 1,
        NA_real_,
        ((.data[["Q60"]] - 1) * 3 / 2) + 1
      ),
      Q66 = dplyr::if_else(
        (.data[["Q66"]] == 4) |
          (.data[["Q66"]] == 3 &
            (.data[["Q72"]] %==% 1 |
              .data[["Q73"]] %==% 1
            )
          ),
        NA_real_,
        ((.data[["Q66"]] - 1) * 3 / 2) + 1
      ),
      raw = dplyr::if_else(
        .data[["n_obs"]] >= 6,
        sum(
          dplyr::c_across(
            dplyr::all_of(paste0("Q", podci_items("sprt", "primary")))
          ),
          na.rm = TRUE
        ),
        NA_real_
      )
    )

  if (score %in% c("mean", "stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(
        mean = dplyr::if_else(
          .data[["n_obs"]] >= 6,
          mean(
            dplyr::c_across(
              dplyr::all_of(paste0("Q", podci_items("sprt", "primary")))
            ),
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

#' @describeIn podci_sprt PODCI Sport Raw Pediatric Score
#' @export
#' @examples
#' podci_sprt_raw_ped(podci, podci_items("sprt"))
podci_sprt_raw_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_raw_ped = podci_sprt(data, ..., score = "raw")
    )
}

#' @describeIn podci_sprt PODCI Sport Mean Pediatric Score
#' @export
#' @examples
#' podci_sprt_mean_ped(podci, podci_items("sprt"))
podci_sprt_mean_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_mean_ped = podci_sprt(data, ..., score = "mean")
    )
}

#' @describeIn podci_sprt PODCI Sport Standard Pediatric Score
#' @export
#' @examples
#' podci_sprt_stnd_ped(podci, podci_items("sprt"))
podci_sprt_stnd_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_stnd_ped = podci_sprt(data, ..., score = "stnd")
    )
}

#' @describeIn podci_sprt PODCI Sport Normal Pediatric Score
#' @export
#' @examples
#' podci_sprt_norm_ped(podci, podci_items("sprt"))
podci_sprt_norm_ped <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_norm_ped = podci_sprt(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ped", "sprt", "m"),
        norm_s = podci_norms("ped", "sprt", "s")
      )
    )
}

#' @describeIn podci_sprt PODCI Sport Raw Adolescent Score
#' @export
#' @examples
#' podci_sprt_raw_ado(podci, podci_items("sprt"))
podci_sprt_raw_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_raw_ado = podci_sprt(data, ..., score = "raw")
    )
}

#' @describeIn podci_sprt PODCI Sport Mean Adolescent Score
#' @export
#' @examples
#' podci_sprt_mean_ado(podci, podci_items("sprt"))
podci_sprt_mean_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_mean_ado = podci_sprt(data, ..., score = "mean")
    )
}

#' @describeIn podci_sprt PODCI Sport Standard Adolescent Score
#' @export
#' @examples
#' podci_sprt_stnd_ado(podci, podci_items("sprt"))
podci_sprt_stnd_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_stnd_ado = podci_sprt(data, ..., score = "stnd")
    )
}

#' @describeIn podci_sprt PODCI Sport Normal Adolescent Score
#' @export
#' @examples
#' podci_sprt_norm_ado(podci, podci_items("sprt"))
podci_sprt_norm_ado <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_norm_ado = podci_sprt(
        data,
        ...,
        score = "norm",
        norm_m = podci_norms("ado", "sprt", "m"),
        norm_s = podci_norms("ado", "sprt", "s")
      )
    )
}
