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
#' @param patient subject of the assessment (pediatric vs adolescent)
#' @param reporter person providing the responses (parent vs self)
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @note
#' Pediatric
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
#' Adolescent (Parent-Report)
#' 1. Q18	Run short distances?
#' 1. Q19	Bicycle or tricycle?
#' 1. Q20	Climb three flights of stairs?
#' 1. Q22	Walk more than a mile?
#' 1. Q23	Walk three blocks?
#' 1. Q26	How often does your child need help from another person for walking
#' and climbing?
#' 1. Q27	How often does your child use assistive devices (such as braces,
#' crutches,  or wheelchair) for walking and climbing?
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
#' Adolescent (Self-Report)
#' 1. Q18	Run short distances?
#' 1. Q19	Bicycle or tricycle?
#' 1. Q20	Climb three flights of stairs?
#' 1. Q22	Walk more than a mile?
#' 1. Q23	Walk three blocks?
#' 1. Q26	How often do you need help from another person for walking and
#' climbing?
#' 1. Q27	How often do you use assistive devices (such as braces,  crutches,
#' or wheelchair) for walking and climbing?
#' 1. Q36	Can you participate in recreational outdoor activities with other
#' kids the same age?
#' 1. Q42 Activity not in season?
#' 1. Q43	Can you participate in pickup games or sports with other kids the
#' same age?
#' 1. Q49 Activity not in season?
#' 1. Q50 	Can you participate in competitive level sports with other kids the
#' same age?
#' 1. Q56 Activity not in season?
#' 1. Q57	How often in the last week did your child get together and do things
#' with friends?
#' 1. Q62 Friends not around?
#' 1. Q63	How often in the last week did you participate in gym/recess?
#' 1. Q69 School not in session?
#' 1. Q70 Does not attend school?
#'
#' A minimum of 6 items must have valid answers to score this scale.
#'
#' @return data augmented with the requested score column
#' @export
podci_sprt <- function(
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
      ~ paste0("Q", podci_items("sprt", patient, reporter))
    ) %>%
    dplyr::rowwise()

  if (reporter == "prnt") {
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(
            paste0("Q", podci_items("sprt", patient, reporter, "primary"))
          ),
          ~ dplyr::if_else(. == 5, NA_real_, .)
        )
      )
  }

  data <- data %>%
    dplyr::mutate(
      n_obs = sum(
        !is.na(
          dplyr::c_across(
            dplyr::all_of(
              paste0("Q", podci_items("sprt", patient, reporter, "primary"))
            )
          )
        )
      ),
      dplyr::across(c(.data[["Q26"]], .data[["Q27"]]), ~ ((. - 1) * 3 / 4) + 1)
    )

  if (reporter == "prnt") {
    data <- data %>%
      dplyr::mutate(
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
                .data[["Q73"]] %==% 1)
            ),
          NA_real_,
          ((.data[["Q66"]] - 1) * 3 / 2) + 1
        )
      )
  }

  if (reporter == "self") {
    data <- data %>%
      dplyr::mutate(
        Q36 = dplyr::if_else(
          .data[["Q36"]] == 4 & .data[["Q42"]] %==% 1,
          NA_real_,
          .data[["Q36"]]
        ),
        Q43 = dplyr::if_else(
          .data[["Q43"]] == 4 & .data[["Q49"]] %==% 1,
          NA_real_,
          .data[["Q43"]]
        ),
        Q50 = dplyr::if_else(
          .data[["Q50"]] == 4 & .data[["Q56"]] %==% 1,
          NA_real_,
          .data[["Q50"]]
        ),
        Q57 = dplyr::if_else(
          .data[["Q57"]] == 3 & .data[["Q62"]] %==% 1,
          NA_real_,
          ((.data[["Q57"]] - 1) * 3 / 2) + 1
        ),
        Q63 = dplyr::if_else(
          .data[["Q63"]] == 4 |
            (.data[["Q63"]] == 3 &
              (.data[["Q69"]] %==% 1 |
                .data[["Q70"]] %==% 1)
            ),
          NA_real_,
          ((.data[["Q63"]] - 1) * 3 / 2) + 1
        )
      )
  }

  data <- data %>%
    dplyr::mutate(
      raw = dplyr::if_else(
        .data[["n_obs"]] >= 6,
        sum(
          dplyr::c_across(
            dplyr::all_of(
              paste0("Q", podci_items("sprt", patient, reporter, "primary"))
            )
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
              dplyr::all_of(
                paste0("Q", podci_items("sprt", patient, reporter, "primary"))
              )
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

#' @describeIn podci_sprt Sport Raw Pediatric Parent Score
#' @export
#' @examples
#' podci_sprt_raw_ped_prnt(podci_ped_prnt, podci_items("sprt", "ped", "prnt"))
#'
podci_sprt_raw_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_raw_ped_prnt = podci_sprt(
        data, ...,
        score = "raw", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_sprt Sport Mean Pediatric Parent Score
#' @export
#' @examples
#' podci_sprt_mean_ped_prnt(podci_ped_prnt, podci_items("sprt", "ped", "prnt"))
#'
podci_sprt_mean_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_mean_ped_prnt = podci_sprt(
        data, ...,
        score = "mean", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_sprt Sport Standard Pediatric Parent Score
#' @export
#' @examples
#' podci_sprt_stnd_ped_prnt(podci_ped_prnt, podci_items("sprt", "ped", "prnt"))
#'
podci_sprt_stnd_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_stnd_ped_prnt = podci_sprt(
        data, ...,
        score = "stnd", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_sprt Sport Normal Pediatric Parent Score
#' @export
#' @examples
#' podci_sprt_norm_ped_prnt(podci_ped_prnt, podci_items("sprt", "ped", "prnt"))
#'
podci_sprt_norm_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_norm_ped_prnt = podci_sprt(
        data,
        ...,
        score = "norm",
        patient = "ped",
        reporter = "prnt",
        norm_m = podci_norms("sprt", "ped", "prnt", "m"),
        norm_s = podci_norms("sprt", "ped", "prnt", "s")
      )
    )
}

#' @describeIn podci_sprt Sport Raw Adolescent Parent Score
#' @export
#' @examples
#' podci_sprt_raw_ado_prnt(podci_ado_prnt, podci_items("sprt", "ado", "prnt"))
#'
podci_sprt_raw_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_raw_ado_prnt = podci_sprt(
        data, ...,
        score = "raw", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_sprt Sport Mean Adolescent Parent Score
#' @export
#' @examples
#' podci_sprt_mean_ado_prnt(podci_ado_prnt, podci_items("sprt", "ado", "prnt"))
#'
podci_sprt_mean_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_mean_ado_prnt = podci_sprt(
        data, ...,
        score = "mean", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_sprt Sport Standard Adolescent Parent Score
#' @export
#' @examples
#' podci_sprt_stnd_ado_prnt(podci_ado_prnt, podci_items("sprt", "ado", "prnt"))
#'
podci_sprt_stnd_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_stnd_ado_prnt = podci_sprt(
        data, ...,
        score = "stnd", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_sprt Sport Normal Adolescent Parent Score
#' @export
#' @examples
#' podci_sprt_norm_ado_prnt(podci_ado_prnt, podci_items("sprt", "ado", "prnt"))
#'
podci_sprt_norm_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_norm_ado_prnt = podci_sprt(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "prnt",
        norm_m = podci_norms("sprt", "ado", "prnt", "m"),
        norm_s = podci_norms("sprt", "ado", "prnt", "s")
      )
    )
}

#' @describeIn podci_sprt Sport Raw Adolescent Self Score
#' @export
#' @examples
#' podci_sprt_raw_ado_self(podci_ado_self, podci_items("sprt", "ado", "self"))
#'
podci_sprt_raw_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_raw_ado_self = podci_sprt(
        data, ...,
        score = "raw", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_sprt Sport Mean Adolescent Self Score
#' @export
#' @examples
#' podci_sprt_mean_ado_self(podci_ado_self, podci_items("sprt", "ado", "self"))
#'
podci_sprt_mean_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_mean_ado_self = podci_sprt(
        data, ...,
        score = "mean", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_sprt Sport Standard Adolescent Self Score
#' @export
#' @examples
#' podci_sprt_stnd_ado_self(podci_ado_self, podci_items("sprt", "ado", "self"))
#'
podci_sprt_stnd_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_stnd_ado_self = podci_sprt(
        data, ...,
        score = "stnd", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_sprt Sport Normal Adolescent Self Score
#' @export
#' @examples
#' podci_sprt_norm_ado_self(podci_ado_self, podci_items("sprt", "ado", "self"))
#'
podci_sprt_norm_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_sprt_norm_ado_self = podci_sprt(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "self",
        norm_m = podci_norms("sprt", "ado", "self", "m"),
        norm_s = podci_norms("sprt", "ado", "self", "s")
      )
    )
}
