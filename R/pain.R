utils::globalVariables(c("Q17", "Q72", "Q75", "n_obs", "stnd"))

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
#' @param patient subject of the assessment (pediatric vs adolescent)
#' @param reporter person providing the responses (parent vs self)
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @note
#' Pediatric
#' 1. Q17	Did pain or discomfort interfere with your child’s activities?
#' 1. Q75	How much pain has your child had during the last week?
#' 1. Q76	During the last week,  how much did pain interfere with your child’s
#' normal activities (including at home,  outside of the home,  and at school)?
#'
#' A minimum of 2 items must have valid answers to score this scale.
#'
#' Adolescent (Parent-Report)
#' 1. Q17	Did pain or discomfort interfere with your child’s activities?
#' 1. Q75	How much pain has your child had during the last week?
#' 1. Q76	During the last week,  how much did pain interfere with your child’s
#' normal activities (including at home,  outside of the home,  and at school)?
#'
#' A minimum of 2 items must have valid answers to score this scale.
#'
#' Adolescent (Self-Report)
#' 1. Q17 Did pain or discomfort interfere with your activities?
#' 1. Q72 How much pain have you had during the last week?
#' 1. Q73 During the last week,  how much did pain interfere with your normal
#' activities (including at home,  outside of the home,  and at school)?
#'
#' A minimum of 2 items must have valid answers to score this scale.
#'
#' @return data augmented with the requested score column
#' @export
podci_pain <- function(
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
      ~ paste0("Q", podci_items("pain", patient, reporter))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n_obs = sum(!is.na(dplyr::c_across(dplyr::everything()))),
      Q17 = ((4 - Q17) * 4 / 3) + 1
    )

  if (reporter == "prnt") {
    data <- data %>% dplyr::mutate(Q75 = ((Q75 - 1) * 4 / 5) + 1)
  } else {
    data <- data %>% dplyr::mutate(Q72 = ((Q72 - 1) * 4 / 5) + 1)
  }

  data <- data %>%
    dplyr::mutate(
      raw = dplyr::if_else(
        n_obs >= 2,
        sum(dplyr::c_across(-n_obs), na.rm = TRUE),
        NA_real_
      )
    )

  if (score %in% c("mean", "stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(
        mean = dplyr::if_else(
          n_obs >= 2,
          mean(
            dplyr::c_across(-c(n_obs, raw)),
            na.rm = TRUE
          ),
          NA_real_
        )
      )
  }

  if (score %in% c("stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(stnd = ((4 - (mean - 1)) / 4) * 100)
  }

  if (score == "norm") {
    data <- data %>%
      dplyr::mutate(norm = 10 * ((stnd - norm_m) / norm_s) + 50)
  }

  data %>%
    dplyr::pull(!!score)
}

#' @describeIn podci_pain Pain Raw Pediatric Parent Score
#' @export
#' @examples
#' podci_pain_raw_ped_prnt(podci_ped_prnt, podci_items("pain", "ped", "prnt"))
#'
podci_pain_raw_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_raw_ped_prnt = podci_pain(
        data, ...,
        score = "raw", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_pain Pain Mean Pediatric Parent Score
#' @export
#' @examples
#' podci_pain_mean_ped_prnt(podci_ped_prnt, podci_items("pain", "ped", "prnt"))
#'
podci_pain_mean_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_mean_ped_prnt = podci_pain(
        data, ...,
        score = "mean", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_pain Pain Standard Pediatric Parent Score
#' @export
#' @examples
#' podci_pain_stnd_ped_prnt(podci_ped_prnt, podci_items("pain", "ped", "prnt"))
#'
podci_pain_stnd_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_stnd_ped_prnt = podci_pain(
        data, ...,
        score = "stnd", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_pain Pain Normal Pediatric Parent Score
#' @export
#' @examples
#' podci_pain_norm_ped_prnt(podci_ped_prnt, podci_items("pain", "ped", "prnt"))
#'
podci_pain_norm_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_norm_ped_prnt = podci_pain(
        data,
        ...,
        score = "norm",
        patient = "ped",
        reporter = "prnt",
        norm_m = podci_norms("pain", "ped", "prnt", "m"),
        norm_s = podci_norms("pain", "ped", "prnt", "s")
      )
    )
}

#' @describeIn podci_pain Pain Raw Adolescent Parent Score
#' @export
#' @examples
#' podci_pain_raw_ado_prnt(podci_ado_prnt, podci_items("pain", "ado", "prnt"))
#'
podci_pain_raw_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_raw_ado_prnt = podci_pain(
        data, ...,
        score = "raw", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_pain Pain Mean Adolescent Parent Score
#' @export
#' @examples
#' podci_pain_mean_ado_prnt(podci_ado_prnt, podci_items("pain", "ado", "prnt"))
#'
podci_pain_mean_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_mean_ado_prnt = podci_pain(
        data, ...,
        score = "mean", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_pain Pain Standard Adolescent Parent Score
#' @export
#' @examples
#' podci_pain_stnd_ado_prnt(podci_ado_prnt, podci_items("pain", "ado", "prnt"))
#'
podci_pain_stnd_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_stnd_ado_prnt = podci_pain(
        data, ...,
        score = "stnd", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_pain Pain Normal Adolescent Parent Score
#' @export
#' @examples
#' podci_pain_norm_ado_prnt(podci_ado_prnt, podci_items("pain", "ado", "prnt"))
#'
podci_pain_norm_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_norm_ado_prnt = podci_pain(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "prnt",
        norm_m = podci_norms("pain", "ado", "prnt", "m"),
        norm_s = podci_norms("pain", "ado", "prnt", "s")
      )
    )
}

#' @describeIn podci_pain Pain Raw Adolescent Self Score
#' @export
#' @examples
#' podci_pain_raw_ado_self(podci_ado_self, podci_items("pain", "ado", "self"))
#'
podci_pain_raw_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_raw_ado_self = podci_pain(
        data, ...,
        score = "raw", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_pain Pain Mean Adolescent Self Score
#' @export
#' @examples
#' podci_pain_mean_ado_self(podci_ado_self, podci_items("pain", "ado", "self"))
#'
podci_pain_mean_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_mean_ado_self = podci_pain(
        data, ...,
        score = "mean", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_pain Pain Standard Adolescent Self Score
#' @export
#' @examples
#' podci_pain_stnd_ado_self(podci_ado_self, podci_items("pain", "ado", "self"))
#'
podci_pain_stnd_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_stnd_ado_self = podci_pain(
        data, ...,
        score = "stnd", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_pain Pain Normal Adolescent Self Score
#' @export
#' @examples
#' podci_pain_norm_ado_self(podci_ado_self, podci_items("pain", "ado", "self"))
#'
podci_pain_norm_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_pain_norm_ado_self = podci_pain(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "self",
        norm_m = podci_norms("pain", "ado", "self", "m"),
        norm_s = podci_norms("pain", "ado", "self", "s")
      )
    )
}
