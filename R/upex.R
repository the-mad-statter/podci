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
#' @param patient subject of the assessment (pediatric vs adolescent)
#' @param reporter person providing the responses (parent vs self)
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @note
#' Pediatric
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
#' Adolescent (Parent-Report)
#' 1. Q1	Lift heavy books?
#' 1. Q2	Pour a half gallon of milk?
#' 1. Q3	Open a jar that has been opened before?
#' 1. Q4	Use a fork and spoon?
#' 1. Q5	Comb his/her hair?
#' 1. Q6	Button buttons?
#' 1. Q8	Write with a pencil?
#' 1. Q32	Turn door knobs?
#'
#' Any item rated "5" (Too young for this activity) is considered missing and
#' is not added into the scale.
#'
#' A minimum of 4 items must have valid answers to score this scale (including
#' those marked "too young" as missing).
#'
#' Adolescent (Self-Report)
#' 1. Q1	Lift heavy books?
#' 1. Q2	Pour a half gallon of milk?
#' 1. Q3	Open a jar that has been opened before?
#' 1. Q4	Use a fork and spoon?
#' 1. Q5	Comb your hair?
#' 1. Q6	Button buttons?
#' 1. Q8	Write with a pencil?
#' 1. Q32	Turn door knobs?
#'
#' A minimum of 4 items must have valid answers to score this scale.
#'
#' @return data augmented with the requested score column
#' @export
podci_upex <- function(
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
      ~ paste0("Q", podci_items("upex", patient, reporter))
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
      raw = dplyr::if_else(
        n_obs >= 4,
        sum(dplyr::c_across(-n_obs), na.rm = TRUE),
        NA_real_
      )
    )

  if (score %in% c("mean", "stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(
        mean = dplyr::if_else(
          n_obs >= 4,
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
      dplyr::mutate(stnd = ((4 - mean) / 3) * 100)
  }

  if (score == "norm") {
    data <- data %>%
      dplyr::mutate(norm = 10 * ((stnd - norm_m) / norm_s) + 50)
  }

  data %>%
    dplyr::pull(!!score)
}

#' @describeIn podci_upex Upper Extremity Raw Pediatric Parent Score
#' @export
#' @examples
#' podci_upex_raw_ped_prnt(podci_ped_prnt, podci_items("upex", "ped", "prnt"))
#'
podci_upex_raw_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_raw_ped_prnt = podci_upex(
        data, ...,
        score = "raw", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Mean Pediatric Parent Score
#' @export
#' @examples
#' podci_upex_mean_ped_prnt(podci_ped_prnt, podci_items("upex", "ped", "prnt"))
#'
podci_upex_mean_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_mean_ped_prnt = podci_upex(
        data, ...,
        score = "mean", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Standard Pediatric Parent Score
#' @export
#' @examples
#' podci_upex_stnd_ped_prnt(podci_ped_prnt, podci_items("upex", "ped", "prnt"))
#'
podci_upex_stnd_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_stnd_ped_prnt = podci_upex(
        data, ...,
        score = "stnd", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Normal Pediatric Parent Score
#' @export
#' @examples
#' podci_upex_norm_ped_prnt(podci_ped_prnt, podci_items("upex", "ped", "prnt"))
#'
podci_upex_norm_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_norm_ped_prnt = podci_upex(
        data,
        ...,
        score = "norm",
        patient = "ped",
        reporter = "prnt",
        norm_m = podci_norms("upex", "ped", "prnt", "m"),
        norm_s = podci_norms("upex", "ped", "prnt", "s")
      )
    )
}

#' @describeIn podci_upex Upper Extremity Raw Adolescent Parent Score
#' @export
#' @examples
#' podci_upex_raw_ado_prnt(podci_ado_prnt, podci_items("upex", "ado", "prnt"))
#'
podci_upex_raw_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_raw_ado_prnt = podci_upex(
        data, ...,
        score = "raw", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Mean Adolescent Parent Score
#' @export
#' @examples
#' podci_upex_mean_ado_prnt(podci_ado_prnt, podci_items("upex", "ado", "prnt"))
#'
podci_upex_mean_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_mean_ado_prnt = podci_upex(
        data, ...,
        score = "mean", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Standard Adolescent Parent Score
#' @export
#' @examples
#' podci_upex_stnd_ado_prnt(podci_ado_prnt, podci_items("upex", "ado", "prnt"))
#'
podci_upex_stnd_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_stnd_ado_prnt = podci_upex(
        data, ...,
        score = "stnd", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Normal Adolescent Parent Score
#' @export
#' @examples
#' podci_upex_norm_ado_prnt(podci_ado_prnt, podci_items("upex", "ado", "prnt"))
#'
podci_upex_norm_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_norm_ado_prnt = podci_upex(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "prnt",
        norm_m = podci_norms("upex", "ado", "prnt", "m"),
        norm_s = podci_norms("upex", "ado", "prnt", "s")
      )
    )
}

#' @describeIn podci_upex Upper Extremity Raw Adolescent Self Score
#' @export
#' @examples
#' podci_upex_raw_ado_self(podci_ado_self, podci_items("upex", "ado", "self"))
#'
podci_upex_raw_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_raw_ado_self = podci_upex(
        data, ...,
        score = "raw", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Mean Adolescent Self Score
#' @export
#' @examples
#' podci_upex_mean_ado_self(podci_ado_self, podci_items("upex", "ado", "self"))
#'
podci_upex_mean_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_mean_ado_self = podci_upex(
        data, ...,
        score = "mean", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Standard Adolescent Self Score
#' @export
#' @examples
#' podci_upex_stnd_ado_self(podci_ado_self, podci_items("upex", "ado", "self"))
#'
podci_upex_stnd_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_stnd_ado_self = podci_upex(
        data, ...,
        score = "stnd", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_upex Upper Extremity Normal Adolescent Self Score
#' @export
#' @examples
#' podci_upex_norm_ado_self(podci_ado_self, podci_items("upex", "ado", "self"))
#'
podci_upex_norm_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_upex_norm_ado_self = podci_upex(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "self",
        norm_m = podci_norms("upex", "ado", "self", "m"),
        norm_s = podci_norms("upex", "ado", "self", "s")
      )
    )
}
