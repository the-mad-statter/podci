utils::globalVariables(c("n_obs", "stnd"))

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
#' @param patient subject of the assessment (pediatric vs adolescent)
#' @param reporter person providing the responses (parent vs self)
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @note
#' Pediatric
#' 1. Q10	How he/she looks?
#' 1. Q11	His/her body?
#' 1. Q12	What clothes or shoes he/she can wear?
#' 1. Q13	His/her ability to do the same things his/her friends do?
#' 1. Q14	His/her health in general?
#'
#' Any item rated "5" (Too young for this activity) is considered missing and
#' is not added into the scale.
#'
#' A minimum of 3 items must have valid answers to score this scale (including
#' those marked "too young" as missing).
#'
#' Adolescent (Parent-Report)
#' 1. Q10	How he/she looks?
#' 1. Q11	His/her body?
#' 1. Q12	What clothes or shoes he/she can wear?
#' 1. Q13	His/her ability to do the same things his/her friends do?
#' 1. Q14	His/her health in general?
#'
#' Any item rated "5" (Too young for this activity) is considered missing and
#' is not added into the scale.
#'
#' A minimum of 3 items must have valid answers to score this scale (including
#' those marked "too young" as missing).
#'
#' Adolescent (Self-Report)
#' 1. Q10	How you look?
#' 1. Q11	Your body?
#' 1. Q12	What clothes or shoes you can wear?
#' 1. Q13	Your ability to do the same things your friends do?
#' 1. Q14	Your health in general?
#'
#' A minimum of 3 items must have valid answers to score this scale.
#'
#' @return data augmented with the requested score column
#' @export
podci_hapy <- function(
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
      ~ paste0("Q", podci_items("hapy", patient, reporter))
    ) %>%
    dplyr::rowwise()

  if (reporter == "prnt") {
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(), ~ dplyr::if_else(. == 6, NA_real_, .)
        )
      )
  }

  data <- data %>%
    dplyr::mutate(
      n_obs = sum(!is.na(dplyr::c_across(dplyr::everything()))),
      raw = dplyr::if_else(
        n_obs >= 3,
        sum(dplyr::c_across(-n_obs), na.rm = TRUE),
        NA_real_
      )
    )

  if (score %in% c("mean", "stnd", "norm")) {
    data <- data %>%
      dplyr::mutate(
        mean = dplyr::if_else(
          n_obs >= 3,
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
      dplyr::mutate(stnd = ((5 - mean) / 4) * 100)
  }

  if (score == "norm") {
    data <- data %>%
      dplyr::mutate(norm = 10 * ((stnd - norm_m) / norm_s) + 50)
  }

  data %>%
    dplyr::pull(!!score)
}

#' @describeIn podci_hapy Happy Mean Pediatric Parent Score
#' @export
#' @examples
#' podci_hapy_raw_ped_prnt(podci_ped_prnt, podci_items("hapy", "ped", "prnt"))
#'
podci_hapy_raw_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_raw_ped_prnt = podci_hapy(
        data, ...,
        score = "raw", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_hapy Happy Mean Pediatric Parent Score
#' @export
#' @examples
#' podci_hapy_mean_ped_prnt(podci_ped_prnt, podci_items("hapy", "ped", "prnt"))
#'
podci_hapy_mean_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_mean_ped_prnt = podci_hapy(
        data, ...,
        score = "mean", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_hapy Happy Standard Pediatric Parent Score
#' @export
#' @examples
#' podci_hapy_stnd_ped_prnt(podci_ped_prnt, podci_items("hapy", "ped", "prnt"))
#'
podci_hapy_stnd_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_stnd_ped_prnt = podci_hapy(
        data, ...,
        score = "stnd", patient = "ped", reporter = "prnt"
      )
    )
}

#' @describeIn podci_hapy Happy Normal Pediatric Parent Score
#' @export
#' @examples
#' podci_hapy_norm_ped_prnt(podci_ped_prnt, podci_items("hapy", "ped", "prnt"))
#'
podci_hapy_norm_ped_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_norm_ped_prnt = podci_hapy(
        data,
        ...,
        score = "norm",
        patient = "ped",
        reporter = "prnt",
        norm_m = podci_norms("hapy", "ped", "prnt", "m"),
        norm_s = podci_norms("hapy", "ped", "prnt", "s")
      )
    )
}

#' @describeIn podci_hapy Happy Mean Adolescent Parent Score
#' @export
#' @examples
#' podci_hapy_raw_ado_prnt(podci_ado_prnt, podci_items("hapy", "ado", "prnt"))
#'
podci_hapy_raw_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_raw_ado_prnt = podci_hapy(
        data, ...,
        score = "raw", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_hapy Happy Mean Adolescent Parent Score
#' @export
#' @examples
#' podci_hapy_mean_ado_prnt(podci_ado_prnt, podci_items("hapy", "ado", "prnt"))
#'
podci_hapy_mean_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_mean_ado_prnt = podci_hapy(
        data, ...,
        score = "mean", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_hapy Happy Standard Adolescent Parent Score
#' @export
#' @examples
#' podci_hapy_stnd_ado_prnt(podci_ado_prnt, podci_items("hapy", "ado", "prnt"))
#'
podci_hapy_stnd_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_stnd_ado_prnt = podci_hapy(
        data, ...,
        score = "stnd", patient = "ado", reporter = "prnt"
      )
    )
}

#' @describeIn podci_hapy Happy Normal Adolescent Parent Score
#' @export
#' @examples
#' podci_hapy_norm_ado_prnt(podci_ado_prnt, podci_items("hapy", "ado", "prnt"))
#'
podci_hapy_norm_ado_prnt <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_norm_ado_prnt = podci_hapy(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "prnt",
        norm_m = podci_norms("hapy", "ado", "prnt", "m"),
        norm_s = podci_norms("hapy", "ado", "prnt", "s")
      )
    )
}

#' @describeIn podci_hapy Happy Mean Adolescent Self Score
#' @export
#' @examples
#' podci_hapy_raw_ado_self(podci_ado_self, podci_items("hapy", "ado", "self"))
#'
podci_hapy_raw_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_raw_ado_self = podci_hapy(
        data, ...,
        score = "raw", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_hapy Happy Mean Adolescent Self Score
#' @export
#' @examples
#' podci_hapy_mean_ado_self(podci_ado_self, podci_items("hapy", "ado", "self"))
#'
podci_hapy_mean_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_mean_ado_self = podci_hapy(
        data, ...,
        score = "mean", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_hapy Happy Standard Adolescent Self Score
#' @export
#' @examples
#' podci_hapy_stnd_ado_self(podci_ado_self, podci_items("hapy", "ado", "self"))
#'
podci_hapy_stnd_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_stnd_ado_self = podci_hapy(
        data, ...,
        score = "stnd", patient = "ado", reporter = "self"
      )
    )
}

#' @describeIn podci_hapy Happy Normal Adolescent Self Score
#' @export
#' @examples
#' podci_hapy_norm_ado_self(podci_ado_self, podci_items("hapy", "ado", "self"))
#'
podci_hapy_norm_ado_self <- function(data, ...) {
  data %>%
    dplyr::mutate(
      podci_hapy_norm_ado_self = podci_hapy(
        data,
        ...,
        score = "norm",
        patient = "ado",
        reporter = "self",
        norm_m = podci_norms("hapy", "ado", "self", "m"),
        norm_s = podci_norms("hapy", "ado", "self", "s")
      )
    )
}
