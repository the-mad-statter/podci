#' PODCI Global Scores
#'
#' @param data a [dplyr::tibble] containing the PODCI happy item responses
#' to be scored
#' @param upex_stnd upper extremity standard score
#' @param tran_stnd transfer standard score
#' @param sprt_stnd sport standard score
#' @param pain_stnd pain standard score
#' @param score requested scale class:
#' * standard `[0, 100]`
#' * normative
#'     * pediatric `[-77, 58]`
#'     * adolescent `[-78, 57]`
#' @param norm_m mean value to use when computing normative scores
#' @param norm_s standard deviation value to use when computing normative scores
#'
#' @return data augmented with the requested score column
podci_glob <- function(
    data,
    upex_stnd, tran_stnd, sprt_stnd, pain_stnd,
    score = c("stnd", "norm"),
    norm_m,
    norm_s) {
  score <- match.arg(score)

  data <- data %>%
    dplyr::select(
      {{ upex_stnd }},
      {{ tran_stnd }},
      {{ sprt_stnd }},
      {{ pain_stnd }}
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(stnd = mean(dplyr::c_across(dplyr::everything())))

  if (score == "norm") {
    data <- data %>%
      dplyr::mutate(norm = 10 * ((.data[["stnd"]] - norm_m) / norm_s) + 50)
  }

  data %>%
    dplyr::pull(!!score)
}

#' @describeIn podci_glob PODCI Global Standard Pediatric Score
#' @export
#' @examples
#' podci %>%
#'   podci_upex_stnd_ped(podci_items("upex")) %>%
#'   podci_tran_stnd_ped(podci_items("tran")) %>%
#'   podci_sprt_stnd_ped(podci_items("sprt")) %>%
#'   podci_pain_stnd_ped(podci_items("pain")) %>%
#'   podci_glob_stnd_ped(
#'     podci_upex_stnd_ped,
#'     podci_tran_stnd_ped,
#'     podci_sprt_stnd_ped,
#'     podci_pain_stnd_ped
#'   )
podci_glob_stnd_ped <- function(
    data,
    upex_stnd,
    tran_stnd,
    sprt_stnd,
    pain_stnd) {
  data %>%
    dplyr::mutate(
      podci_glob_stnd_ped = podci_glob(
        data,
        {{ upex_stnd }},
        {{ tran_stnd }},
        {{ sprt_stnd }},
        {{ pain_stnd }},
        "stnd"
      )
    )
}

#' @describeIn podci_glob PODCI Global Normal Pediatric Score
#' @export
#' @examples
#' podci %>%
#'   podci_upex_stnd_ped(podci_items("upex")) %>%
#'   podci_tran_stnd_ped(podci_items("tran")) %>%
#'   podci_sprt_stnd_ped(podci_items("sprt")) %>%
#'   podci_pain_stnd_ped(podci_items("pain")) %>%
#'   podci_glob_norm_ped(
#'     podci_upex_stnd_ped,
#'     podci_tran_stnd_ped,
#'     podci_sprt_stnd_ped,
#'     podci_pain_stnd_ped
#'   )
podci_glob_norm_ped <- function(
    data,
    upex_stnd,
    tran_stnd,
    sprt_stnd,
    pain_stnd) {
  data %>%
    dplyr::mutate(
      podci_glob_norm_ped = podci_glob(
        data,
        {{ upex_stnd }},
        {{ tran_stnd }},
        {{ sprt_stnd }},
        {{ pain_stnd }},
        "norm",
        podci_norms("ped", "glob", "m"),
        podci_norms("ped", "glob", "s")
      )
    )
}

#' @describeIn podci_glob PODCI Global Standard Adolescent Score
#' @export
#' @examples
#' podci %>%
#'   podci_upex_stnd_ado(podci_items("upex")) %>%
#'   podci_tran_stnd_ado(podci_items("tran")) %>%
#'   podci_sprt_stnd_ado(podci_items("sprt")) %>%
#'   podci_pain_stnd_ado(podci_items("pain")) %>%
#'   podci_glob_stnd_ado(
#'     podci_upex_stnd_ado,
#'     podci_tran_stnd_ado,
#'     podci_sprt_stnd_ado,
#'     podci_pain_stnd_ado
#'   )
podci_glob_stnd_ado <- function(
    data,
    upex_stnd,
    tran_stnd,
    sprt_stnd,
    pain_stnd) {
  data %>%
    dplyr::mutate(
      podci_glob_stnd_ado = podci_glob(
        data,
        {{ upex_stnd }},
        {{ tran_stnd }},
        {{ sprt_stnd }},
        {{ pain_stnd }},
        "stnd"
      )
    )
}

#' @describeIn podci_glob PODCI Global Normal Adolescent Score
#' @export
#' @examples
#' podci %>%
#'   podci_upex_stnd_ado(podci_items("upex")) %>%
#'   podci_tran_stnd_ado(podci_items("tran")) %>%
#'   podci_sprt_stnd_ado(podci_items("sprt")) %>%
#'   podci_pain_stnd_ado(podci_items("pain")) %>%
#'   podci_glob_norm_ado(
#'     podci_upex_stnd_ado,
#'     podci_tran_stnd_ado,
#'     podci_sprt_stnd_ado,
#'     podci_pain_stnd_ado
#'   )
podci_glob_norm_ado <- function(
    data,
    upex_stnd,
    tran_stnd,
    sprt_stnd,
    pain_stnd) {
  data %>%
    dplyr::mutate(
      podci_glob_norm_ado = podci_glob(
        data,
        {{ upex_stnd }},
        {{ tran_stnd }},
        {{ sprt_stnd }},
        {{ pain_stnd }},
        "norm",
        podci_norms("ado", "glob", "m"),
        podci_norms("ado", "glob", "s")
      )
    )
}
