#' PODCI Global Scores
#'
#' @param data a [dplyr::tibble] containing the PODCI upper extremity,
#' transfer, sport, and pain standard scores
#' to be scored
#' @param upex_stnd,upex_stnd_ped_prnt,upex_stnd_ado_prnt,upex_stnd_ado_self
#' upper extremity standard score
#' @param tran_stnd,tran_stnd_ped_prnt,tran_stnd_ado_prnt,tran_stnd_ado_self
#' transfer standard score
#' @param sprt_stnd,sprt_stnd_ped_prnt,sprt_stnd_ado_prnt,sprt_stnd_ado_self
#' sport standard score
#' @param pain_stnd,pain_stnd_ped_prnt,pain_stnd_ado_prnt,pain_stnd_ado_self
#'  pain standard score
#' @param score requested scale class:
#' * standard `[0, 100]`
#' * normative
#'     * pediatric `[-77, 58]`
#'     * adolescent `[-78, 57]`
#' @param norm_m mean value to use when computing normative scores
#' (See [podci_norms])
#' @param norm_s standard deviation value to use when computing normative scores
#' (See [podci_norms])
#'
#' @return data augmented with the requested score column
#' @export
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

#' @describeIn podci_glob Global Standard Pediatric Parent Score
#' @export
#' @examples
#' ## podci_glob_stnd_ped_prnt()
#' podci_ped_prnt %>%
#'   podci_upex_stnd_ped_prnt(podci_items("upex", "ped", "prnt")) %>%
#'   podci_tran_stnd_ped_prnt(podci_items("tran", "ped", "prnt")) %>%
#'   podci_sprt_stnd_ped_prnt(podci_items("sprt", "ped", "prnt")) %>%
#'   podci_pain_stnd_ped_prnt(podci_items("pain", "ped", "prnt")) %>%
#'   podci_glob_stnd_ped_prnt(
#'     podci_upex_stnd_ped_prnt,
#'     podci_tran_stnd_ped_prnt,
#'     podci_sprt_stnd_ped_prnt,
#'     podci_pain_stnd_ped_prnt
#'   )
#'
podci_glob_stnd_ped_prnt <- function(
    data,
    upex_stnd_ped_prnt,
    tran_stnd_ped_prnt,
    sprt_stnd_ped_prnt,
    pain_stnd_ped_prnt) {
  data %>%
    dplyr::mutate(
      podci_glob_stnd_ped_prnt = podci_glob(
        data,
        {{ upex_stnd_ped_prnt }},
        {{ tran_stnd_ped_prnt }},
        {{ sprt_stnd_ped_prnt }},
        {{ pain_stnd_ped_prnt }},
        "stnd"
      )
    )
}

#' @describeIn podci_glob Global Normal Pediatric Parent Score
#' @export
#' @examples
#' ## podci_glob_norm_ped_prnt()
#' podci_ped_prnt %>%
#'   podci_upex_stnd_ped_prnt(podci_items("upex", "ped", "prnt")) %>%
#'   podci_tran_stnd_ped_prnt(podci_items("tran", "ped", "prnt")) %>%
#'   podci_sprt_stnd_ped_prnt(podci_items("sprt", "ped", "prnt")) %>%
#'   podci_pain_stnd_ped_prnt(podci_items("pain", "ped", "prnt")) %>%
#'   podci_glob_norm_ped_prnt(
#'     podci_upex_stnd_ped_prnt,
#'     podci_tran_stnd_ped_prnt,
#'     podci_sprt_stnd_ped_prnt,
#'     podci_pain_stnd_ped_prnt
#'   )
#'
podci_glob_norm_ped_prnt <- function(
    data,
    upex_stnd_ped_prnt,
    tran_stnd_ped_prnt,
    sprt_stnd_ped_prnt,
    pain_stnd_ped_prnt) {
  data %>%
    dplyr::mutate(
      podci_glob_norm_ped_prnt = podci_glob(
        data,
        {{ upex_stnd_ped_prnt }},
        {{ tran_stnd_ped_prnt }},
        {{ sprt_stnd_ped_prnt }},
        {{ pain_stnd_ped_prnt }},
        "norm",
        podci_norms("glob", "ped", "prnt", "m"),
        podci_norms("glob", "ped", "prnt", "s")
      )
    )
}

#' @describeIn podci_glob Global Standard Adolescent Parent Score
#' @export
#' @examples
#' ## podci_glob_stnd_ado_prnt()
#' podci_ado_prnt %>%
#'   podci_upex_stnd_ado_prnt(podci_items("upex", "ado", "prnt")) %>%
#'   podci_tran_stnd_ado_prnt(podci_items("tran", "ado", "prnt")) %>%
#'   podci_sprt_stnd_ado_prnt(podci_items("sprt", "ado", "prnt")) %>%
#'   podci_pain_stnd_ado_prnt(podci_items("pain", "ado", "prnt")) %>%
#'   podci_glob_stnd_ado_prnt(
#'     podci_upex_stnd_ado_prnt,
#'     podci_tran_stnd_ado_prnt,
#'     podci_sprt_stnd_ado_prnt,
#'     podci_pain_stnd_ado_prnt
#'   )
#'
podci_glob_stnd_ado_prnt <- function(
    data,
    upex_stnd_ado_prnt,
    tran_stnd_ado_prnt,
    sprt_stnd_ado_prnt,
    pain_stnd_ado_prnt) {
  data %>%
    dplyr::mutate(
      podci_glob_stnd_ado_prnt = podci_glob(
        data,
        {{ upex_stnd_ado_prnt }},
        {{ tran_stnd_ado_prnt }},
        {{ sprt_stnd_ado_prnt }},
        {{ pain_stnd_ado_prnt }},
        "stnd"
      )
    )
}

#' @describeIn podci_glob Global Normal Adolescent Parent Score
#' @export
#' @examples
#' ## podci_glob_norm_ado_prnt()
#' podci_ado_prnt %>%
#'   podci_upex_stnd_ado_prnt(podci_items("upex", "ado", "prnt")) %>%
#'   podci_tran_stnd_ado_prnt(podci_items("tran", "ado", "prnt")) %>%
#'   podci_sprt_stnd_ado_prnt(podci_items("sprt", "ado", "prnt")) %>%
#'   podci_pain_stnd_ado_prnt(podci_items("pain", "ado", "prnt")) %>%
#'   podci_glob_norm_ado_prnt(
#'     podci_upex_stnd_ado_prnt,
#'     podci_tran_stnd_ado_prnt,
#'     podci_sprt_stnd_ado_prnt,
#'     podci_pain_stnd_ado_prnt
#'   )
#'
podci_glob_norm_ado_prnt <- function(
    data,
    upex_stnd_ado_prnt,
    tran_stnd_ado_prnt,
    sprt_stnd_ado_prnt,
    pain_stnd_ado_prnt) {
  data %>%
    dplyr::mutate(
      podci_glob_norm_ado_prnt = podci_glob(
        data,
        {{ upex_stnd_ado_prnt }},
        {{ tran_stnd_ado_prnt }},
        {{ sprt_stnd_ado_prnt }},
        {{ pain_stnd_ado_prnt }},
        "norm",
        podci_norms("glob", "ado", "prnt", "m"),
        podci_norms("glob", "ado", "prnt", "s")
      )
    )
}

#' @describeIn podci_glob Global Standard Adolescent Self Score
#' @export
#' @examples
#' ## podci_glob_stnd_ado_self()
#' podci_ado_self %>%
#'   podci_upex_stnd_ado_self(podci_items("upex", "ado", "self")) %>%
#'   podci_tran_stnd_ado_self(podci_items("tran", "ado", "self")) %>%
#'   podci_sprt_stnd_ado_self(podci_items("sprt", "ado", "self")) %>%
#'   podci_pain_stnd_ado_self(podci_items("pain", "ado", "self")) %>%
#'   podci_glob_stnd_ado_self(
#'     podci_upex_stnd_ado_self,
#'     podci_tran_stnd_ado_self,
#'     podci_sprt_stnd_ado_self,
#'     podci_pain_stnd_ado_self
#'   )
#'
podci_glob_stnd_ado_self <- function(
    data,
    upex_stnd_ado_self,
    tran_stnd_ado_self,
    sprt_stnd_ado_self,
    pain_stnd_ado_self) {
  data %>%
    dplyr::mutate(
      podci_glob_stnd_ado_self = podci_glob(
        data,
        {{ upex_stnd_ado_self }},
        {{ tran_stnd_ado_self }},
        {{ sprt_stnd_ado_self }},
        {{ pain_stnd_ado_self }},
        "stnd"
      )
    )
}

#' @describeIn podci_glob Global Normal Adolescent Self Score
#' @export
#' @examples
#' ## podci_glob_norm_ado_self()
#' podci_ado_self %>%
#'   podci_upex_stnd_ado_self(podci_items("upex", "ado", "self")) %>%
#'   podci_tran_stnd_ado_self(podci_items("tran", "ado", "self")) %>%
#'   podci_sprt_stnd_ado_self(podci_items("sprt", "ado", "self")) %>%
#'   podci_pain_stnd_ado_self(podci_items("pain", "ado", "self")) %>%
#'   podci_glob_norm_ado_self(
#'     podci_upex_stnd_ado_self,
#'     podci_tran_stnd_ado_self,
#'     podci_sprt_stnd_ado_self,
#'     podci_pain_stnd_ado_self
#'   )
#'
podci_glob_norm_ado_self <- function(
    data,
    upex_stnd_ado_self,
    tran_stnd_ado_self,
    sprt_stnd_ado_self,
    pain_stnd_ado_self) {
  data %>%
    dplyr::mutate(
      podci_glob_norm_ado_self = podci_glob(
        data,
        {{ upex_stnd_ado_self }},
        {{ tran_stnd_ado_self }},
        {{ sprt_stnd_ado_self }},
        {{ pain_stnd_ado_self }},
        "norm",
        podci_norms("glob", "ado", "self", "m"),
        podci_norms("glob", "ado", "self", "s")
      )
    )
}
