test_that("podci_glob_stnd_ped_prnt works", {
  expect_equal(
    podci_ped_prnt %>%
      podci_upex_stnd_ped_prnt(podci_items("upex", "ped", "prnt")) %>%
      podci_tran_stnd_ped_prnt(podci_items("tran", "ped", "prnt")) %>%
      podci_sprt_stnd_ped_prnt(podci_items("sprt", "ped", "prnt")) %>%
      podci_pain_stnd_ped_prnt(podci_items("pain", "ped", "prnt")) %>%
      podci_glob_stnd_ped_prnt(
        podci_upex_stnd_ped_prnt,
        podci_tran_stnd_ped_prnt,
        podci_sprt_stnd_ped_prnt,
        podci_pain_stnd_ped_prnt
      ) %>%
      round_podci_output_for_tests(),
    dplyr::tibble(
      podci_upex_stnd_ped_prnt =
        c(93.33, 95.24, 66.67, 79.17, 42.86, 100, 87.5, 83.33, 90.48, 33.33),
      podci_tran_stnd_ped_prnt =
        c(96.3, 97.22, 67.42, 96.67, 93.33, 100, 100, 90.91, 90.00, 33.33),
      podci_sprt_stnd_ped_prnt =
        c(92.86, 78.7, 71.97, 96.67, 84.38, 100, 96.97, 81.48, 91.67, 16.67),
      podci_pain_stnd_ped_prnt =
        c(100, 100, 93.33, 100, 100, 100, 100, 82.22, 100, 71.11),
      podci_glob_stnd_ped_prnt =
        c(95.62, 92.79, 74.85, 93.12, 80.14, 100, 96.12, 84.49, 93.04, 38.61)
    )
  )
})

test_that("podci_glob_norm_ped_prnt works", {
  expect_equal(
    podci_ped_prnt %>%
      podci_upex_stnd_ped_prnt(podci_items("upex", "ped", "prnt")) %>%
      podci_tran_stnd_ped_prnt(podci_items("tran", "ped", "prnt")) %>%
      podci_sprt_stnd_ped_prnt(podci_items("sprt", "ped", "prnt")) %>%
      podci_pain_stnd_ped_prnt(podci_items("pain", "ped", "prnt")) %>%
      podci_glob_norm_ped_prnt(
        podci_upex_stnd_ped_prnt,
        podci_tran_stnd_ped_prnt,
        podci_sprt_stnd_ped_prnt,
        podci_pain_stnd_ped_prnt
      ) %>%
      round_podci_output_for_tests(),
    dplyr::tibble(
      podci_upex_stnd_ped_prnt =
        c(93.33, 95.24, 66.67, 79.17, 42.86, 100, 87.5, 83.33, 90.48, 33.33),
      podci_tran_stnd_ped_prnt =
        c(96.3, 97.22, 67.42, 96.67, 93.33, 100, 100, 90.91, 90.00, 33.33),
      podci_sprt_stnd_ped_prnt =
        c(92.86, 78.7, 71.97, 96.67, 84.38, 100, 96.97, 81.48, 91.67, 16.67),
      podci_pain_stnd_ped_prnt =
        c(100, 100, 93.33, 100, 100, 100, 100, 82.22, 100, 71.11),
      podci_glob_norm_ped_prnt =
        c(52.36, 48.53, 24.27, 48.98, 31.42, 58.28, 53.03, 37.30, 48.86, -24.73)
    )
  )
})

test_that("podci_glob_stnd_ado_prnt works", {
  expect_equal(
    podci_ado_prnt %>%
      podci_upex_stnd_ado_prnt(podci_items("upex", "ado", "prnt")) %>%
      podci_tran_stnd_ado_prnt(podci_items("tran", "ado", "prnt")) %>%
      podci_sprt_stnd_ado_prnt(podci_items("sprt", "ado", "prnt")) %>%
      podci_pain_stnd_ado_prnt(podci_items("pain", "ado", "prnt")) %>%
      podci_glob_stnd_ado_prnt(
        podci_upex_stnd_ado_prnt,
        podci_tran_stnd_ado_prnt,
        podci_sprt_stnd_ado_prnt,
        podci_pain_stnd_ado_prnt
      ) %>%
      round_podci_output_for_tests(),
    dplyr::tibble(
      podci_upex_stnd_ado_prnt =
        c(100, 79.17, 79.17, 100, 100, 95.83, 100, 95.83, 100, 100),
      podci_tran_stnd_ado_prnt =
        c(100, 96.97, 100, 100, 100, 100, 100, 90.91, 90.91, 81.82),
      podci_sprt_stnd_ado_prnt =
        c(100, 67.36, 73.48, 100, 100, 90.28, 90.91, 83.33, 91.67, 81.82),
      podci_pain_stnd_ado_prnt =
        c(82.22, 88.89, 56.11, 77.78, 100, 88.89, 100, 100, 93.33, 100),
      podci_glob_stnd_ado_prnt =
        c(95.56, 83.1, 77.19, 94.44, 100, 93.75, 97.73, 92.52, 93.98, 90.91)
    )
  )
})

test_that("podci_glob_norm_ado_prnt works", {
  expect_equal(
    podci_ado_prnt %>%
      podci_upex_stnd_ado_prnt(podci_items("upex", "ado", "prnt")) %>%
      podci_tran_stnd_ado_prnt(podci_items("tran", "ado", "prnt")) %>%
      podci_sprt_stnd_ado_prnt(podci_items("sprt", "ado", "prnt")) %>%
      podci_pain_stnd_ado_prnt(podci_items("pain", "ado", "prnt")) %>%
      podci_glob_norm_ado_prnt(
        podci_upex_stnd_ado_prnt,
        podci_tran_stnd_ado_prnt,
        podci_sprt_stnd_ado_prnt,
        podci_pain_stnd_ado_prnt
      ) %>%
      round_podci_output_for_tests(),
    dplyr::tibble(
      podci_upex_stnd_ado_prnt =
        c(100, 79.17, 79.17, 100, 100, 95.83, 100, 95.83, 100, 100),
      podci_tran_stnd_ado_prnt =
        c(100, 96.97, 100, 100, 100, 100, 100, 90.91, 90.91, 81.82),
      podci_sprt_stnd_ado_prnt =
        c(100, 67.36, 73.48, 100, 100, 90.28, 90.91, 83.33, 91.67, 81.82),
      podci_pain_stnd_ado_prnt =
        c(82.22, 88.89, 56.11, 77.78, 100, 88.89, 100, 100, 93.33, 100),
      podci_glob_norm_ado_prnt =
        c(51.05, 34.17, 26.16, 49.55, 57.08, 48.6, 53.99, 46.94, 48.91, 44.75)
    )
  )
})

test_that("podci_glob_stnd_ado_self works", {
  expect_equal(
    podci_ado_self %>%
      podci_upex_stnd_ado_self(podci_items("upex", "ado", "self")) %>%
      podci_tran_stnd_ado_self(podci_items("tran", "ado", "self")) %>%
      podci_sprt_stnd_ado_self(podci_items("sprt", "ado", "self")) %>%
      podci_pain_stnd_ado_self(podci_items("pain", "ado", "self")) %>%
      podci_glob_stnd_ado_self(
        podci_upex_stnd_ado_self,
        podci_tran_stnd_ado_self,
        podci_sprt_stnd_ado_self,
        podci_pain_stnd_ado_self
      ) %>%
      round_podci_output_for_tests(),
    dplyr::tibble(
      podci_upex_stnd_ado_self =
        c(95.83, 95.83, 100, 100, 100, 25, 100, 91.67, 100, 100),
      podci_tran_stnd_ado_self =
        c(96.97, 100, 100, 100, 96.97, 96.97, 100, 100, 100, 100),
      podci_sprt_stnd_ado_self =
        c(80, 82.14, 84.85, 91.67, 96.97, 100, 94.44, 96.97, 87.5, 100),
      podci_pain_stnd_ado_self =
        c(41.11, NA, 69.44, 100, 90, 78.33, 100, 56.11, 100, 62.78),
      podci_glob_stnd_ado_self =
        c(78.48, NA, 88.57, 97.92, 95.98, 75.08, 98.61, 86.19, 96.88, 90.69)
    )
  )
})

test_that("podci_glob_norm_ado_self works", {
  expect_equal(
    podci_ado_self %>%
      podci_upex_stnd_ado_self(podci_items("upex", "ado", "self")) %>%
      podci_tran_stnd_ado_self(podci_items("tran", "ado", "self")) %>%
      podci_sprt_stnd_ado_self(podci_items("sprt", "ado", "self")) %>%
      podci_pain_stnd_ado_self(podci_items("pain", "ado", "self")) %>%
      podci_glob_norm_ado_self(
        podci_upex_stnd_ado_self,
        podci_tran_stnd_ado_self,
        podci_sprt_stnd_ado_self,
        podci_pain_stnd_ado_self
      ) %>%
      round_podci_output_for_tests(),
    dplyr::tibble(
      podci_upex_stnd_ado_self =
        c(95.83, 95.83, 100, 100, 100, 25, 100, 91.67, 100, 100),
      podci_tran_stnd_ado_self =
        c(96.97, 100, 100, 100, 96.97, 96.97, 100, 100, 100, 100),
      podci_sprt_stnd_ado_self =
        c(80, 82.14, 84.85, 91.67, 96.97, 100, 94.44, 96.97, 87.5, 100),
      podci_pain_stnd_ado_self =
        c(41.11, NA, 69.44, 100, 90, 78.33, 100, 56.11, 100, 62.78),
      podci_glob_norm_ado_self =
        c(27.91, NA, 41.59, 54.25, 51.63, 23.3, 55.19, 38.35, 52.84, 44.46)
    )
  )
})
