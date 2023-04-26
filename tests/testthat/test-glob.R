test_that("podci_glob_stnd_ped works", {
  expect_equal(
    podci %>%
      podci_upex_stnd_ped(podci_items("upex")) %>%
      podci_tran_stnd_ped(podci_items("tran")) %>%
      podci_sprt_stnd_ped(podci_items("sprt")) %>%
      podci_pain_stnd_ped(podci_items("pain")) %>%
      podci_glob_stnd_ped(
        podci_upex_stnd_ped,
        podci_tran_stnd_ped,
        podci_sprt_stnd_ped,
        podci_pain_stnd_ped
      ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(
      podci_upex_stnd_ped = c(80.00, 86.67, 90.48),
      podci_tran_stnd_ped = c(100.00, 88.64, 96.97),
      podci_sprt_stnd_ped = c(88.54, 90.83, 82.58),
      podci_pain_stnd_ped = c(100.00, 100.00, 85.19),
      podci_glob_stnd_ped = c(92.14, 91.53, 88.80)
    )
  )
})

test_that("podci_glob_norm_ped works", {
  expect_equal(
    podci %>%
      podci_upex_stnd_ped(podci_items("upex")) %>%
      podci_tran_stnd_ped(podci_items("tran")) %>%
      podci_sprt_stnd_ped(podci_items("sprt")) %>%
      podci_pain_stnd_ped(podci_items("pain")) %>%
      podci_glob_norm_ped(
        podci_upex_stnd_ped,
        podci_tran_stnd_ped,
        podci_sprt_stnd_ped,
        podci_pain_stnd_ped
      ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(
      podci_upex_stnd_ped = c(80.00, 86.67, 90.48),
      podci_tran_stnd_ped = c(100.00, 88.64, 96.97),
      podci_sprt_stnd_ped = c(88.54, 90.83, 82.58),
      podci_pain_stnd_ped = c(100.00, 100.00, 85.19),
      podci_glob_norm_ped = c(47.64, 46.83, 43.14)
    )
  )
})

test_that("podci_glob_stnd_ado works", {
  expect_equal(
    podci %>%
      podci_upex_stnd_ado(podci_items("upex")) %>%
      podci_tran_stnd_ado(podci_items("tran")) %>%
      podci_sprt_stnd_ado(podci_items("sprt")) %>%
      podci_pain_stnd_ado(podci_items("pain")) %>%
      podci_glob_stnd_ado(
        podci_upex_stnd_ado,
        podci_tran_stnd_ado,
        podci_sprt_stnd_ado,
        podci_pain_stnd_ado
      ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(
      podci_upex_stnd_ado = c(80.00, 86.67, 90.48),
      podci_tran_stnd_ado = c(100.00, 88.64, 96.97),
      podci_sprt_stnd_ado = c(88.54, 90.83, 82.58),
      podci_pain_stnd_ado = c(100.00, 100.00, 85.19),
      podci_glob_stnd_ado = c(92.14, 91.53, 88.80)
    )
  )
})

test_that("podci_glob_norm_ado works", {
  expect_equal(
    podci %>%
      podci_upex_stnd_ado(podci_items("upex")) %>%
      podci_tran_stnd_ado(podci_items("tran")) %>%
      podci_sprt_stnd_ado(podci_items("sprt")) %>%
      podci_pain_stnd_ado(podci_items("pain")) %>%
      podci_glob_norm_ado(
        podci_upex_stnd_ado,
        podci_tran_stnd_ado,
        podci_sprt_stnd_ado,
        podci_pain_stnd_ado
      ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(
      podci_upex_stnd_ado = c(80.00, 86.67, 90.48),
      podci_tran_stnd_ado = c(100.00, 88.64, 96.97),
      podci_sprt_stnd_ado = c(88.54, 90.83, 82.58),
      podci_pain_stnd_ado = c(100.00, 100.00, 85.19),
      podci_glob_norm_ado = c(45.91, 45.05, 41.15)
    )
  )
})
