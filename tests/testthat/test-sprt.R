test_that("podci_sprt_raw_ped works", {
  expect_equal(
    podci_sprt_raw_ped(
      podci,
      paste0("X", podci_items("sprt"))
    ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_sprt_raw_ped = c(10.75, 12.75, 16.75))
  )
})

test_that("podci_sprt_mean_ped works", {
  expect_equal(
    podci_sprt_mean_ped(
      podci,
      paste0("X", podci_items("sprt"))
    ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_sprt_mean_ped = c(1.34, 1.27, 1.52))
  )
})

test_that("podci_sprt_stnd_ped works", {
  expect_equal(
    podci_sprt_stnd_ped(
      podci,
      paste0("X", podci_items("sprt"))
    ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_sprt_stnd_ped = c(88.54, 90.83, 82.58))
  )
})

test_that("podci_sprt_norm_ped works", {
  expect_equal(
    podci_sprt_norm_ped(
      podci,
      paste0("X", podci_items("sprt"))
    ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_sprt_norm_ped = c(46.18, 48.43, 40.34))
  )
})

test_that("podci_sprt_raw_ado works", {
  expect_equal(
    podci_sprt_raw_ado(
      podci,
      paste0("X", podci_items("sprt"))
    ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_sprt_raw_ado = c(10.75, 12.75, 16.75))
  )
})

test_that("podci_sprt_mean_ado works", {
  expect_equal(
    podci_sprt_mean_ado(
      podci,
      paste0("X", podci_items("sprt"))
    ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_sprt_mean_ado = c(1.34, 1.27, 1.52))
  )
})

test_that("podci_sprt_stnd_ado works", {
  expect_equal(
    podci_sprt_stnd_ado(
      podci,
      paste0("X", podci_items("sprt"))
    ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_sprt_stnd_ado = c(88.54, 90.83, 82.58))
  )
})

test_that("podci_sprt_norm_ado works", {
  expect_equal(
    podci_sprt_norm_ado(
      podci,
      paste0("X", podci_items("sprt"))
    ) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_sprt_norm_ado = c(45.04, 47.12, 39.61))
  )
})
