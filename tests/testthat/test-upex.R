test_that("podci_upex_raw_ped works", {
  expect_equal(
    podci_upex_raw_ped(podci, paste0("X", podci_items("upex"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_upex_raw_ped = c(8.00, 7.00, 9.00))
  )
})

test_that("podci_upex_mean_ped works", {
  expect_equal(
    podci_upex_mean_ped(podci, paste0("X", podci_items("upex"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_upex_mean_ped = c(1.60, 1.40, 1.29))
  )
})

test_that("podci_upex_stnd_ped works", {
  expect_equal(
    podci_upex_stnd_ped(podci, paste0("X", podci_items("upex"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_upex_stnd_ped = c(80.00, 86.67, 90.48))
  )
})

test_that("podci_upex_norm_ped works", {
  expect_equal(
    podci_upex_norm_ped(podci, paste0("X", podci_items("upex"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_upex_norm_ped = c(39.64, 45.44, 48.75))
  )
})

test_that("podci_upex_raw_ado works", {
  expect_equal(
    podci_upex_raw_ado(podci, paste0("X", podci_items("upex"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_upex_raw_ado = c(8.00, 7.00, 9.00))
  )
})

test_that("podci_upex_mean_ado works", {
  expect_equal(
    podci_upex_mean_ado(podci, paste0("X", podci_items("upex"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_upex_mean_ado = c(1.60, 1.40, 1.29))
  )
})

test_that("podci_upex_stnd_ado works", {
  expect_equal(
    podci_upex_stnd_ado(podci, paste0("X", podci_items("upex"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_upex_stnd_ado = c(80.00, 86.67, 90.48))
  )
})

test_that("podci_upex_norm_ado works", {
  expect_equal(
    podci_upex_norm_ado(podci, paste0("X", podci_items("upex"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_upex_norm_ado = c(12.76, 26.01, 33.58))
  )
})
