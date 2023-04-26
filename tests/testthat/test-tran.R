test_that("podci_tran_raw_ped works", {
  expect_equal(
    podci_tran_raw_ped(podci, paste0("X", podci_items("tran"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_tran_raw_ped = c(9.00, 14.75, 12.00))
  )
})

test_that("podci_tran_mean_ped works", {
  expect_equal(
    podci_tran_mean_ped(podci, paste0("X", podci_items("tran"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_tran_mean_ped = c(1.00, 1.34, 1.09))
  )
})

test_that("podci_tran_stnd_ped works", {
  expect_equal(
    podci_tran_stnd_ped(podci, paste0("X", podci_items("tran"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_tran_stnd_ped = c(100.00, 88.64, 96.97))
  )
})

test_that("podci_tran_norm_ped works", {
  expect_equal(
    podci_tran_norm_ped(podci, paste0("X", podci_items("tran"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_tran_norm_ped = c(52.92, 32.98, 47.61))
  )
})

test_that("podci_tran_raw_ado works", {
  expect_equal(
    podci_tran_raw_ado(podci, paste0("X", podci_items("tran"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_tran_raw_ado = c(9.00, 14.75, 12.00))
  )
})

test_that("podci_tran_mean_ado works", {
  expect_equal(
    podci_tran_mean_ado(podci, paste0("X", podci_items("tran"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_tran_mean_ado = c(1.00, 1.34, 1.09))
  )
})

test_that("podci_tran_stnd_ado works", {
  expect_equal(
    podci_tran_stnd_ado(podci, paste0("X", podci_items("tran"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_tran_stnd_ado = c(100.00, 88.64, 96.97))
  )
})

test_that("podci_tran_norm_ado works", {
  expect_equal(
    podci_tran_norm_ado(podci, paste0("X", podci_items("tran"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_tran_norm_ado = c(52.00, 29.27, 45.94))
  )
})
