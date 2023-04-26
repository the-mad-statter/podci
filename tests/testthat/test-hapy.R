test_that("podci_hapy_raw_ped works", {
  expect_equal(
    podci_hapy_raw_ped(podci, paste0("X", podci_items("hapy"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_hapy_raw_ped = c(5.00, 5.00, 8.00))
  )
})

test_that("podci_hapy_mean_ped works", {
  expect_equal(
    podci_hapy_mean_ped(podci, paste0("X", podci_items("hapy"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_hapy_mean_ped = c(1.00, 1.00, 1.60))
  )
})

test_that("podci_hapy_stnd_ped works", {
  expect_equal(
    podci_hapy_stnd_ped(podci, paste0("X", podci_items("hapy"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_hapy_stnd_ped = c(100.00, 100.00, 85.00))
  )
})

test_that("podci_hapy_norm_ped works", {
  expect_equal(
    podci_hapy_norm_ped(podci, paste0("X", podci_items("hapy"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_hapy_norm_ped = c(57.23, 57.23, 46.60))
  )
})

test_that("podci_hapy_raw_ado works", {
  expect_equal(
    podci_hapy_raw_ado(podci, paste0("X", podci_items("hapy"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_hapy_raw_ado = c(5.00, 5.00, 8.00))
  )
})

test_that("podci_hapy_mean_ado works", {
  expect_equal(
    podci_hapy_mean_ado(podci, paste0("X", podci_items("hapy"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_hapy_mean_ado = c(1.00, 1.00, 1.60))
  )
})

test_that("podci_hapy_stnd_ado works", {
  expect_equal(
    podci_hapy_stnd_ado(podci, paste0("X", podci_items("hapy"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_hapy_stnd_ado = c(100.00, 100.00, 85.00))
  )
})

test_that("podci_hapy_norm_ado works", {
  expect_equal(
    podci_hapy_norm_ado(podci, paste0("X", podci_items("hapy"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_hapy_norm_ado = c(60.00, 60.00, 51.67))
  )
})
