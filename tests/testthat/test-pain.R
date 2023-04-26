test_that("podci_pain_raw_ped works", {
  expect_equal(
    podci_pain_raw_ped(podci, paste0("X", podci_items("pain"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_pain_raw_ped = c(3.00, 3.00, 4.33))
  )
})

test_that("podci_pain_mean_ped works", {
  expect_equal(
    podci_pain_mean_ped(podci, paste0("X", podci_items("pain"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_pain_mean_ped = c(1.00, 1.00, 1.44))
  )
})

test_that("podci_pain_stnd_ped works", {
  expect_equal(
    podci_pain_stnd_ped(podci, paste0("X", podci_items("pain"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_pain_stnd_ped = c(100.00, 100.00, 85.19))
  )
})

test_that("podci_pain_norm_ped works", {
  expect_equal(
    podci_pain_norm_ped(podci, paste0("X", podci_items("pain"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_pain_norm_ped = c(55.48, 55.48, 44.72))
  )
})

test_that("podci_pain_raw_ado works", {
  expect_equal(
    podci_pain_raw_ado(podci, paste0("X", podci_items("pain"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_pain_raw_ado = c(3.00, 3.00, 4.33))
  )
})

test_that("podci_pain_mean_ado works", {
  expect_equal(
    podci_pain_mean_ado(podci, paste0("X", podci_items("pain"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_pain_mean_ado = c(1.00, 1.00, 1.44))
  )
})

test_that("podci_pain_stnd_ado works", {
  expect_equal(
    podci_pain_stnd_ado(podci, paste0("X", podci_items("pain"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_pain_stnd_ado = c(100.00, 100.00, 85.19))
  )
})

test_that("podci_pain_norm_ado works", {
  expect_equal(
    podci_pain_norm_ado(podci, paste0("X", podci_items("pain"))) %>%
      dplyr::select(dplyr::starts_with("podci")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(., digits = 2))),
    dplyr::tibble(podci_pain_norm_ado = c(57.06, 57.06, 48.34))
  )
})
