test_that("podci_hapy_raw_ped_prnt works", {
  expect_equal(
    podci_hapy_raw_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("hapy", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_raw_ped_prnt,
      5.00,
      5.00,
      6.00,
      5.00,
      NA,
      5.00,
      5.00,
      10.00,
      5.00,
      15.00
    )
  )
})

test_that("podci_hapy_mean_ped_prnt works", {
  expect_equal(
    podci_hapy_mean_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("hapy", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_mean_ped_prnt,
      1.00,
      1.00,
      1.20,
      1.00,
      NA,
      1.00,
      1.00,
      2.00,
      1.00,
      3.00
    )
  )
})

test_that("podci_hapy_stnd_ped_prnt works", {
  expect_equal(
    podci_hapy_stnd_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("hapy", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_stnd_ped_prnt,
      100.00,
      100.00,
      95.00,
      100.00,
      NA,
      100.00,
      100.00,
      75.00,
      100.00,
      50.00
    )
  )
})

test_that("podci_hapy_norm_ped_prnt works", {
  expect_equal(
    podci_hapy_norm_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("hapy", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_norm_ped_prnt,
      57.23,
      57.23,
      53.69,
      57.23,
      NA,
      57.23,
      57.23,
      39.51,
      57.23,
      21.79
    )
  )
})

test_that("podci_hapy_raw_ado_prnt works", {
  expect_equal(
    podci_hapy_raw_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("hapy", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_raw_ado_prnt,
      5.00,
      7.00,
      6.00,
      5.00,
      5.00,
      12.00,
      5.00,
      8.00,
      5.00,
      5.00
    )
  )
})

test_that("podci_hapy_mean_ado_prnt works", {
  expect_equal(
    podci_hapy_mean_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("hapy", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_mean_ado_prnt,
      1.00,
      1.40,
      1.20,
      1.00,
      1.00,
      2.40,
      1.00,
      1.60,
      1.00,
      1.00
    )
  )
})

test_that("podci_hapy_stnd_ado_prnt works", {
  expect_equal(
    podci_hapy_stnd_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("hapy", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_stnd_ado_prnt,
      100.00,
      90.00,
      95.00,
      100.00,
      100.00,
      65.00,
      100.00,
      85.00,
      100.00,
      100.00
    )
  )
})

test_that("podci_hapy_norm_ado_prnt works", {
  expect_equal(
    podci_hapy_norm_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("hapy", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_norm_ado_prnt,
      60.31,
      54.69,
      57.50,
      60.31,
      60.31,
      40.65,
      60.31,
      51.88,
      60.31,
      60.31
    )
  )
})

test_that("podci_hapy_raw_ado_self works", {
  expect_equal(
    podci_hapy_raw_ado_self(
      podci_ado_self, paste0("X", podci_items("hapy", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_raw_ado_self,
      9.00,
      5.00,
      5.00,
      6.00,
      9.00,
      8.00,
      5.00,
      5.00,
      10.00,
      5.00
    )
  )
})

test_that("podci_hapy_mean_ado_self works", {
  expect_equal(
    podci_hapy_mean_ado_self(
      podci_ado_self, paste0("X", podci_items("hapy", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_mean_ado_self,
      1.80,
      1.25,
      1.00,
      1.20,
      1.80,
      1.60,
      1.00,
      1.00,
      2.00,
      1.00
    )
  )
})

test_that("podci_hapy_stnd_ado_self works", {
  expect_equal(
    podci_hapy_stnd_ado_self(
      podci_ado_self, paste0("X", podci_items("hapy", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_stnd_ado_self,
      80.00,
      93.75,
      100.00,
      95.00,
      80.00,
      85.00,
      100.00,
      100.00,
      75.00,
      100.00
    )
  )
})

test_that("podci_hapy_norm_ado_self works", {
  expect_equal(
    podci_hapy_norm_ado_self(
      podci_ado_self, paste0("X", podci_items("hapy", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_hapy_norm_ado_self,
      49.07,
      56.80,
      60.31,
      57.50,
      49.07,
      51.88,
      60.31,
      60.31,
      46.27,
      60.31
    )
  )
})
