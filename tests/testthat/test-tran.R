test_that("podci_tran_raw_ped_prnt works", {
  expect_equal(
    podci_tran_raw_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("tran", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_raw_ped_prnt,
      10.00,
      9.75,
      21.75,
      11.00,
      12.00,
      10.00,
      11.00,
      14.00,
      13.00,
      33.00
    )
  )
})

test_that("podci_tran_mean_ped_prnt works", {
  expect_equal(
    podci_tran_mean_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("tran", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_mean_ped_prnt,
      1.11,
      1.08,
      1.98,
      1.10,
      1.20,
      1.00,
      1.00,
      1.27,
      1.30,
      3.00
    )
  )
})

test_that("podci_tran_stnd_ped_prnt works", {
  expect_equal(
    podci_tran_stnd_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("tran", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_stnd_ped_prnt,
      96.30,
      97.22,
      67.42,
      96.67,
      93.33,
      100.00,
      100.00,
      90.91,
      90.00,
      33.33
    )
  )
})

test_that("podci_tran_norm_ped_prnt works", {
  expect_equal(
    podci_tran_norm_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("tran", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_norm_ped_prnt,
      46.42,
      48.05,
      -4.23,
      47.07,
      41.22,
      52.92,
      52.92,
      36.97,
      35.38,
      -64.05
    )
  )
})

test_that("podci_tran_raw_ado_prnt works", {
  expect_equal(
    podci_tran_raw_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("tran", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_raw_ado_prnt,
      11.00,
      12.00,
      11.00,
      11.00,
      11.00,
      11.00,
      11.00,
      14.00,
      14.00,
      17.00
    )
  )
})

test_that("podci_tran_mean_ado_prnt works", {
  expect_equal(
    podci_tran_mean_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("tran", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_mean_ado_prnt,
      1.00,
      1.09,
      1.00,
      1.00,
      1.00,
      1.00,
      1.00,
      1.27,
      1.27,
      1.55
    )
  )
})

test_that("podci_tran_stnd_ado_prnt works", {
  expect_equal(
    podci_tran_stnd_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("tran", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_stnd_ado_prnt,
      100.00,
      96.97,
      100.00,
      100.00,
      100.00,
      100.00,
      100.00,
      90.91,
      90.91,
      81.82
    )
  )
})

test_that("podci_tran_norm_ado_prnt works", {
  expect_equal(
    podci_tran_norm_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("tran", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_norm_ado_prnt,
      51.88,
      45.35,
      51.88,
      51.88,
      51.88,
      51.88,
      51.88,
      32.30,
      32.30,
      12.71
    )
  )
})

test_that("podci_tran_raw_ado_self works", {
  expect_equal(
    podci_tran_raw_ado_self(
      podci_ado_self, paste0("X", podci_items("tran", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_raw_ado_self,
      12.00,
      9.00,
      11.00,
      11.00,
      12.00,
      12.00,
      11.00,
      11.00,
      11.00,
      11.00
    )
  )
})

test_that("podci_tran_mean_ado_self works", {
  expect_equal(
    podci_tran_mean_ado_self(
      podci_ado_self, paste0("X", podci_items("tran", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_mean_ado_self,
      1.09,
      1.00,
      1.00,
      1.00,
      1.09,
      1.09,
      1.00,
      1.00,
      1.00,
      1.00
    )
  )
})

test_that("podci_tran_stnd_ado_self works", {
  expect_equal(
    podci_tran_stnd_ado_self(
      podci_ado_self, paste0("X", podci_items("tran", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_stnd_ado_self,
      96.97,
      100.00,
      100.00,
      100.00,
      96.97,
      96.97,
      100.00,
      100.00,
      100.00,
      100.00
    )
  )
})

test_that("podci_tran_norm_ado_self works", {
  expect_equal(
    podci_tran_norm_ado_self(
      podci_ado_self, paste0("X", podci_items("tran", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_tran_norm_ado_self,
      45.35,
      51.88,
      51.88,
      51.88,
      45.35,
      45.35,
      51.88,
      51.88,
      51.88,
      51.88
    )
  )
})
