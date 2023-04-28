test_that("podci_sprt_raw_ped_prnt works", {
  expect_equal(
    podci_sprt_raw_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("sprt", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_raw_ped_prnt,
      8.50,
      14.75,
      20.25,
      11.00,
      11.75,
      12.00,
      12.00,
      11.75,
      12.00,
      34.75
    )
  )
})

test_that("podci_sprt_mean_ped_prnt works", {
  expect_equal(
    podci_sprt_mean_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("sprt", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_mean_ped_prnt,
      1.21,
      1.64,
      1.84,
      1.10,
      1.47,
      1.00,
      1.09,
      1.31,
      1.00,
      3.16
    )
  )
})

test_that("podci_sprt_stnd_ped_prnt works", {
  expect_equal(
    podci_sprt_stnd_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("sprt", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_stnd_ped_prnt,
      92.86,
      78.70,
      71.97,
      96.67,
      84.38,
      100.00,
      96.97,
      89.81,
      100.00,
      28.03
    )
  )
})

test_that("podci_sprt_norm_ped_prnt works", {
  expect_equal(
    podci_sprt_norm_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("sprt", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_norm_ped_prnt,
      50.41,
      36.55,
      29.96,
      54.13,
      42.10,
      57.40,
      54.43,
      47.43,
      57.40,
      -13.04
    )
  )
})

test_that("podci_sprt_raw_ado_prnt works", {
  expect_equal(
    podci_sprt_raw_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("sprt", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_raw_ado_prnt,
      12.00,
      23.75,
      19.75,
      12.00,
      12.00,
      15.50,
      14.00,
      12.00,
      12.00,
      11.00
    )
  )
})

test_that("podci_sprt_mean_ado_prnt works", {
  expect_equal(
    podci_sprt_mean_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("sprt", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_mean_ado_prnt,
      1.00,
      1.98,
      1.80,
      1.00,
      1.00,
      1.29,
      1.27,
      1.20,
      1.00,
      1.00
    )
  )
})

test_that("podci_sprt_stnd_ado_prnt works", {
  expect_equal(
    podci_sprt_stnd_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("sprt", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_stnd_ado_prnt,
      100.00,
      67.36,
      73.48,
      100.00,
      100.00,
      90.28,
      90.91,
      93.33,
      100.00,
      100.00
    )
  )
})

test_that("podci_sprt_norm_ado_prnt works", {
  expect_equal(
    podci_sprt_norm_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("sprt", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_norm_ado_prnt,
      55.79,
      25.93,
      31.53,
      55.79,
      55.79,
      46.90,
      47.48,
      49.69,
      55.79,
      55.79
    )
  )
})

test_that("podci_sprt_raw_ado_self works", {
  expect_equal(
    podci_sprt_raw_ado_self(
      podci_ado_self, paste0("X", podci_items("sprt", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_raw_ado_self,
      16.00,
      10.75,
      16.00,
      15.00,
      12.00,
      12.00,
      14.00,
      12.00,
      16.50,
      12.00
    )
  )
})

test_that("podci_sprt_mean_ado_self works", {
  expect_equal(
    podci_sprt_mean_ado_self(
      podci_ado_self, paste0("X", podci_items("sprt", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_mean_ado_self,
      1.60,
      1.54,
      1.45,
      1.25,
      1.09,
      1.00,
      1.17,
      1.09,
      1.38,
      1.00
    )
  )
})

test_that("podci_sprt_stnd_ado_self works", {
  expect_equal(
    podci_sprt_stnd_ado_self(
      podci_ado_self, paste0("X", podci_items("sprt", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_stnd_ado_self,
      80.00,
      82.14,
      84.85,
      91.67,
      96.97,
      100.00,
      94.44,
      96.97,
      87.50,
      100.00
    )
  )
})

test_that("podci_sprt_norm_ado_self works", {
  expect_equal(
    podci_sprt_norm_ado_self(
      podci_ado_self, paste0("X", podci_items("sprt", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_sprt_norm_ado_self,
      37.50,
      39.46,
      41.93,
      48.17,
      53.02,
      55.79,
      50.71,
      53.02,
      44.36,
      55.79
    )
  )
})
