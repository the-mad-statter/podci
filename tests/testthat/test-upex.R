test_that("podci_upex_raw_ped_prnt works", {
  expect_equal(
    podci_upex_raw_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("upex", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_raw_ped_prnt,
      6.00,
      8.00,
      16.00,
      13.00,
      19.00,
      7.00,
      11.00,
      12.00,
      9.00,
      24.00
    )
  )
})

test_that("podci_upex_mean_ped_prnt works", {
  expect_equal(
    podci_upex_mean_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("upex", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_mean_ped_prnt,
      1.20,
      1.14,
      2.00,
      1.62,
      2.71,
      1.00,
      1.38,
      1.50,
      1.29,
      3.00
    )
  )
})

test_that("podci_upex_stnd_ped_prnt works", {
  expect_equal(
    podci_upex_stnd_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("upex", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_stnd_ped_prnt,
      93.33,
      95.24,
      66.67,
      79.17,
      42.86,
      100.00,
      87.5,
      83.33,
      90.48,
      33.33
    )
  )
})

test_that("podci_upex_norm_ped_prnt works", {
  expect_equal(
    podci_upex_norm_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("upex", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_norm_ped_prnt,
      51.23,
      52.89,
      28.04,
      38.91,
      7.33,
      57.03,
      46.16,
      42.54,
      48.75,
      -0.95
    )
  )
})

test_that("podci_upex_raw_ado_prnt works", {
  expect_equal(
    podci_upex_raw_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("upex", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_raw_ado_prnt,
      8.00,
      13.00,
      13.00,
      8.00,
      8.00,
      9.00,
      8.00,
      9.00,
      8.00,
      8.00
    )
  )
})

test_that("podci_upex_mean_ado_prnt works", {
  expect_equal(
    podci_upex_mean_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("upex", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_mean_ado_prnt,
      1.00,
      1.62,
      1.62,
      1.00,
      1.00,
      1.12,
      1.00,
      1.12,
      1.00,
      1.00
    )
  )
})

test_that("podci_upex_stnd_ado_prnt works", {
  expect_equal(
    podci_upex_stnd_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("upex", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_stnd_ado_prnt,
      100.00,
      79.17,
      79.17,
      100.00,
      100.00,
      95.83,
      100.00,
      95.83,
      100.00,
      100.00
    )
  )
})

test_that("podci_upex_norm_ado_prnt works", {
  expect_equal(
    podci_upex_norm_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("upex", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_norm_ado_prnt,
      52.50,
      11.10,
      11.10,
      52.50,
      52.50,
      44.22,
      52.50,
      44.22,
      52.50,
      52.50
    )
  )
})

test_that("podci_upex_raw_ado_self works", {
  expect_equal(
    podci_upex_raw_ado_self(
      podci_ado_self, paste0("X", podci_items("upex", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_raw_ado_self,
      9.00,
      9.00,
      8.00,
      8.00,
      8.00,
      26.00,
      8.00,
      10.00,
      8.00,
      8.00
    )
  )
})

test_that("podci_upex_mean_ado_self works", {
  expect_equal(
    podci_upex_mean_ado_self(
      podci_ado_self, paste0("X", podci_items("upex", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_mean_ado_self,
      1.12,
      1.12,
      1.00,
      1.00,
      1.00,
      3.25,
      1.00,
      1.25,
      1.00,
      1.00
    )
  )
})

test_that("podci_upex_stnd_ado_self works", {
  expect_equal(
    podci_upex_stnd_ado_self(
      podci_ado_self, paste0("X", podci_items("upex", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_stnd_ado_self,
      95.83,
      95.83,
      100.00,
      100.00,
      100.00,
      25.00,
      100.00,
      91.67,
      100.00,
      100.00
    )
  )
})

test_that("podci_upex_norm_ado_self works", {
  expect_equal(
    podci_upex_norm_ado_self(
      podci_ado_self, paste0("X", podci_items("upex", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_upex_norm_ado_self,
      44.22,
      44.22,
      52.50,
      52.50,
      52.50,
      -96.54,
      52.50,
      35.94,
      52.50,
      52.50
    )
  )
})
