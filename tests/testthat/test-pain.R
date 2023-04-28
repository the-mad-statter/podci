test_that("podci_pain_raw_ped_prnt works", {
  expect_equal(
    podci_pain_raw_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("pain", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_raw_ped_prnt,
      3.00,
      3.00,
      3.80,
      3.00,
      3.00,
      3.00,
      3.00,
      5.13,
      3.00,
      6.47
    )
  )
})

test_that("podci_pain_mean_ped_prnt works", {
  expect_equal(
    podci_pain_mean_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("pain", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_mean_ped_prnt,
      1.00,
      1.00,
      1.27,
      1.00,
      1.00,
      1.00,
      1.00,
      1.71,
      1.00,
      2.16
    )
  )
})

test_that("podci_pain_stnd_ped_prnt works", {
  expect_equal(
    podci_pain_stnd_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("pain", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_stnd_ped_prnt,
      100.00,
      100.00,
      93.33,
      100.00,
      100.00,
      100.00,
      100.00,
      82.22,
      100.00,
      71.11
    )
  )
})

test_that("podci_pain_norm_ped_prnt works", {
  expect_equal(
    podci_pain_norm_ped_prnt(
      podci_ped_prnt, paste0("X", podci_items("pain", "ped", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_norm_ped_prnt,
      55.48,
      55.48,
      50.64,
      55.48,
      55.48,
      55.48,
      55.48,
      42.57,
      55.48,
      34.49
    )
  )
})

test_that("podci_pain_raw_ado_prnt works", {
  expect_equal(
    podci_pain_raw_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("pain", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_raw_ado_prnt,
      5.13,
      4.33,
      8.27,
      5.67,
      3.00,
      4.33,
      3.00,
      3.00,
      3.80,
      3.00
    )
  )
})

test_that("podci_pain_mean_ado_prnt works", {
  expect_equal(
    podci_pain_mean_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("pain", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_mean_ado_prnt,
      1.71,
      1.44,
      2.76,
      1.89,
      1.00,
      1.44,
      1.00,
      1.00,
      1.27,
      1.00
    )
  )
})

test_that("podci_pain_stnd_ado_prnt works", {
  expect_equal(
    podci_pain_stnd_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("pain", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_stnd_ado_prnt,
      82.22,
      88.89,
      56.11,
      77.78,
      100.00,
      88.89,
      100.00,
      100.00,
      93.33,
      100.00
    )
  )
})

test_that("podci_pain_norm_ado_prnt works", {
  expect_equal(
    podci_pain_norm_ado_prnt(
      podci_ado_prnt, paste0("X", podci_items("pain", "ado", "prnt"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_norm_ado_prnt,
      46.88,
      50.76,
      31.69,
      44.29,
      57.22,
      50.76,
      57.22,
      57.22,
      53.34,
      57.22
    )
  )
})

test_that("podci_pain_raw_ado_self works", {
  expect_equal(
    podci_pain_raw_ado_self(
      podci_ado_self, paste0("X", podci_items("pain", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_raw_ado_self,
      10.07,
      NA,
      6.67,
      3.00,
      2.80,
      5.60,
      3.00,
      8.27,
      2.00,
      7.47
    )
  )
})

test_that("podci_pain_mean_ado_self works", {
  expect_equal(
    podci_pain_mean_ado_self(
      podci_ado_self, paste0("X", podci_items("pain", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_mean_ado_self,
      3.36,
      NA,
      2.22,
      1.00,
      1.40,
      1.87,
      1.00,
      2.76,
      1.00,
      2.49
    )
  )
})

test_that("podci_pain_stnd_ado_self works", {
  expect_equal(
    podci_pain_stnd_ado_self(
      podci_ado_self, paste0("X", podci_items("pain", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_stnd_ado_self,
      41.11,
      NA,
      69.44,
      100.00,
      90.00,
      78.33,
      100.00,
      56.11,
      100.00,
      62.78
    )
  )
})

test_that("podci_pain_norm_ado_self works", {
  expect_equal(
    podci_pain_norm_ado_self(
      podci_ado_self, paste0("X", podci_items("pain", "ado", "self"))
    ) %>%
      round_podci_output_for_tests(),
    dplyr::tribble(
      ~podci_pain_norm_ado_self,
      22.97,
      NA,
      39.45,
      57.22,
      51.40,
      44.62,
      57.22,
      31.69,
      57.22,
      35.57
    )
  )
})
