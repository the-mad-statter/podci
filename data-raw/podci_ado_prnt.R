podci_ado_prnt <- "data-raw/podci_ado_prnt.csv" %>%
  read.csv() %>%
  dplyr::as_tibble()

usethis::use_data(podci_ado_prnt, overwrite = TRUE)
