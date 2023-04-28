podci_ped_prnt <- "data-raw/podci_ped_prnt.csv" %>%
  read.csv() %>%
  dplyr::as_tibble()

usethis::use_data(podci_ped_prnt, overwrite = TRUE)
