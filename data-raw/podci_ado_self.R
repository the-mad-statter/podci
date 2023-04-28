podci_ado_self <- "data-raw/podci_ado_self.csv" %>%
  read.csv() %>%
  dplyr::as_tibble()

usethis::use_data(podci_ado_self, overwrite = TRUE)
