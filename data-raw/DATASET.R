## code to prepare `DATASET` dataset goes here

set.seed(3657)

optimal_A_25_4 <- generate_designs(25, 4, 2, "A", 100)
result_A_25_4 <- lapply(head(optimal_A_25_4, 4), `[[`, 1)

optimal_D_25_4 <- generate_designs(25, 4, 2, "D", 100)
result_D_25_4 <- lapply(head(optimal_D_25_4, 4), `[[`, 1)

usethis::use_data(optimal_D_25_4, overwrite = TRUE)
usethis::use_data(optimal_A_25_4, overwrite = TRUE)
