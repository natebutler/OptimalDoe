## code to prepare `DATASET` dataset goes here

set.seed(3657)

optimal_A_14_3 <- generate_designs(14, 3, 1, "A", 1000)
result_A_14_3 <- lapply(head(optimal_A_14_3, 4), `[[`, 1)

optimal_D_14_3 <- generate_designs(14, 3, 1, "D", 1000)
result_D_14_3 <- lapply(head(optimal_D_14_3, 4), `[[`, 1)

usethis::use_data(optimal_D_14_3, overwrite = TRUE)
usethis::use_data(optimal_A_14_3, overwrite = TRUE)
