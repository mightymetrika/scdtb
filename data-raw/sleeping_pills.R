## code to prepare `sleeping_pills` dataset goes here
sleeping_pills <- data.frame(day = 1:14,
                             treatment = c("E", "C", "E", "C", "C",
                                           "E", "C", "E", "E", "C",
                                           "C", "E", "E", "C"),
                             sever_compl = c(6, 5, 7, 4, 5,
                                             6, 5, 7, 4, 5,
                                             4, 6, 7, 6))

usethis::use_data(sleeping_pills, overwrite = TRUE)
