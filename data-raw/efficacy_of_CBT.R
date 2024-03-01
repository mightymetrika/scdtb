## code to prepare `efficacy_of_CBT` dataset goes here
efficacy_of_CBT <- data.frame(phase = as.factor(c(rep(0,5), rep(1,5))),
                              Anxious = c(17, 14, 13, 13, 7, 10, 8,11, 10, 12),
                              CATS_N = c(2,4,2,4,2, 8,5,8,7,6),
                              time = 1:10)

usethis::use_data(efficacy_of_CBT, overwrite = TRUE)
