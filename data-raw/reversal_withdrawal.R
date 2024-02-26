## code to prepare `reversal_withdrawal` dataset goes here
reversal_withdrawal <- data.frame(phase = c(rep("baseline1", 6),
                                            rep("teratment1", 5),
                                            rep("baseline2", 5),
                                            rep("teratment2", 5)),
                                  time = 1:21,
                                  extbehavs = c(15, 10, 14, 17, 13, 12,
                                                2, 1, 1, 0, 0,
                                                9, 9, 11, 15, 20,
                                                1, 0, 4, 0, 1))

usethis::use_data(reversal_withdrawal, overwrite = TRUE)
