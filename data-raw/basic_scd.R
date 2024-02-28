## code to prepare `basic_scd` dataset goes here
basic_scd <- data.frame(phase = c(rep("baseline", 5),
                                  rep("treatment", 5)),
                        time = 1:10,
                        socbehavs = c(3, 1, 0, 1, 3,
                                      16, 11, 13, 15, 10))

usethis::use_data(basic_scd, overwrite = TRUE)
