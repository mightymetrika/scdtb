# test_that("multiplication works", {
#   data <- data.frame(phase         = c(rep(0,5), rep(1,5)),
#                      time_in_phase = rep(4:0, 2),
#                      Anxious       = c(17, 14, 13, 13, 7, 10, 8,11, 10, 12),
#                      CATS_N        = c(2,4,2,4,2, 8,5,8,7,6),
#                      time          = 1:10,
#                      participant   = factor(rep("x",10)))
#
#   m1 <- nlme::gls(model       = Anxious ~ time_in_phase + phase + time_in_phase*phase,
#             data        = data,
#             method      = "REML",
#             correlation = nlme::corAR1(, form=~time|participant))
#
#   summary(m1)
#   intervals(m1)
#   expect_equal(2 * 2, 4)
# })
