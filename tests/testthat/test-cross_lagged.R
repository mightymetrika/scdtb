test_that("multiplication works", {

  rw <- reversal_withdrawal
  rw$synth <- sapply(rw$time, function(x){
    stats::rpois(1, x)
  })

  rw <- as.data.frame(rw)


  cl <- cross_lagged(.df = rw, .x = "extbehavs", .y = "synth",
                     lag.max = 5, type = "correlation", plot = FALSE)


  expect_equal(length(cl), 6)
})
