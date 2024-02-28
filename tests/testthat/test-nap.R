test_that("nap gives same result as WWC 5.0 Appendix I reversability example", {

  res <- nap(.df = reversal_withdrawal, .y = "extbehavs", .phase = "phase", .time = "time",
             type = "reversability", phases = list("baseline1", "baseline2"),
             improvement = "negative")
  expect_equal(round(res, 3), 0.617)
})

test_that("nap gives same result as WWC 5.0 Appendix I trend example", {
  res <- nap(.df = reversal_withdrawal, .y = "extbehavs", .phase = "phase", .time = "time",
             type = "trend", last_m = 3, phases = list("baseline1"),
             improvement = "negative")
  expect_equal(round(res,2), 0.44)
})
