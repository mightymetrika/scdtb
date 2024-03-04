test_that("randomization_test works with num_permutations", {
  res <- randomization_test(sleeping_pills, .out = "sever_compl", .cond = "treatment",
                            .time = "day", num_permutations = 1000, cond_levels = c("C", "E"))
  expect_equal(length(res), 6)
})


test_that("randomization_test works with consec set to observed", {
  res <- randomization_test(sleeping_pills, .out = "sever_compl", .cond = "treatment",
                            .time = "day", consec = "observed", cond_levels = c("C", "E"))
  expect_equal(length(res), 6)
})

test_that("randomization_test works with consec set to fixed", {
  res <- randomization_test(sleeping_pills, .out = "sever_compl", .cond = "treatment",
                            .time = "day", consec = "fixed", cond_levels = c("C", "E"),
                            max_consec = 3, min_consec = 1)
  expect_equal(length(res), 6)
})

test_that("randomization_test works with baseic_scd and num_permutations", {
  res <- randomization_test(basic_scd, .out = "socbehavs", .cond = "phase",
                            .time = "time", num_permutations = 1000, cond_levels = c("baseline", "treatment"))

  expect_equal(length(res), 6)
})

test_that("randomization_test works with reversal_withdrawal and num_permutations", {

  # Modify the phase column in base R
  reversal_withdrawal$phase <- ifelse(reversal_withdrawal$phase == "baseline1" | reversal_withdrawal$phase == "baseline2",
                                      "baseline",
                                      "treatment")

  # Call randomization_test function with the modified data frame
  res <- randomization_test(reversal_withdrawal,
                            .out = "extbehavs",
                            .cond = "phase",
                            .time = "time",
                            num_permutations = 1000,
                            cond_levels = c("baseline", "treatment"))

  expect_equal(length(res), 6)
})
