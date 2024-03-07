test_that("raw_plot works on sleeping_pills example", {

  rp <- raw_plot(.df = sleeping_pills, .out = "sever_compl", .time = "day",
                 .cond = "treatment", cond_levels = c("C", "E"),
                 cond_labels = c("Control", "Experimental"))
  expect_s3_class(rp, "gg")
  expect_s3_class(rp, "ggplot")
})
