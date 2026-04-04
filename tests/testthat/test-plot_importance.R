test_that("returns ggplot for fabOF model", {
  skip_if_not_installed("ggplot2")
  p <- plot_importance(model = test_fabof)
  expect_s3_class(p, "ggplot")
})

test_that("returns ggplot for mixfabOF model", {
  skip_if_not_installed("ggplot2")
  p <- plot_importance(model = test_mixfabof)
  expect_s3_class(p, "ggplot")
})

test_that("top_n limits variables shown", {
  skip_if_not_installed("ggplot2")
  p <- plot_importance(model = test_fabof, top_n = 2)
  expect_s3_class(p, "ggplot")
  # Check that the data in the plot has only 2 rows
  expect_equal(nrow(p$data), 2)
})

test_that("errors on non-model input", {
  expect_error(plot_importance(model = list()), "mixfabOF.*fabOF")
})

test_that("errors when no importance available", {
  expect_error(plot_importance(model = test_fabof_noimp), "importance")
})

test_that("sort_ascending works", {
  skip_if_not_installed("ggplot2")
  p <- plot_importance(model = test_fabof, sort_ascending = TRUE)
  expect_s3_class(p, "ggplot")
})
