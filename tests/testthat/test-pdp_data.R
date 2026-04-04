test_that("single continuous variable produces correct grid", {
  set.seed(123)
  d <- test_data[1:50, ]
  result <- fabOF:::pdp_data(d, var = "x1", gridsize = 10)
  expect_equal(nrow(result), nrow(d) * 10)
  expect_true(".id" %in% names(result))
  expect_true("x1" %in% names(result))
})

test_that("single categorical variable uses all levels", {
  set.seed(123)
  d <- test_data[1:50, ]
  result <- fabOF:::pdp_data(d, var = "x3", gridsize = 10)
  expect_equal(nrow(result), nrow(d) * nlevels(d$x3))
  expect_true(is.factor(result$x3))
  expect_equal(levels(result$x3), levels(d$x3))
})

test_that("two continuous variables produce full grid", {
  set.seed(123)
  d <- test_data[1:30, ]
  result <- fabOF:::pdp_data(d, var = c("x1", "x2"), gridsize = 5)
  expect_equal(nrow(result), nrow(d) * 5 * 5)
})

test_that("convexHull=TRUE is subset of convexHull=FALSE", {
  set.seed(123)
  d <- test_data[1:30, ]
  full <- fabOF:::pdp_data(d, var = c("x1", "x2"), gridsize = 5, convexHull = FALSE)
  hull <- fabOF:::pdp_data(d, var = c("x1", "x2"), gridsize = 5, convexHull = TRUE)
  expect_true(nrow(hull) <= nrow(full))
  expect_true(nrow(hull) > 0)
})

test_that(".id column present and correct range", {
  d <- test_data[1:20, ]
  result <- fabOF:::pdp_data(d, var = "x1", gridsize = 5)
  expect_true(all(result$.id >= 1 & result$.id <= nrow(d)))
})
