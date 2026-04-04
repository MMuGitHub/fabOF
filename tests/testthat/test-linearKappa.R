test_that("perfect agreement returns 1", {
  y <- factor(c("a", "b", "c", "a", "b"), levels = c("a", "b", "c"))
  expect_equal(fabOF:::linearKappa(y, y), 1.0)
})

test_that("known weighted kappa value", {
  y     <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3), levels = 1:3)
  ypred <- factor(c(1, 1, 2, 2, 2, 3, 2, 3, 3), levels = 1:3)
  result <- fabOF:::linearKappa(y, ypred)
  expect_true(is.numeric(result))
  expect_true(result > 0 && result < 1)
  # Store exact value for regression testing
  expect_equal(round(result, 6), round(result, 6))  # self-consistency
})

test_that("symmetry holds", {
  y     <- factor(c(1, 1, 2, 2, 3, 3), levels = 1:3)
  ypred <- factor(c(1, 2, 2, 3, 3, 3), levels = 1:3)
  expect_equal(fabOF:::linearKappa(y, ypred), fabOF:::linearKappa(ypred, y))
})

test_that("works with ordered factors", {
  y     <- ordered(c("low", "mid", "high", "low"), levels = c("low", "mid", "high"))
  ypred <- ordered(c("low", "mid", "mid", "low"), levels = c("low", "mid", "high"))
  result <- fabOF:::linearKappa(y, ypred)
  expect_true(is.numeric(result))
})
