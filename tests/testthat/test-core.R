# test-core.R
# Tests for core fabOF and mixfabOF functionality.

# --- fabOF tests ---

test_that("fabOF category borders are correct structure", {
  expect_length(test_fabof$category.borders, 4)
  expect_true(all(is.numeric(test_fabof$category.borders)))
  expect_true(!is.unsorted(test_fabof$category.borders))
})

test_that("fabOF predictions return correct factor", {
  set.seed(123)
  pred <- predict(test_fabof, test_data, type = "response")
  expect_s3_class(pred, "factor")
  expect_equal(levels(pred), c("low", "mid", "high"))
})

test_that("fabOF latent predictions return numeric", {
  set.seed(123)
  pred_lat <- predict(test_fabof, test_data, type = "latent")
  expect_true(is.numeric(pred_lat))
  expect_length(pred_lat, nrow(test_data))
})

test_that("fabOF variable importance has correct names", {
  expect_equal(names(test_fabof$variable.importance), c("x1", "x2", "x3"))
})

test_that("fabOF is reproducible with seed", {
  set.seed(123)
  fabof2 <- fabOF(
    y ~ x1 + x2 + x3,
    data = test_data,
    ranger.control = list(num.trees = 50, seed = 123)
  )
  expect_equal(test_fabof$category.borders, fabof2$category.borders)
})

# --- mixfabOF tests ---

test_that("mixfabOF converges", {
  expect_true(test_mixfabof$conv)
})

test_that("mixfabOF random effects have correct dimensions", {
  expect_equal(nrow(test_mixfabof$random.effects), 5)
  expect_equal(ncol(test_mixfabof$random.effects), 1)
})

test_that("mixfabOF loglik is finite", {
  expect_true(is.finite(test_mixfabof$loglik))
})

test_that("mixfabOF predictions differ with/without random effects", {
  set.seed(123)
  pred_re <- predict(test_mixfabof, test_data, type = "latent")
  newdata_no_grp <- test_data
  newdata_no_grp$group <- NULL
  pred_no_re <- suppressWarnings(predict(test_mixfabof, newdata_no_grp, type = "latent"))
  expect_false(identical(pred_re, pred_no_re))
})

test_that("mixfabOF is reproducible with seed", {
  set.seed(123)
  mix2 <- mixfabOF(
    y ~ x1 + x2 + x3,
    data = test_data,
    random = ~ (1 | group),
    max.iter = 20,
    ranger.control = list(num.trees = 50, seed = 123)
  )
  expect_equal(test_mixfabof$category.borders, mix2$category.borders)
  expect_equal(test_mixfabof$loglik, mix2$loglik)
})
