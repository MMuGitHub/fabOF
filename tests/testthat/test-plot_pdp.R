test_that("continuous x_var returns ggplot (mixfabOF)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  set.seed(123)
  p <- plot_pdp(
    data = test_data, model = test_mixfabof, response = "y",
    x_var = "x1", x_var_title = "X1", nmax = 50, nIce = 10,
    borders = "none"
  )
  expect_s3_class(p, "ggplot")
})

test_that("categorical x_var returns ggplot (mixfabOF)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggridges")
  set.seed(123)
  p <- plot_pdp(
    data = test_data, model = test_mixfabof, response = "y",
    x_var = "x3", x_var_title = "X3", nmax = 50, nIce = 10,
    borders = "none"
  )
  expect_s3_class(p, "ggplot")
})

test_that("verbose=TRUE returns list with correct elements", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  set.seed(123)
  result <- plot_pdp(
    data = test_data, model = test_mixfabof, response = "y",
    x_var = "x1", x_var_title = "X1", nmax = 50, nIce = 10,
    verbose = TRUE, borders = "none"
  )
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("data" %in% names(result))
  expect_true("variable_type" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$variable_type, "continuous")
})

test_that("borders='auto' works with model borders", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  set.seed(123)
  p <- plot_pdp(
    data = test_data, model = test_mixfabof, response = "y",
    x_var = "x1", x_var_title = "X1", nmax = 50, nIce = 10,
    borders = "auto"
  )
  expect_s3_class(p, "ggplot")
})

test_that("borders='none' works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  set.seed(123)
  p <- plot_pdp(
    data = test_data, model = test_mixfabof, response = "y",
    x_var = "x1", x_var_title = "X1", nmax = 50, nIce = 10,
    borders = "none"
  )
  expect_s3_class(p, "ggplot")
})

test_that("conditional coloring with categorical variable", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  set.seed(123)
  p <- plot_pdp(
    data = test_data, model = test_mixfabof, response = "y",
    x_var = "x1", x_var_title = "X1", nmax = 50, nIce = 10,
    borders = "none",
    cond_color_var = "x3",
    cond_color_levels = c("A", "B"),
    cond_color_palette = c("red", "blue")
  )
  expect_s3_class(p, "ggplot")
})

test_that("conditional coloring with continuous variable", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  set.seed(123)
  p <- plot_pdp(
    data = test_data, model = test_mixfabof, response = "y",
    x_var = "x1", x_var_title = "X1", nmax = 50, nIce = 10,
    borders = "none",
    cond_color_var = "x2",
    cond_color_levels = list("Low" = c(-3, 0), "High" = c(0, 3)),
    cond_color_palette = c("red", "blue")
  )
  expect_s3_class(p, "ggplot")
})

test_that("errors on invalid model", {
  expect_error(
    plot_pdp(data = test_data, model = list(), response = "y",
             x_var = "x1", x_var_title = "X1"),
    "mixfabOF.*fabOF"
  )
})

test_that("errors on missing x_var", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  expect_error(
    plot_pdp(data = test_data, model = test_mixfabof, response = "y",
             x_var = "nonexistent", x_var_title = "X"),
    "not found"
  )
})

test_that("verbose data dimensions are reproducible", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  set.seed(123)
  r1 <- plot_pdp(data = test_data, model = test_mixfabof, response = "y",
                 x_var = "x1", x_var_title = "X1", nmax = 50, nIce = 10,
                 verbose = TRUE, borders = "none")
  set.seed(123)
  r2 <- plot_pdp(data = test_data, model = test_mixfabof, response = "y",
                 x_var = "x1", x_var_title = "X1", nmax = 50, nIce = 10,
                 verbose = TRUE, borders = "none")
  expect_equal(nrow(r1$data$ice_data), nrow(r2$data$ice_data))
  expect_equal(nrow(r1$data$pdp_data), nrow(r2$data$pdp_data))
  expect_equal(r1$data$pdp_data$fit, r2$data$pdp_data$fit)
})
