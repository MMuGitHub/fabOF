# helper-test-data.R
# Shared test fixtures auto-sourced by testthat before tests.

# --- Synthetic data ---
set.seed(123)
n <- 200
test_data <- data.frame(
  y  = ordered(sample(c("low", "mid", "high"), n, replace = TRUE),
               levels = c("low", "mid", "high")),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
  group = factor(paste0("G", sample(1:5, n, replace = TRUE)))
)

# --- fabOF model (with importance) ---
set.seed(123)
test_fabof <- fabOF(
  y ~ x1 + x2 + x3,
  data = test_data,
  importance = TRUE,
  importance.reps = 5,
  ranger.control = list(num.trees = 50, seed = 123)
)

# --- fabOF model (without importance, for error testing) ---
set.seed(123)
test_fabof_noimp <- fabOF(
  y ~ x1 + x2 + x3,
  data = test_data,
  importance = FALSE,
  ranger.control = list(num.trees = 50, seed = 123)
)

# --- mixfabOF model ---
set.seed(123)
test_mixfabof <- mixfabOF(
  formula = y ~ x1 + x2 + x3,
  data = test_data,
  random = ~ (1 | group),
  max.iter = 20,
  importance = TRUE,
  importance.reps = 5,
  ranger.control = list(num.trees = 50, seed = 123)
)
