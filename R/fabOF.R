#' Fits a frequency adjusted borders Ordinal Forest (fabOF) as described in Buczak (2024).
#' Designed for ordinal prediction, fabOF assigns numeric scores to the ordinal response categories and fits a regression
#' random forest (RF; Breiman, 2001) on these numeric scores (similar to Ordinal Forest; Hornung, 2020). For fitting the RF model, the \code{ranger}
#' package (Wright & Ziegler, 2017) is used. See 'Details' section below for more information on fabOF.
#'
#' Similar to Ordinal Forest (OF; Hornung, 2020), fabOF assumes that the ordinal response results
#' from an underlying latent numeric variable which can not be observed directly, but can be approximated.
#' To this end, all ordinal response categories are characterized by a numeric category interval and a
#' representative numeric category score. These scores are assigned to the ordinal response categories
#' and used to train a regression RF. For new observations, the numeric RF predictions are transformed
#' into ordinal response categories using the category interval borders.
#'
#' The numeric scores to be assigned to the ordinal response categories can be assigned via the \code{scores} argument.
#' By default, the scores 1, 2, ..., k are used for k categories. The category borders are derived by a
#' heuristic based on the cumulative relative frequencies of the ordinal response categories in the training data.
#' Having trained the regression RF, numeric out-of-bag (OOB) predictions for all training observations are generated.
#' For the j-th category, the lower category border is then chosen as the quantile of the OOB predictions
#' with the cumulative relative frequency up to category j-1 serving as the probability. The upper category border
#' is analogously chosen as the quantile of the OOB predictions
#' with the cumulative relative frequency up to category j serving as the probability. For the lower and upper
#' bound of all category borders, the minimum and maximum numeric score are chosen, respectively. For
#' a more detailed description, see Buczak (2024).
#'
#' For fitting the regression RF, the \code{ranger} package (Wright & Ziegler, 2017) is used. Arguments
#' for \code{ranger} (e.g., \code{num.trees} or \code{mtry}) can be passed via the \code{ranger.control} argument as a list with named entries
#' corresponding to the respective \code{ranger} parameter. See \code{ranger} documentation for an overview
#' of all parameters and their description. If left unspecified in the \code{ranger.control} argument,
#' all parameters are initialized using their respective default from the \code{ranger} package.
#'
#'
#' @title fabOF
#' @param formula Model formula as object of class \code{formula} or \code{character}.
#' @param data Training data as object of class \code{data.frame} or \code{matrix}.
#' @param scores Numeric scores assigned to ordinal response categories.
#' @param importance Compute permutation variable importance based on weighted Cohen's Kappa with linear weights (see Buczak, 2024).
#' @param importance.reps Replications used for computing variable importance. High values increase stability of variable importance results but increase runtime.
#' @param ranger.control List of arguments to pass to \code{ranger} function (e.g., num.trees, mtry, etc.). See \link[ranger]{ranger} documentation for a comprehensive overview of specifiable parameters.
#' @return Fitted model object of class fabOF containing
#'    \item{\code{ranger.fit}}{Forest object trained using numeric scores as target.}
#'    \item{\code{category.borders}}{Numeric category borders determined by frequency-based heuristic.}
#'    \item{\code{categories}}{Response categories of ordinal target variable.}
#'    \item{\code{category.scores}}{Numeric scores assigned to ordinal response categories.}
#'    \item{\code{category.frequencies}}{Frequencies of ordinal response categories in the training data.}
#'    \item{\code{variable.importance}}{Permutation variable importance values.}
#'    \item{\code{target}}{Target variable name.}
#'    \item{\code{call}}{Function call.}
#' @examples
#' \dontrun{
#' # Example from Tutz (2021)
#' library(mlbench)
#' data(BostonHousing)
#' data <- BostonHousing[, names(BostonHousing) != "medv"]
#' data$medv_ord <- 1
#' data$medv_ord[BostonHousing$medv > 15 & BostonHousing$medv <= 19] <- 2
#' data$medv_ord[BostonHousing$medv > 19 & BostonHousing$medv <= 22] <- 3
#' data$medv_ord[BostonHousing$medv > 22 & BostonHousing$medv <= 25] <- 4
#' data$medv_ord[BostonHousing$medv > 25 & BostonHousing$medv <= 32] <- 5
#' data$medv_ord[BostonHousing$medv > 32] <- 6
#' data$medv_ord <- as.factor(data$medv_ord)
#'
#' fabOF.boston <- fabOF(medv_ord ~ . , data = data)
#'
#' fabOF.boston}
#' @author Philip Buczak
#' @references
#' \itemize{
#'   \item Buczak, P. (2024). fabOF: A Novel Tree Ensemble Method for Ordinal Prediction. OSF Pre-print. \doi{10.31219/osf.io/h8t4p}.
#'   \item Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. J Stat Softw 77:1-17. \doi{10.18637/jss.v077.i01}.
#'   \item Breiman, L. (2001). Random Forests. Mach Learn, 45:5-32. \doi{10.1023/A:1010933404324}.
#'   \item Hornung, R. (2020). Ordinal Forests. J Classif. 37: 4-17. \doi{10.1007/s00357-018-9302-x}.
#'   }
#'
#' @import ranger
#' @export
fabOF <- function(
  formula,
  data,
  scores = NULL,
  importance = FALSE,
  importance.reps = 100,
  ranger.control = NULL
) {
  if (!class(formula) %in% c("formula", "character")) {
    stop(
      "Error: Argument 'formula' must either be of class formula or character."
    )
  }

  if (is.character(formula)) {
    formula <- as.formula(formula)
  }

  target <- toString(formula[[2]])

  if (!is.factor(data[, target])) {
    stop(
      "Error: Target variable must be a factor variable (where the level order represents the order of the ordinal categories)."
    )
  }
  if (length(target) != 1) {
    stop(
      "Error: Invalid length of target in formula. Only one target variable supported."
    )
  }

  class.idx <- as.numeric(data[, target])
  cats <- levels(data[, target])
  ncats <- length(cats)

  if (ncats <= 1) {
    stop("Error: Number of categories must be greater than 1.")
  }

  if (is.null(scores)) {
    scores <- 1:ncats
  } else {
    if (!is.numeric(scores)) {
      stop("Error: Provided scores must be numeric.")
    }
    if (length(scores) != length(levels(data[, target]))) {
      stop(
        "Error: Must provide score for each ordinal response category level."
      )
    }
    if (is.unsorted(scores)) {
      stop("Error: Provided scores must be in ascending order.")
    }
  }

  data.tmp <- data
  data.tmp[, target] <- scores[class.idx]

  if (importance) {
    ranger.control <- c(ranger.control, list(keep.inbag = TRUE))
  }

  ranger.control <- makeRangerControl(ranger.control)
  variable.importance <- NULL

  ranger.fit <- do.call(
    ranger::ranger,
    c(list(formula = formula, data = data.tmp), ranger.control),
    quote = TRUE
  )
  ranger.fit$call <- NULL

  pred <- ranger.fit$predictions
  cat.freqs <- table(data[, target])
  cat.cumsum <- cumsum(cat.freqs) / nrow(data)
  oob.pred.quantiles <- as.numeric(quantile(
    pred,
    probs = as.numeric(cat.cumsum)[-ncats]
  ))
  cat.borders <- c(scores[1], oob.pred.quantiles, scores[ncats])

  if (importance) {
    pred.num <- sapply(pred, function(x) {
      max(which(x >= cat.borders[1:length(cats)]))
    })
    pred.cat <- factor(cats[pred.num], levels = cats)
    orig.err <- linearKappa(data[, target], pred.cat)
    vars <- attr(terms(formula, data = data), "term.labels")
    perm.err <- matrix(0, nrow = length(vars), ncol = importance.reps)
    oob.mat <- do.call(cbind, ranger.fit$inbag.counts) < 1

    for (i in 1:length(vars)) {
      var.orig <- data.tmp[, vars[i]]

      for (j in 1:importance.reps) {
        var.perm <- sample(var.orig)
        data.tmp[, vars[i]] <- var.perm

        pred.mat <- predict(
          ranger.fit,
          data.tmp,
          predict.all = TRUE
        )$predictions
        pred.oob <- rowSums(pred.mat * oob.mat) / rowSums(oob.mat)
        pred.oob.num <- sapply(pred.oob, function(x) {
          max(which(x >= cat.borders[1:length(cats)]))
        })
        pred.oob.cat <- factor(cats[pred.oob.num], levels = cats)

        perm.err[i, j] <- linearKappa(data[, target], pred.oob.cat)
      }

      data.tmp[, vars[i]] <- var.orig
    }

    variable.importance <- orig.err - rowMeans(perm.err)
    names(variable.importance) <- vars
  }

  result <- list(
    ranger.fit = ranger.fit,
    category.borders = cat.borders,
    categories = cats,
    category.scores = scores,
    category.frequencies = cat.freqs,
    variable.importance = variable.importance,
    target = target,
    call = sys.call()
  )
  class(result) <- "fabOF"

  return(result)
}
