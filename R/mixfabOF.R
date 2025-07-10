#' Fits a Mixed-effects Frequency-Adjusted Borders Ordinal Forest (mixfabOF) as described in Buczak (2024a).
#' Designed for ordinal prediction in hierarchical data settings, mixfabOF assigns numeric scores to the ordinal response categories and
#' iterates between estimating fixed and random effects. See 'Details' section below for more information on mixfabOF.
#'
#' Extending Frequency-Adjusted Borders Ordinal Forest (\link[fabOF]{fabOF}; Buczak, 2024b) to hierarchical data, mixfabOF combines
#' the logic of fabOF with the iterative estimation procedure proposed in Hajjem et al. (2014) that extends regression random forest to
#' hierarchical data settings. To this end, mixfabOF assigns \code{scores} to the ordinal response categories and iterates between
#' estimating the fixed effects structure using random forest and the random effects structure using a linear mixed model.
#'
#' The mixfabOF model is specified via the \code{formula} and \code{random} where \code{formula} specifies the fixed effects structure
#' (e.g., y ~ x1 + x2 + ...) and \code{random} specifies the random effects structure based on a grouping variable
#' (e.g., ~ 1|id for a random intercept model with grouping variable 'id'). The estimation procedures runs until a minimum relative
#' change in log-likelihood is not achieved (specified via \code{delta}) or the maximum number of iterations (\code{max.iter}) is exhausted.
#'
#' By setting \code{importance = TRUE}, permutation variable importance values can be computed. For the variable importance, predictive
#' performance is assessed using weighted Cohen's Kappa with linear weights (see Buczak, 2024a and Buczak, 2024b for more details).
#'
#' For fitting the regression RF, the \code{ranger} package (Wright & Ziegler, 2017) is used. Arguments
#' for \code{ranger} (e.g., \code{num.trees} or \code{mtry}) can be passed via the \code{ranger.control} argument as a list with named entries
#' corresponding to the respective \code{ranger} parameter. See \code{ranger} documentation for an overview
#' of all parameters and their description. If left unspecified in the \code{ranger.control} argument,
#' all parameters are initialized using their respective default from the \code{ranger} package.
#'
#'
#' @title mixfabOF
#' @param formula Fixed effects formula as object of class \code{formula} or \code{character}.
#' @param data Training data as object of class \code{data.frame} or \code{matrix}.
#' @param random Random effects structure as object of class \code{formula} or \code{character} . See 'Details' for syntax details.
#' @param delta Minimum relative change in log-likelihood for iterative estimation procedure.
#' @param max.iter Maximum number of iterations for iterative estimation procedure.
#' @param scores Numeric scores assigned to ordinal response categories.
#' @param importance Compute permutation variable importance based on weighted Cohen's Kappa with linear weights.
#' @param importance.reps Replications used for computing variable importance. High values increase stability of variable importance results but increase runtime.
#' @param permute.clusterwise Should permutations only occur within the same cluster?
#' @param ranger.control List of arguments to pass to \code{ranger} function (e.g., num.trees, mtry, etc.). See \link[ranger]{ranger} documentation for a comprehensive overview of specifiable parameters.
#' @return Fitted model object of class mixfabOF containing
#'    \item{\code{ranger.fit}}{Forest object trained using numeric scores as target.}
#'    \item{\code{category.borders}}{Numeric category borders determined by frequency-based heuristic.}
#'    \item{\code{categories}}{Response categories of ordinal target variable.}
#'    \item{\code{category.scores}}{Numeric scores assigned to ordinal response categories.}
#'    \item{\code{category.frequencies}}{Frequencies of ordinal response categories in the training data.}
#'    \item{\code{variable.importance}}{Permutation variable importance values.}
#'    \item{\code{target}}{Target variable name.}
#'    \item{\code{call}}{Function call.}
#'    \item{\code{loglik}}{Log-likelihood value in last iteration.}
#' @examples
#' \dontrun{}
#' @author Philip Buczak
#' @references
#' \itemize{
#'   \item Buczak, P. (2024a). Mixed-Effects Frequency-Adjusted Borders Ordinal Forest: A Tree Ensemble Method for Ordinal Prediction with Hierarchical Data. OSF Pre-print. \doi{10.31219/osf.io/ny6we}.
#'   \item Buczak, P. (2024b). fabOF: A Novel Tree Ensemble Method for Ordinal Prediction. OSF Pre-print. \doi{10.31219/osf.io/h8t4p}.
#'   \item Hajjem, A., Bellavance, F. & Larocque, D. (2014). Mixed-effects Random Forest for Clustered Data. JSCS. \doi{10.1080/00949655.2012.741599}
#'   \item Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. J Stat Softw 77:1-17. \doi{10.18637/jss.v077.i01}.
#'   \item Breiman, L. (2001). Random Forests. Mach Learn, 45:5-32. \doi{10.1023/A:1010933404324}.
#'   }
#'
#' @import ranger
#' @export
mixfabOF <- function(formula, data, random, delta = 0.001, max.iter = 100, scores = NULL,
                     importance = FALSE, importance.reps = 100, permute.clusterwise = FALSE, ranger.control = NULL) {

  if(!class(formula) %in% c("formula", "character")) {
    stop("Error: Argument 'formula' must either be of class formula or character.")
  }

  if(is.character(formula)) {
    formula <- as.formula(formula)
  }

  target <- toString(formula[[2]])

  if(!is.factor(data[, target])) {
    stop("Error: Target variable must be a factor variable (where the level order represents the order of the ordinal categories).")
  }
  if(length(target) != 1) {
    stop("Error: Invalid length of target in formula. Only one target variable supported.")
  }

  cats <- levels(data[, target])
  n.cats <- length(cats)
  cat.idx <- sapply(data[, target], function(x) which(x == levels(data[, target])))

  if(n.cats <= 1) {
    stop("Error: Number of categories must be greater than 1.")
  }

  if(is.null(scores)) {
    scores <- 1:n.cats
  } else {
    if(!is.numeric(scores)) {
      stop("Error: Provided scores must be numeric.")
    }
    if(length(scores) != length(levels(data[, target]))) {
      stop("Error: Must provide score for each ordinal response category level.")
    }
    if(is.unsorted(scores)) {
      stop("Error: Provided scores must be in ascending order.")
    }
  }

  if(importance) {
    ranger.control <- c(ranger.control, list(keep.inbag = TRUE))
  }

  ranger.control <- makeRangerControl(ranger.control)
  variable.importance <- NULL

  data.tmp <- data
  data.tmp[, target] <- scores[cat.idx]

  if(is.character(random)) {
    random <- as.formula(random)
  }
  random.string <- attr(terms(random, data = data), "term.labels")
  random.stripped <- gsub(" ", "", random.string)
  random.split <- strsplit(random.stripped, "\\|")[[1]]

  if(length(random.split) == 1) {
    stop("Error: No grouping variable specified in random effect part.")
  }

  grp.var <- random.split[2]

  if(!is.factor(data[, grp.var])) {
    stop("Error: Grouping variable must be a factor variable.")
  }

  id <- data[, grp.var]
  ran.var <- strsplit(random.split[1], "\\+")[[1]]

  if("1" %in% ran.var) {
    Z <- as.matrix(cbind(1, data[, ran.var[ran.var != "1"]]))
  } else {
    Z <- as.matrix(data[, ran.var])
  }

  y.num <- data.tmp[, target]
  n.obs <- length(y.num)
  n.grp <- length(unique(id))
  n.re <- ncol(Z)
  b.hat <- matrix(0, nrow = n.grp, ncol = n.re)
  sigma.sq.hat <- 1
  D.hat <- diag(1, n.re)
  eps.hat <- numeric(n.obs)

  if(!is.numeric(delta) | delta <= 0) {
    stop("Error: delta must be a numeric value greater than 0.")
  }

  if(!is.numeric(max.iter) | max.iter < 1) {
    stop("Error: max.iter must be a positive integer value.")
  }

  iter <- 0
  continue <- TRUE
  ll.old <- -Inf

  while(continue) {
    iter <- iter + 1

    y.star <- numeric(n.obs)
    for(j in 1:n.grp) {
      id.j <- which(id == unique(id)[j])
      y.star[id.j] <- y.num[id.j] - Z[id.j,, drop = FALSE] %*% b.hat[j,]
    }

    data.tmp[, target] <- y.star

    ranger.fit <- do.call(ranger, c(list(formula = formula, data = data.tmp), ranger.control), quote = TRUE)
    ranger.fit$call <- NULL
    f.hat <- ranger.fit$predictions

    D.hat.new <- matrix(0, ncol = n.re, nrow = n.re)
    sigma.sq.hat.new <- 0

    for(j in 1:n.grp) {
      id.j <- which(id == unique(id)[j])
      Z.j <- Z[id.j,, drop = FALSE]
      V.j <- Z.j %*% D.hat %*% t(Z.j) + diag(sigma.sq.hat, length(id.j))
      V.j.inv <- solve(V.j)

      b.hat[j,] <- D.hat %*% t(Z.j) %*% V.j.inv %*% (y.num[id.j] - f.hat[id.j])
      eps.hat[id.j] <- y.num[id.j] - f.hat[id.j] - Z.j %*% b.hat[j,]

      D.hat.new <- D.hat.new + b.hat[j,] %*% t(b.hat[j,]) + D.hat -
        D.hat %*% t(Z.j) %*% V.j.inv %*% Z.j %*% D.hat

      sigma.sq.hat.new <- sigma.sq.hat.new + t(eps.hat[id.j]) %*% eps.hat[id.j] +
        sigma.sq.hat * (length(id.j) - sigma.sq.hat * sum(diag(V.j.inv)))
    }

    D.hat <- D.hat.new/n.grp
    sigma.sq.hat <- as.numeric(sigma.sq.hat.new/n.obs)

    ll.new <- logLik(Z = Z, id = id, b.hat = b.hat, eps.hat = eps.hat,
                     D.hat = D.hat, sigma.sq.hat = sigma.sq.hat)

    if(iter > 1) {
      if((ll.old - ll.new)/ll.old < delta) {
        continue <- FALSE
        conv <- TRUE
      } else if(iter >= max.iter) {
        continue <- FALSE
        conv <- FALSE
        warning("Warning: max.iter reached without convergence.")
      }
    }
    ll.old <- ll.new
  }

  rownames(b.hat) <- unique(id)
  pred <- numeric(n.obs)

  for(j in 1:n.grp) {
    id.j <- which(id == unique(id)[j])
    grp.id <- which(rownames(b.hat) == unique(id)[j])
    pred[id.j] <- f.hat[id.j] + Z[id.j,, drop = FALSE] %*% b.hat[j,]
  }

  cum.prob.cats <- cumsum(table(data[, target])/n.obs)
  cat.borders <- c(-Inf,
                   as.numeric(quantile(pred, probs = c(cum.prob.cats[-n.cats]))),
                   Inf)

  browser()
  
  if(importance) {
    pred.num <- sapply(pred, function(x) max(which(x >= cat.borders[1:length(cats)])))
    pred.cat <- factor(cats[pred.num], levels = cats)
    orig.err <- linearKappa(data[, target], pred.cat)
    vars <- attr(terms(formula, data = data), "term.labels")
    perm.err <- matrix(0, nrow = length(vars), ncol = importance.reps)
    oob.mat <- do.call(cbind, ranger.fit$inbag.counts) < 1

    for(v in 1:length(vars)) {
      var.orig <- data.tmp[, vars[v]]
      print(v)

      for(r in 1:importance.reps) {
        print(r)
        if(permute.clusterwise) {
          for(j in 1:n.grp) {
            id.j <- which(id == unique(id)[j])
            data.tmp[id.j, vars[v]] <- sample(var.orig[id.j])
          }
        } else {
          var.perm <- sample(var.orig)
          data.tmp[, vars[v]] <- var.perm
        }

        pred.mat <- predict(ranger.fit, data.tmp, predict.all = TRUE)$predictions
        pred.oob <- rowSums(pred.mat * oob.mat) / rowSums(oob.mat)

        for(j in 1:n.grp) {
          id.j <- which(id == unique(id)[j])
          grp.id <- which(rownames(b.hat) == unique(id)[j])
          pred.oob[id.j] <- pred.oob[id.j] + Z[id.j,, drop = FALSE] %*% b.hat[j,]
        }

        pred.oob.num <- sapply(pred.oob, function(x) max(which(x >= cat.borders[1:length(cats)])))
        pred.oob.cat <- factor(cats[pred.oob.num], levels = cats)

        perm.err[v, r] <- linearKappa(data[, target], pred.oob.cat)
      }

      data.tmp[, vars[v]] <- var.orig
    }

    variable.importance <- orig.err - rowMeans(perm.err)
    names(variable.importance) <- vars
  }

  call <- sys.call()

  result <- list(ranger.fit = ranger.fit, random.effects = b.hat,
                 random.effect.var = D.hat, residual.var = sigma.sq.hat,
                 categories = cats, category.borders = cat.borders,
                 variable.importance = variable.importance,
                 loglik = ll.new, call = call, iter = iter, conv = conv,
                 formula = formula,
                 random.formula = random)

  class(result) <- "mixfabOF"

  return(result)

}
