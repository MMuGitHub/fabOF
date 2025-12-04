#' Predicts ordinal response categories for new observations using a mixfabOF model fit. For unknown clusters, prediction is performed based on the fixed effects only.
#'
#' @title Predict mixfabOF
#' @param object Fitted mixfabOF model object.
#' @param newdata Dataset containing new observations to be predicted.
#' @param type Character string specifying the type of prediction. Either "response" (default) for ordinal category predictions or "latent" for latent variable predictions.
#' @return Predicted ordinal response category labels (if type = "response") or numeric latent variable predictions (if type = "latent").
#' @author Philip Buczak
#' @export
predict.mixfabOF <-
  function(object, newdata, type = c("response", "latent"), ...) {
    if (!inherits(object, "mixfabOF")) {
      stop("Error: Object must be of class mixfabOF.")
    }
    if (is.null(newdata)) {
      stop("Error: Must provide data for prediction.")
    }

    ranger.pred <- predict(object$ranger.fit, data = newdata)$predictions

    random.string <- attr(terms(object$random.formula), "term.labels")
    random.stripped <- gsub(" ", "", random.string)
    random.split <- strsplit(random.stripped, "\\|")[[1]]
    grp.var <- random.split[2]

    if (is.null(newdata[[grp.var]])) {
      warning(
        "Random effect column is missing in newdata. Predictions are based on RE = 0"
      )
      #newdata[[grp.var]] <- rep(NA, nrow(newdata))
      pred <- ranger.pred
    } else {
      
      #PROPOSED FIX to OG fabOF: Use [[ to ensure we get a vector (works with both data.frame and tibble)
      id <- newdata[[grp.var]]
      ran.var <- strsplit(random.split[1], "\\+")[[1]]
      
      if ("1" %in% ran.var) {
        Z <- as.matrix(cbind(1, newdata[, ran.var[ran.var != "1"], drop = FALSE]))
      } else {
        Z <- as.matrix(newdata[, ran.var, drop = FALSE])
      }
      grps <- unique(id)
      n.grps <- length(grps)
      pred <- numeric(nrow(newdata))

      for (j in 1:n.grps) {
        id.j <- which(id == grps[j])

        if (grps[j] %in% rownames(object$random.effects)) {
          grp.id <- which(rownames(object$random.effects) == grps[j])
          pred[id.j] <- ranger.pred[id.j] +
            Z[id.j, , drop = FALSE] %*%
              object$random.effects[grp.id, ]
        } else {
          pred[id.j] <- ranger.pred[id.j]
        }
      }
    }

    # Inspired by Roman Hornung's ordinalForest R package

    pred.num <- sapply(pred, function(x) {
      max(which(x >= object$category.borders[1:length(object$categories)]))
    })
    pred.cat <- factor(object$categories[pred.num], levels = object$categories)

    if (type == "latent") {
      return(pred)
    } else {
      return(pred.cat)
    }
  }
