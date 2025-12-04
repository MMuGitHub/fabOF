#' Predicts ordinal response categories for new observations using a fabOF model fit.
#'
#' @title Predict fabOF
#' @param object Fitted fabOF model object.
#' @param newdata Dataset containing new observations to be predicted.
#' @param type Character string specifying the type of prediction. Either "response" (default) for ordinal category predictions or "latent" for latent variable predictions.
#' @return Predicted ordinal response category labels (if type = "response") or numeric latent variable predictions (if type = "latent").
#' @author Philip Buczak
#' @export
predict.fabOF <- function(object, newdata, type = c("response", "latent"), ...) {
  if (!inherits(object, "fabOF")) {
    stop("Error: Object class must be fabOF.")
  }
  if (is.null(newdata)) {
    stop("Error: Must provide data for prediction.")
  }

  cats <- object$categories
  pred <- predict(object$ranger.fit, newdata)$predictions

  if (type == "latent") {
    return(pred)
  } else {
    # Inspired by Roman Hornung's ordinalForest R package
    pred.num <- sapply(pred, function(x) {
      max(which(x >= object$category.borders[1:length(cats)]))
    })
    pred.cat <- factor(cats[pred.num], levels = cats)
    return(pred.cat)
  }
}
