# Internal function used in fabOF for generating a list of parameter settings for ranger.
makeRangerControl <- function(ranger.control = NULL) {

  out <- as.list(formals(ranger))

  if(is.null(ranger.control)) {
    out$formula <- NULL
    out$data <- NULL
    out["..."] <- NULL
    out$sample.fraction <- ifelse(out$replace, 1, 0.632)
    return(out)
  }

  if(!is.list(ranger.control)) {
    stop("Error: Arguments to ranger function must be provided as list.")
  }

  conflicting.params <- names(ranger.control)[
    !names(ranger.control) %in% names(out)]

  if(length(conflicting.params) > 0) {
    warning(paste0("Warning: The following parameter(s) specified in ranger.control is/are no valid argument(s) of ranger and will be ignored: ", paste(conflicting.params, collapse = ", "), "."))
  }

  valid.params <- names(ranger.control)[!names(ranger.control) %in% conflicting.params]

  if(length(valid.params) == 0) {
    return(out)
  }

  for(param in valid.params) {
    out[[param]] <- ranger.control[[param]]
  }

  if(!is.null(out$splitrule) &&
     !out$splitrule %in% c("variance", "extratrees", "maxstat", "beta")) {
    stop("Error: Invalid splitrule choice for (mix)fabOF. Select one of: variance, extratrees or maxstat. Default: variance.")
  }

  if(out$quantreg) {
    warning("Warning: The 'quantreg' argument of ranger is not supported by (mix)fabOF. Instead, quantreg = FALSE will be used.")
    out$quantreg <- FALSE
  }

  if(out$probability) {
    warning("Warning: The 'probability' argument of ranger is not supported by (mix)fabOF. Instead, probability = FALSE will be used.")
    out$probability <- FALSE
  }

  if(!is.null(out$dependent.variable.name)) {
    warning("Warning: The 'dependent.variable.name' argument of ranger is not supported by (mix)fabOF. Instead, dependent.variable.name = NULL will be used. Please use the 'formula' argument of fabOF to specify the model.")
    out["dependent.variable.name"] <- list(NULL)
  }

  if(!is.null(out$status.variable.name)) {
    warning("Warning: The 'status.variable.name' argument of ranger is not supported by (mix)fabOF. Instead, status.variable.name = NULL will be used. Please use the 'formula' argument of fabOF to specify the model.")
    out["status.variable.name"] <- list(NULL)
  }

  if(!is.null(out$classification)) {
    warning("Warning: The 'classification' argument of ranger is not supported by (mix)fabOF. Instead, classification = NULL will be used.")
    out["classification"] <- list(NULL)
  }

  if(!is.null(out$x)) {
    warning("Warning: The 'x' argument of ranger is not supported by (mix)fabOF. Instead, x = NULL will be used. Please use the 'data' argument of fabOF to pass the training data.")
    out["x"] = list(NULL)
  }

  if(!is.null(out$y)) {
    warning("Warning: The 'y' argument of ranger is not supported by (mix)fabOF. Instead, y = NULL will be used. Please use the 'data' argument of fabOF to pass the training data.")
    out["y"] = list(NULL)
  }

  if(!"sample.fraction" %in% valid.params) {
    out$sample.fraction <- ifelse(out$replace, 1, 0.632)
  }

  out$formula <- NULL
  out$data <- NULL
  out["..."] <- NULL

  return(out)
}
