#' Interpolate kidney function via a linear mixed effects model
#'
#' @param fit A mixed model fitted using [lme4::lmer()].
#' @param egfr Interpolation identifies the date at which `egfr` is reached.
#'        Can be a vector or a single value.
#' @param id Name of the ID variable used to identify each study subject.
#' @param origin Origin of the time scale.
#'        Can be a vector of dates or a single value.
#' @param time_scale Time scale used for the calculations; can be either `"years"` or `"days"`.
#'
#' @return A data frame with a row per subject and a column per `egfr` value.
#' @export
#'
#' @examples
interpolate_date <- function(fit, egfr, id, origin, time_scale) {
  # Match time_scale
  time_scale <- match.arg(arg = time_scale, choices = c("years", "days"), several.ok = FALSE)
  # Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'fit' must inherit from 'lmerMod'
  checkmate::assert_class(x = fit, classes = "lmerMod", add = arg_checks)
  # 'egfr' must be a numeric vector
  checkmate::assert_numeric(x = egfr, add = arg_checks)
  # 'id', 'time_scale' must be a string
  checkmate::assert_string(x = id, add = arg_checks)
  checkmate::assert_string(x = time_scale, add = arg_checks)
  # 'origin' must inherit from 'Date'
  checkmate::assert_class(x = origin, classes = "Date", add = arg_checks)
  # Report
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  # Extract subject-specific intercept and slope
  ssl <- stats::coef(fit)[[id]]
  intercept <- ssl[, "(Intercept)"]
  slope <- ssl[, which(names(ssl) != "(Intercept)")]

  # Identify date at which a given eGFR is reached, based on the model
  out <- lapply(X = seq_along(egfr), FUN = function(i) {
    t <- (egfr[i] - intercept) / slope
    if (time_scale == "years") {
      t <- t * 365.242
    }
    t <- t + origin
  })
  names(out) <- paste0("time_", egfr)
  out <- do.call(cbind.data.frame, out)

  # Return
  return(out)
}
