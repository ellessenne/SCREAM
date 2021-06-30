#' Simulate longitudinal eGFR data
#'
#' @param n Number of observations.
#' @param id Name of the ID variable that will be used throughout.
#' @param fixed_intercept Fixed intercept.
#' @param fixed_slope Fixed slope.
#' @param B Variance-covariance matrix of the random effects.
#' @param sigma Standard deviation of the residual error term.
#' @param lambda Parameter \eqn{lambda} of the baseline hazard for the exponential survival model.
#' @param size Parameter `'size'` for the negative binomial distribution; see [stats::dnbinom()].
#' @param prob Parameter `'prob'` for the negative binomial distribution; see [stats::dnbinom()].
#' @param origin If `origin = NULL` (the default), observation times are relative to a time zero. Otherwise, if a date is passed to `origin`, all dates are re-calculated relative to it.
#'
#' @return A data frame with simulate data for `n` subjects.
#' @export
#'
#' @examples
#'
#' set.seed(42)
#' df <- simulate_long_egfr(n = 3)
#' df
#'
#' set.seed(42)
#' df <- simulate_long_egfr(n = 3, origin = as.Date("2006-01-01"))
#' df
simulate_long_egfr <- function(n, id = "lopnr", fixed_intercept = 100, fixed_slope = -1, B = matrix(data = c(100, 10, 10, 5), nrow = 2, ncol = 2), sigma = 10, lambda = 1, size = 1, prob = 0.1, origin = NULL) {
  n <- 3
  id <- "lopnr"
  fixed_intercept <- 100
  fixed_slope <- -1
  B <- matrix(data = c(100, 10, 10, 5), nrow = 2, ncol = 2)
  sigma <- 10
  lambda <- 1
  size <- 1
  prob <- 0.1
  origin <- as.Date("2006-01-01")

  # Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'n', 'fixed_intercept', 'fixed_slope', 'sigma', 'lambda', 'size', 'prob' must be numeric
  checkmate::assert_number(x = n, add = arg_checks)
  checkmate::assert_number(x = fixed_intercept, add = arg_checks)
  checkmate::assert_number(x = fixed_slope, add = arg_checks)
  checkmate::assert_number(x = sigma, add = arg_checks)
  checkmate::assert_number(x = lambda, add = arg_checks)
  checkmate::assert_number(x = size, add = arg_checks)
  checkmate::assert_number(x = prob, add = arg_checks)
  # 'id' must be a string
  checkmate::assert_string(x = id, add = arg_checks)
  # 'B' must be a 2x2 matrix
  checkmate::assert_matrix(x = B, add = arg_checks)
  checkmate::assert_true(x = nrow(B) == 2, add = arg_checks)
  checkmate::assert_true(x = ncol(B) == 2, add = arg_checks)
  # 'origin' must be a date or NULL
  checkmate::assert_date(x = origin, null.ok = TRUE, add = arg_checks)
  # Report
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  # Subject-specific number of observations, based on a negative binomial distribution
  nobs <- stats::rnbinom(n = n, size = size, prob = prob) + 1

  # For each subject, simulate a 'nobs' observations from an exponential survival model
  time <- lapply(X = seq(n), FUN = function(i) {
    u <- stats::runif(n = nobs[i], min = 0, max = 1)
    t <- -log(u) / lambda
    t <- cumsum(t)
    out <- data.frame(time = t, n = seq(nobs[i]), N = nobs[i])
    out[[id]] <- i
    return(out)
  })
  time <- do.call(rbind.data.frame, time)

  # Simulate random effects and create subject-specific intercept, slope
  random_effects <- mvtnorm::rmvnorm(n = n, mean = c(0, 0), sigma = B)
  random_effects <- as.data.frame(random_effects)
  names(random_effects) <- c("intercept", "slope")
  random_effects[[id]] <- seq(n)
  random_effects["intercept"] <- random_effects["intercept"] + fixed_intercept
  random_effects["slope"] <- random_effects["slope"] + fixed_slope

  # Merge all of the above
  out <- merge(time, random_effects, by = id)

  # Simulate eGFR based everything else been done so far
  out$egfr <- out$intercept + out$slope * out$time + stats::rnorm(n = nrow(out), sd = sigma)

  # If origin is supplied, provide dates
  if (!is.null(origin)) {
    out$time <- (out$time * 365.242) + origin
  }

  # Return
  return(out)
}
