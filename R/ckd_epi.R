#' CKD-EPI Equation
#'
#' @param creatinine Serum creatinine (in μmol/L);
#' @param age Age;
#' @param female Gender (with females identified by `female = 1`).
#'
#' @return Estimated eGFR, in mL/min/1.73m2.
#'
#' @note Compare e.g. with <https://www.kidney.org/professionals/kdoqi/gfr_calculator>.
#'
#' @export
#'
#' @examples
#' ckd_epi(creatinine = 50, age = 40, female = 0)
#' ckd_epi(creatinine = 50, age = 40, female = 1)
#'
ckd_epi <- function(creatinine, age, female) {
  n_crea <- length(creatinine)
  n_age <- length(age)
  n_female <- length(female)
  if (n_crea != n_age || n_crea != n_female || n_age != n_female) stop("Size mismatch! Check input data.", call. = FALSE)

  k <- ifelse(female == 1, 62, 80)
  alpha <- ifelse(female == 1, -0.329, -0.411)

  out <- numeric(length = n_crea)
  f1 <- which(female == 1)

  out <- 141 * (pmin(creatinine / k, 1)^alpha) * (pmax(creatinine / k, 1)^(-1.209)) * (0.993^age)
  out[f1] <- out[f1] * 1.018

  return(out)
}
