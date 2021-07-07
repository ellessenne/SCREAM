#' @title Example of a dataset with ICD-10 codes
#'
#' @format A data frame with 123,772 rows and 7 variables, for 5,000 subjects. Included variables are:
#' * `id` Subject ID.
#' * `date` Date corresponding to each code.
#' * `index_date` Index date (used e.g. to calculate multimorbidity at that specific point in time).
#' * `code` ICD-10 code.
#' * `origin` Origin of each ICD-10 code, with `origin = 1` denoting hospital care, `origin = 2` denoting secondary care, and `origin = 3` denoting primary care.
#' * `n` Sequential number for each observation.
#' * `N` Total number of observations per subject.
#' @rdname icd10
#'
#' @examples
#' data("icd10", package = "SCREAM")
"icd10"
