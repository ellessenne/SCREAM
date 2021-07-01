#' @title Example of a dataset with ICD-10 codes
#'
#' @format A data frame with 51,118 rows and 6 variables:
#' * `id` Subject ID.
#' * `date` Date corresponding to each code.
#' * `index_date` Index date (used e.g. to calculate multimorbidity at that specific point in time).
#' * `code` ICD-10 code.
#' * `n` Sequential number for each observation.
#' * `N` Total number of observations per subject.
#' @rdname icd10
#'
#' @examples
#' data("icd10", package = "SCREAM")
"icd10"
