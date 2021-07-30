#' @title Example of a dataset with ICD-10 codes
#'
#' @format A list with two simulated data frames of ICD-10 codes for 3,000 subjects, one with hospitalisation codes, and one with inpatient and outpatient claims.
#' Each data set includes the following variables:
#' * `id` Subject ID.
#' * `date` Date corresponding to each code.
#' * `index_date` Index date (used e.g. to calculate multimorbidity at that specific point in time).
#' * `code` ICD-10 code.
#' @rdname icd10
#'
#' @examples
#' data("icd10", package = "SCREAM")
"icd10"
