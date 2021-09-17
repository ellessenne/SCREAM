#' @title Example Dataset with ICD-10 Codes
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

#' @title Index of ATC Codes
#'
#' @format An index of ATC codes, including compound names.
#' The following columns are included:
#' * `ATC.Code` The ATC code.
#' * `Name` Name of the compound corresponding to a given ATC.
#' @rdname atc
#'
#' @examples
#' data("atc", package = "SCREAM")
"atc"
