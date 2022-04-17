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

#' @title Example Dataset with Drug Utilisation Codes
#'
#' @format A data frame with drug utilisation information for 3,000 subjects with the following columns:
#' * `id` Subject ID.
#' * `date` Date corresponding to each drug purchase.
#' * `index_date` Index date (used e.g. to calculate multimorbidity at that specific point in time).
#' * `atc` ATC code.
#' * `npacks` Number of drug packages purchased at this given date.
#' @rdname drug
#'
#' @examples
#' data("drug", package = "SCREAM")
"drug"


#' @title Example Dataset of survival outcome
#'
#' @format A data frame of 1,000 subjects with the following columns:
#' * `lopnr` subject unique ID.
#' * `index_date` index date.
#' * `tstart_as_Date` or `tstart` index date with class `Date` or type `numeric`.
#' * `tstop_as_Date` or `tstop` outcome/censoring date with class `Date` or type `numeric`.
#' * `outcome_event` indicator of the survival outcome.
#' * `null_weight` weight for each included patients.
#' @rdname sample_survival_data
#'
#' @examples
#' data("sample_survival_data", package = "SCREAM")
"sample_survival_data"
