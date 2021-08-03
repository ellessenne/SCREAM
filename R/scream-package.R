#' @title Utility functions for data analysis in SCREAM
#'
#' @description Utility function for data analysis using data from the Stockholm
#' CREAtinine Measurements project.
#'
#' @name SCREAM
#' @docType package
#' @author Alessandro Gasparini (alessandro.gasparini@@ki.se)
#' @import comorbidity data.table lme4 mvtnorm stats stringi usethis
#' @importFrom data.table :=
#'

# Quiets concerns of R CMD check re: variable names used internally
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "..mv", ".target", ".yd", "cirrhosis", "cirrhosis1", "cirrhosis2", "cpain", "ddiff", "ibs", "index_date", "lag1", "ok", "severe_constipation"))

#' @keywords internal
.datatable.aware <- TRUE
