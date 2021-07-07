#' @title Utility functions for data analysis in SCREAM
#'
#' @description Utility function for data analysis using data from the Stockholm
#' CREAtinine Measurements project.
#'
#' @name SCREAM
#' @docType package
#' @author Alessandro Gasparini (alessandro.gasparini@@ki.se)
#' @import comorbidity data.table lme4 mvtnorm stats usethis
#' @importFrom data.table :=
#'
# Quiets concerns of R CMD check re: variable names used internally
if (getRversion() >= "2.15.1") utils::globalVariables(c("..mv", "...yd", "...target", "ibs", "severe_constipation"))
