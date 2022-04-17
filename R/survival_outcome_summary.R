#' Summarise the basic information of a survival outcome
#'
#' @description `survival_outcome_summary` is a function used to produce a summary
#' of a survival outcome in a cohort study, including number of patients,
#' follow-up time (median [IQR], years), number of events, person years,
#' rate with 95% confidence interval (per 1000 person years).
#'
#' @param outcome survival outcome of interest.
#' @param tstart index date with class `"Date"` or numeric value `0`.
#' @param tstop outcome occurrence date or censoring date with class `"Date"` or
#' numeric value of days from index date to outcome occurrence date or censoring date.
#' @param exposure the summary of the survival outcome is usually displayed in each exposure
#' group, if the summary of the survival outcome for all included patients should displayed,
#' set `exposure = "all"`.
#' @param weight weight for each included patient, in crude cohort dataset, it is equal to `1`,
#' in propensity-score weighted cohort dataset, it is equal to \eqn{\frac{1}{PS}} for exposed group,
#' and \eqn{\frac{1}{1-PS}} for noexposed group.
#' @param dataset dataset of the cohort study, in a time-fixed format of one patient one row.
#' @param digit number of digits after the decimal point
#'
#' @return a dataframe showing the summary of the survival outcome.
#' @export
#' @importFrom dplyr "mutate" "summarise" "group_by"
#' @import lubridate spatstat.geom
#'
#' @examples
#' data("sample_survival_data", package = "SCREAM")
#' survival_outcome_summary("outcome_event",
#'     "tstart_as_Date",
#'     "tstop_as_Date",
#'     "all",
#'     "null_weight",
#'     sample_survival_data,
#'     digit = 2)
survival_outcome_summary <- function(outcome,
                                     tstart,
                                     tstop,
                                     exposure,
                                     weight,
                                     dataset,
                                     digit = 2) {
  if (class(dataset[[tstart]]) == 'Date' & class(dataset[[tstop]]) == 'Date') {
    dta <- dataset %>%
      mutate(time2outcome = time_length(difftime(.data[[tstop]],
                                                 .data[[tstart]]),
                                        'year'))
  } else {
    dta <- dataset %>%
      mutate(time2outcome = (.data[[tstop]] - .data[[tstart]]) / 365.25)
  }
  if (exposure == 'all') {
    base_trt <- dta %>% mutate(group = exposure) %>%
      group_by(group) %>%
      summarise(n = sum(.data[[weight]]),
                median.followup = round(weighted.median(time2outcome, w = .data[[weight]]), digit),
                q25.followup = round(weighted.quantile(time2outcome, w = .data[[weight]], 0.25), digit),
                q75.followup = round(weighted.quantile(time2outcome, w = .data[[weight]], 0.75), digit)
      ) %>% data.frame()
    incid_trt <- dta  %>% mutate(group = exposure) %>%
      group_by(group) %>%
      summarise(event = round(sum(.data[[outcome]] * .data[[weight]]), digit),
                person_years = round(sum(time2outcome * .data[[weight]]), digit),
                rate = round(event / person_years * 1000, digit),
                lci = round(rate_CI_calculation(event)[1] / person_years * 1000, digit),
                uci = round(rate_CI_calculation(event)[2] / person_years * 1000, digit))
  } else {
    base_trt <- dta %>% mutate(group = .data[[exposure]]) %>%
      group_by(group) %>%
      summarise(n = sum(.data[[weight]]),
                median.followup = round(weighted.median(time2outcome, w = weight), digit),
                q25.followup = round(weighted.quantile(time2outcome, w = weight, 0.25), digit),
                q75.followup = round(weighted.quantile(ttime2outcome, w = weight, 0.75), digit)
      )
    incid_trt <- dta %>% mutate(group = .data[[exposure]]) %>%
      group_by(group) %>%
      summarise(event = round(sum(.data[[outcome]] * .data[[weight]]), digit),
                person_years = round(sum(time2outcome * .data[[weight]]), digit),
                rate = round(event / person_years * 1000, digit),
                lci = round(rate_CI_calculation(event)[1] / person_years * 1000, digit),
                uci = round(rate_CI_calculation(event)[2] / person_years * 1000, digit))
  }
  incid_trt <- as.data.frame(incid_trt) %>%
    mutate(rate = paste0(rate, " (", lci, "-", uci, ")"))
  total_trt <- base_trt %>%
    mutate(followup = paste0(base_trt[, 3], " [", base_trt[, 4], "-", base_trt[, 5], "]")) %>%
    dplyr:: select(c(1, 2, 6)) %>%
    cbind(incid_trt[, c(2, 3, 4)])
  rownames(total_trt) <- NULL
  colnames(total_trt) <- c('Group', 'N', 'Follow-up, years', 'No.event', 'Person years', 'Incidence rate, per 1000 person years)')
  return(total_trt)
}


#' @noRd
rate_CI_calculation <- function (X, conf.level = 0.95) {
  alpha = 1 - conf.level
  upper <- 0.5 * qchisq((1 - (alpha / 2)), (2 * X))
  lower <- 0.5 * qchisq(alpha / 2, (2 * X + 2))
  return(c(lower, upper))
}


