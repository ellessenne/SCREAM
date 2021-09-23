#' Calculate multimorbidity domains
#'
#' @description Calculate 30 morbidity domains corresponding to chronic conditions, according to the definition of Tonelli et al. (see reference below).
#'
#' @param data_hospitalisations Dataset used for data input, containing hospitalisation codes (see the algorithm in the paper by Tonelli et al.).
#' Must be in long format and contain a column with subject IDs, ICD-10 codes, dates at which each code was recorded, and an index date at which the conditions are calculated.
#' @param data_claims Dataset used for data input, containing inpatient and outpatient codes (see the algorithm in the paper by Tonelli et al.).
#' Must be in long format and contain a column with subject IDs, ICD-10 codes, dates at which each code was recorded, and an index date at which the conditions are calculated.
#' @param data_drugs Dataset used for data input, containing drugs purchases. This expands the algorithm by Tonelli et al., and must be in long format and including a column with subject IDs, ATC codes, dates at which each drug was purchased, how many packages of such drugs were purchased, and an index date at which the conditions are calculated.
#' @param id Name of the column identifying subject IDs in every `data_*` dataset.
#' @param code Name of the column identifying ICD-10 codes in `data_hospitalisations` and `data_claims`.
#' @param date Name of the column identifying dates at which codes are recorded or drugs purchased in every `data_*` input dataset.
#' @param index_date Name of the column identifying index date in every `data_*` input dataset.
#' @param atc Name of the column identifying ATC codes in the `data_drugs` input dataset.
#' @param npacks Name of the column identifying the number of purchased drug packages in the `data_drugs` input dataset.
#' @param combine_cirrhosis Cirrhosis is defined as the concurrent presence of (at least) two codes, if `combine_cirrhosis = TRUE` (the default) then a single column (combination of the two) is returned.
#' If not, two columns are returned.
#'
#' @return A dataset with a row per individual and a column per condition.
#'
#' @references Tonelli, M., Wiebe, N., Fortin, M. et al. _Methods for identifying 30 chronic conditions: application to administrative data._ BMC Med Inform Decis Mak 15, 31 (2016). \doi{10.1186/s12911-015-0155-5}.
#'
#' @export
#'
#' @examples
#' data("icd10", package = "SCREAM")
#' data("drug", package = "SCREAM")
#' multimorbidity(
#'   data_hospitalisations = icd10$hospitalisations,
#'   data_claims = icd10$claims,
#'   data_drugs = drug,
#'   id = "id",
#'   code = "code",
#'   date = "date",
#'   index_date = "index_date",
#'   atc = "atc",
#'   npacks = "npacks"
#' )
multimorbidity <- function(data_hospitalisations, data_claims, data_drugs, id, code, date, index_date, atc, npacks, combine_cirrhosis = TRUE) {
  # devtools::load_all()
  # library(dplyr)
  # data("icd10", package = "SCREAM")
  # data("drug", package = "SCREAM")
  # data_hospitalisations <- icd10$hospitalisations
  # data_claims <- icd10$claims
  # data_hospitalisations <- rename(data_hospitalisations, lopnr = id, diagnosis = code, datum = date)
  # data_claims <- rename(data_claims, lopnr = id, diagnosis = code, datum = date)
  # data_drugs <- rename(drug, lopnr = id, antal = npacks, datum = date)
  # id <- "lopnr"
  # code <- "diagnosis"
  # date <- "datum"
  # atc <- "atc"
  # npacks <- "antal"
  # index_date <- "index_date"
  # combine_cirrhosis <- TRUE

  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'data_hospitalisations', 'data_claimns', and 'data_drugs' must be data.frames (or analogous)
  checkmate::assert_true(x = inherits(x = data_hospitalisations, what = c("data.frame", "data.table", "tbl", "tbl_df")), add = arg_checks)
  checkmate::assert_true(x = inherits(x = data_claims, what = c("data.frame", "data.table", "tbl", "tbl_df")), add = arg_checks)
  checkmate::assert_true(x = inherits(x = data_drugs, what = c("data.frame", "data.table", "tbl", "tbl_df")), add = arg_checks)
  # id, code, date, index_date must be a single string value
  checkmate::assert_string(x = id, add = arg_checks)
  checkmate::assert_string(x = code, add = arg_checks)
  checkmate::assert_string(x = date, add = arg_checks)
  checkmate::assert_string(x = index_date, add = arg_checks)
  checkmate::assert_string(x = atc, add = arg_checks)
  checkmate::assert_string(x = npacks, add = arg_checks)
  # combine_cirrhosis must be a boolean
  checkmate::assert_logical(x = combine_cirrhosis, add = arg_checks)
  checkmate::assert_string(x = index_date, add = arg_checks)
  # id, code, date, index_date must be in data_inpatient and data_outpatient
  checkmate::assert_subset(x = id, choices = names(data_hospitalisations), add = arg_checks)
  checkmate::assert_subset(x = code, choices = names(data_hospitalisations), add = arg_checks)
  checkmate::assert_subset(x = date, choices = names(data_hospitalisations), add = arg_checks)
  checkmate::assert_subset(x = index_date, choices = names(data_hospitalisations), add = arg_checks)
  checkmate::assert_subset(x = id, choices = names(data_claims), add = arg_checks)
  checkmate::assert_subset(x = code, choices = names(data_claims), add = arg_checks)
  checkmate::assert_subset(x = date, choices = names(data_claims), add = arg_checks)
  checkmate::assert_subset(x = index_date, choices = names(data_claims), add = arg_checks)
  # Analogous for data_drugs
  checkmate::assert_subset(x = id, choices = names(data_drugs), add = arg_checks)
  checkmate::assert_subset(x = date, choices = names(data_drugs), add = arg_checks)
  checkmate::assert_subset(x = index_date, choices = names(data_drugs), add = arg_checks)
  checkmate::assert_subset(x = atc, choices = names(data_drugs), add = arg_checks)
  checkmate::assert_subset(x = npacks, choices = names(data_drugs), add = arg_checks)
  # 'date', 'index_date' must be actual dates
  checkmate::assert_date(x = data_hospitalisations[[date]], add = arg_checks)
  checkmate::assert_date(x = data_hospitalisations[[index_date]], add = arg_checks)
  checkmate::assert_date(x = data_claims[[date]], add = arg_checks)
  checkmate::assert_date(x = data_claims[[index_date]], add = arg_checks)
  checkmate::assert_date(x = data_drugs[[date]], add = arg_checks)
  checkmate::assert_date(x = data_drugs[[index_date]], add = arg_checks)
  # 'code', 'atc' must be strings
  checkmate::assert_character(x = data_hospitalisations[[code]], add = arg_checks)
  checkmate::assert_character(x = data_claims[[code]], add = arg_checks)
  checkmate::assert_character(x = data_drugs[[atc]], add = arg_checks)
  # 'npacks' must be numeric
  checkmate::assert_numeric(x = data_drugs[[npacks]], add = arg_checks)
  # Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### First, prep data
  data_hospitalisations <- .multimorbidity_prep_data(data = data_hospitalisations, id = id, code = code, date = date, index_date = index_date)
  data_claims <- .multimorbidity_prep_data(data = data_claims, id = id, code = code, date = date, index_date = index_date)
  data_drugs <- .multimorbidity_prep_drugs(data = data_drugs, id = id, atc = atc, npacks = npacks, date = date, index_date = index_date)

  ### Then, calculate conditions for hospitalisation data
  out_hospitalisations <- .multimorbidity_compute(data_hospitalisations, doing_hospitalisations = TRUE)

  ### Then, for all claims data
  out_claims <- .multimorbidity_compute(data_claims, doing_hospitalisations = FALSE)

  ### Augment with zeros if some IDs are missing in either...
  IDdf <- data.table()
  IDdf[, id := unique(c(data_hospitalisations[["id"]], data_claims[["id"]], data_drugs[["id"]]))]
  out_hospitalisations <- merge(out_hospitalisations, IDdf, all.y = TRUE, allow.cartesian = TRUE, by = "id")
  data.table::setnafill(x = out_hospitalisations, type = "const", fill = 0)
  out_claims <- merge(out_claims, IDdf, all.y = TRUE, allow.cartesian = TRUE, by = "id")
  data.table::setnafill(x = out_claims, type = "const", fill = 0)

  ### Sanity check here, just in case
  .multimorbidity_sanity(hospitalisations = out_hospitalisations, claims = out_claims)

  ### Now, combine the two
  out <- data.table()
  out[, id := out_hospitalisations[["id"]]]
  for (w in names(.multimorbidity_howmany())) {
    doing <- .multimorbidity_howmany()[[w]]
    if (is.na(doing["claims"])) {
      res <- ifelse(out_hospitalisations[[w]] >= doing["hospitalisations"], 1, 0)
    } else {
      res <- ifelse(out_hospitalisations[[w]] >= doing["hospitalisations"] | out_claims[[w]] >= doing["claims"], 1, 0)
    }
    out[, (w) := res]
  }

  # Combine 'cirrhosis1' and 'cirrhosis2' (if requested)
  if (combine_cirrhosis) {
    out[, cirrhosis := pmin(cirrhosis1, cirrhosis2)]
    out[, cirrhosis1 := NULL]
    out[, cirrhosis2 := NULL]
    mv <- c("id", .multimorbidity_order())
    out <- out[, ..mv]
  }

  ### Enrich algorithm with drugs data
  out_drugs <- .multimorbidity_compute_drugs(data = data_drugs)
  data.table::setnames(x = out_drugs, new = c("id", paste0("drugs_", names(out_drugs)[names(out_drugs) != "id"])))
  out <- merge(x = out, y = out_drugs, all.x = TRUE, by = "id")
  data.table::setnafill(x = out_drugs, type = "const", fill = 0)
  out[, cpain := ifelse(epilepsy == 1, pmax(cpain, drugs_cpain), pmax(cpain, drugs_cpain, drugs_cpain_no_epilepsy))]
  out[, depression := pmax(depression, drugs_depression)]
  out[, drugs_cpain := NULL]
  out[, drugs_cpain_no_epilepsy := NULL]
  out[, drugs_depression := NULL]

  ### Restore ID column and return
  data.table::setnames(x = out, old = "id", new = id)
  out <- data.table::setDF(out)
  return(out)
}

#' @keywords internal
.multimorbidity_prep_data <- function(data, id, code, date, index_date) {
  ### Tidy codes
  data <- comorbidity:::.tidy(x = data, code = code)
  ### Turn x into a DT
  data.table::setDT(data)
  ### Subset only relevant columns
  mv <- c(id, code, date, index_date)
  data <- data[, ..mv]
  ### Assign standardised names
  data.table::setnames(x = data, new = c("id", "code", "date", "index_date"))
  ### Return data.table
  return(data)
}

#' @keywords internal
.multimorbidity_prep_drugs <- function(data, id, atc, npacks, date, index_date) {
  ### Turn x into a DT
  data.table::setDT(data)
  ### Subset only relevant columns
  mv <- c(id, atc, npacks, date, index_date)
  data <- data[, ..mv]
  ### Assign standardised names
  data.table::setnames(x = data, new = c("id", "atc", "npacks", "date", "index_date"))
  ### Return data.table
  return(data)
}

#' @keywords internal
.multimorbidity_compute <- function(data, doing_hospitalisations) {
  ### Keep only codes that can be used somewhere
  allc <- c(.multimorbidity_codes(), .multimorbidity_exclusions())
  idx <- vapply(X = allc, FUN = function(x) !is.null(x), FUN.VALUE = logical(length = 1L))
  allc <- allc[idx]
  allc <- lapply(X = allc, FUN = .collapse_codes)
  allc <- paste(allc, collapse = "|")
  mv <- c("id", "index_date")
  safetydf <- data[, ..mv]
  safetydf <- unique(safetydf)
  idx <- stringi::stri_detect_regex(str = data[["code"]], pattern = allc)
  data <- data[idx]
  data <- merge(data, safetydf, all.y = TRUE, allow.cartesian = TRUE, by = c("id", "index_date"))
  data.table::set(data, which(is.na(data[["code"]])), "code", ".NOTACODE!")
  data[["date"]][is.na(data[["date"]])] <- data[["index_date"]][is.na(data[["date"]])]

  ### If doing claims, chronic pain is a different story
  if (isFALSE(doing_hospitalisations)) {
    idx <- stringi::stri_detect_regex(str = data[["code"]], pattern = .collapse_codes(.multimorbidity_codes()[["cpain"]]))
    data_cpain <- data[idx]
    data.table::setorderv(x = data_cpain, cols = c("id", "date"))
    data_cpain[, lag1 := data.table::shift(date, n = 1), by = "id"]
    data_cpain[, ddiff := as.numeric(date - lag1)]
    data_cpain[, ok := as.numeric(!is.na(ddiff) & (ddiff >= 30))]
    data_cpain <- data_cpain[, .(ok = sum(ok)), by = "id"]
    data_cpain <- merge(data_cpain, safetydf, all.y = TRUE, allow.cartesian = TRUE, by = "id")
    data_cpain[, index_date := NULL]
    data_cpain[, ok := ifelse(is.na(ok), 0, ok)]
    data_cpain[, ok := as.numeric(ok > 0)]
  }

  ### Apply permanence of codes (if not doing hospitalisations)
  if (isFALSE(doing_hospitalisations)) {
    data <- data[date <= index_date]
    data[, .yd := index_date - date]
    data[, .yd := as.numeric(.yd) / 365.242]
    data[, .target := NA]
    for (k in seq_along(.multimorbidity_codes())) {
      if (!is.null(.multimorbidity_years()[[k]])) {
        idx <- grep(pattern = .collapse_codes(x = .multimorbidity_codes()[[k]]), x = data[["code"]])
        data$.target[idx] <- .multimorbidity_years()[[k]]
      }
    }
    data <- data[is.na(.target) | .yd <= .target]
    data[, .yd := NULL]
    data[, .target := NULL]
    data <- merge(data, safetydf, all.y = TRUE, allow.cartesian = TRUE, by = c("id", "index_date"))
    data.table::set(data, which(is.na(data[["code"]])), "code", ".NOTACODE!")
    data[["date"]][is.na(data[["date"]])] <- data[["index_date"]][is.na(data[["date"]])]
  }

  ### Subset only 'id' and 'code' columns
  mv <- c("id", "code")
  data <- data[, ..mv]

  ### Apply all regex
  for (k in seq_along(.multimorbidity_codes())) {
    cds <- .collapse_codes(x = .multimorbidity_codes()[[k]])
    res <- stringi::stri_detect_regex(str = data[["code"]], pattern = cds)
    data[, (names(.multimorbidity_codes())[k]) := res]
  }

  ### Summarise by 'id'
  out <- purrr::map(.x = names(.multimorbidity_codes()), .f = function(n) {
    data[, stats::setNames(list(sum(get(n))), n), by = "id"]
    # Use the following line instead to get one-zeros instead of counting the number of matches:
    # data[, stats::setNames(list(as.numeric(sum(get(n)) > 0)), n), by = id]
  })
  out <- purrr::reduce(.x = out, .f = merge, all = TRUE, by = "id")
  data.table::setorderv(out, cols = "id")

  ### Now, fix chronic pain
  if (isFALSE(doing_hospitalisations)) {
    # This turns all cpain values into zeros if there are no *at least* two codes >30 days apart
    out[, cpain := cpain * data_cpain[["ok"]]]
  }

  ### Process exclusions
  mv <- c("id", "code")
  data <- data[, ..mv]
  for (k in seq_along(.multimorbidity_exclusions())) {
    if (!is.null(.multimorbidity_exclusions()[[k]])) {
      cds <- .collapse_codes(x = .multimorbidity_exclusions()[[k]])
      res <- grepl(pattern = cds, x = data[["code"]])
      data[, (names(.multimorbidity_codes())[k]) := res]
    }
  }
  excl <- purrr::map(.x = names(.multimorbidity_exclusions()), .f = function(n) {
    if (!is.null(.multimorbidity_exclusions()[[n]])) {
      data[, stats::setNames(list(sum(get(n))), n), by = "id"]
    }
  })
  excl <- excl[!vapply(X = excl, FUN = is.null, FUN.VALUE = logical(length = 1L))]
  excl <- purrr::reduce(.x = excl, .f = merge, all = TRUE, by = "id")
  data.table::setorderv(excl, cols = "id")
  for (n in names(excl)) {
    if (n != "id") {
      excl[, (n) := ifelse(get(n) > 0, 1, 0)]
    }
  }
  # ->
  out[, ibs := ibs * (1 - excl$ibs)]
  out[, severe_constipation := severe_constipation * (1 - excl$severe_constipation)]

  ### Return
  out <- data.table::setDF(out)
  return(out)
}

#' @keywords internal
.multimorbidity_compute_drugs <- function(data) {
  ### Keep only codes that can be used somewhere
  idx <- vapply(X = .multimorbidity_drugs_atc(), FUN = function(x) !is.null(x), FUN.VALUE = logical(length = 1L))
  allc <- .multimorbidity_drugs_atc()[idx]
  allc <- lapply(X = allc, FUN = .collapse_codes)
  allc <- paste(allc, collapse = "|")
  mv <- c("id", "index_date")
  safetydf <- data[, ..mv]
  safetydf <- unique(safetydf)
  idx <- stringi::stri_detect_regex(str = data[["atc"]], pattern = allc)
  data <- data[idx]
  data <- merge(data, safetydf, all.y = TRUE, allow.cartesian = TRUE, by = c("id", "index_date"))
  data.table::set(data, which(is.na(data[["atc"]])), "atc", ".NOTADRUG!")
  data.table::set(data, which(is.na(data[["npacks"]])), "npacks", 0)
  data[["date"]][is.na(data[["date"]])] <- data[["index_date"]][is.na(data[["date"]])]

  ### Filter purchases prior to index date
  data <- data[date <= index_date]
  ### Filter codes in the previous .yd years only
  data[, .yd := index_date - date]
  data[, .yd := as.numeric(.yd) / 365.242]
  data[, .target := NA]
  for (k in seq_along(.multimorbidity_drugs_atc())) {
    if (!is.null(.multimorbidity_drugs_years()[[k]])) {
      idx <- grep(pattern = .collapse_codes(x = .multimorbidity_drugs_atc()[[k]]), x = data[["atc"]])
      data$.target[idx] <- .multimorbidity_drugs_years()[[k]]
    }
  }
  data <- data[is.na(.target) | .yd <= .target]
  data[, .yd := NULL]
  data[, .target := NULL]
  data <- merge(data, safetydf, all.y = TRUE, allow.cartesian = TRUE, by = c("id", "index_date"))
  data.table::set(data, which(is.na(data[["code"]])), "code", ".NOTACODE!")
  data.table::set(data, which(is.na(data[["atc"]])), "atc", ".NOTADRUG!")
  data.table::set(data, which(is.na(data[["npacks"]])), "npacks", 0)
  data[["date"]][is.na(data[["date"]])] <- data[["index_date"]][is.na(data[["date"]])]
  for (d in seq_along(.multimorbidity_drugs_atc())) {
    res <- stringi::stri_detect_regex(str = data[["atc"]], pattern = .collapse_codes(.multimorbidity_drugs_atc()[[d]]))
    doing <- names(.multimorbidity_drugs_atc())[d]
    data[[doing]] <- res
    data[[doing]] <- data[[doing]] * data[["npacks"]]
  }
  data <- purrr::map(.x = names(.multimorbidity_drugs_atc()), .f = function(n) {
    data[, stats::setNames(list(sum(get(n))), n), by = "id"]
    # Use the following line instead to get one-zeros instead of counting the number of matches:
    # data[, stats::setNames(list(as.numeric(sum(get(n)) > 0)), n), by = id]
  })
  data <- purrr::reduce(.x = data, .f = merge, all = TRUE, by = "id")

  ### Apply the 'how many' logic
  out <- data.table()
  out[, id := data[["id"]]]
  for (w in names(.multimorbidity_drugs_howmany())) {
    doing <- .multimorbidity_drugs_howmany()[[w]]
    res <- ifelse(data[[w]] >= doing, 1, 0)
    out[, (w) := res]
  }

  ### Return
  return(out)
}

#' @keywords internal
.multimorbidity_sanity <- function(hospitalisations, claims) {
  .msg <- "An unexpected error occurred. Please file an issue at https://github.com/ellessenne/SCREAM/issues, including a reproducible example (https://github.com/ellessenne/SCREAM/issues)."
  if (any(names(hospitalisations) != names(claims))) stop(.msg, call. = FALSE)
  if (ncol(hospitalisations) != ncol(claims)) stop(.msg, call. = FALSE)
  if (nrow(hospitalisations) != nrow(claims)) stop(.msg, call. = FALSE)
}

#' @keywords internal
.collapse_codes <- function(x) paste0("^", paste(x, collapse = "|^"))
