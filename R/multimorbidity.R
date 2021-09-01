#' Calculate multimorbidity domains
#'
#' @description Calculate 30 morbidity domains corresponding to chronic conditions, according to the definition of Tonelli et al. (see reference below).
#'
#' @param data_hospitalisations Dataset used for data input, containing hospitalisation codes (see the algorithm in the paper by Tonelli et al.).
#' Must be in long format and contain a column with subject IDs, ICD-10 codes, dates at which each code was recorded, and an index date at which the conditions are calculated.
#' @param data_claims Dataset used for data input, containing inpatient and outpatient codes (see the algorithm in the paper by Tonelli et al.).
#' Must be in long format and contain a column with subject IDs, ICD-10 codes, dates at which each code was recorded, and an index date at which the conditions are calculated.
#' @param id Name of the column identifying subject IDs in `data`.
#' @param code Name of the column identifying ICD-10 codes in `data`.
#' @param date Name of the column identifying dates at which codes are recorded in `data`.
#' @param index_date Name of the column identifying index date in `data`.
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
#' multimorbidity(
#'   data_hospitalisations = icd10$hospitalisations,
#'   data_claims = icd10$claims,
#'   id = "id",
#'   code = "code",
#'   date = "date",
#'   index_date = "index_date"
#' )
multimorbidity <- function(data_hospitalisations = NULL, data_claims = NULL, id, code, date, index_date, combine_cirrhosis = TRUE) {
  # library(dplyr)
  # data("icd10", package = "SCREAM")
  # data_hospitalisations <- icd10$hospitalisations
  # data_claims <- icd10$claims
  # data_hospitalisations <- rename(data_hospitalisations, lopnr = id, diagnosis = code, datum = date)
  # data_claims <- rename(data_claims, lopnr = id, diagnosis = code, datum = date)
  # id <- "lopnr"
  # code <- "diagnosis"
  # date <- "datum"
  # index_date <- "index_date"

  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'data_inpatient' and 'data_outpatient' must be data.frames (or analogous)
  checkmate::assert_true(x = inherits(x = data_hospitalisations, what = c("data.frame", "data.table", "tbl", "tbl_df")), add = arg_checks)
  checkmate::assert_true(x = inherits(x = data_claims, what = c("data.frame", "data.table", "tbl", "tbl_df")), add = arg_checks)
  # id, code, date, index_date must be a single string value
  checkmate::assert_string(x = id, add = arg_checks)
  checkmate::assert_string(x = code, add = arg_checks)
  checkmate::assert_string(x = date, add = arg_checks)
  checkmate::assert_string(x = index_date, add = arg_checks)
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
  # 'date', 'index_date' must be actual dates
  checkmate::assert_date(x = data_hospitalisations[[date]], add = arg_checks)
  checkmate::assert_date(x = data_hospitalisations[[index_date]], add = arg_checks)
  checkmate::assert_date(x = data_claims[[date]], add = arg_checks)
  checkmate::assert_date(x = data_claims[[index_date]], add = arg_checks)
  # Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### First, prep data
  data_hospitalisations <- .multimorbidity_prep_data(data = data_hospitalisations, id = id, code = code, date = date, index_date = index_date)
  data_claims <- .multimorbidity_prep_data(data = data_claims, id = id, code = code, date = date, index_date = index_date)

  ### Then, calculate conditions for hospitalisation data
  out_hospitalisations <- .multimorbidity_compute(data_hospitalisations, doing_hospitalisations = TRUE)

  ### Then, for all claims data
  out_claims <- .multimorbidity_compute(data_claims, doing_hospitalisations = FALSE)

  ### Augment with zeros if some IDs are missing in either...
  IDdf <- data.table()
  IDdf[, id := unique(c(data_hospitalisations[["id"]], data_claims[["id"]]))]
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

  # Finally, combine 'cirrhosis1' and 'cirrhosis2' before returning (if so)
  if (combine_cirrhosis) {
    out[, cirrhosis := pmin(cirrhosis1, cirrhosis2)]
    out[, cirrhosis1 := NULL]
    out[, cirrhosis2 := NULL]
    mv <- c("id", .multimorbidity_order())
    out <- out[, ..mv]
  }

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
.multimorbidity_sanity <- function(hospitalisations, claims) {
  .msg <- "An unexpected error occurred. Please file an issue at https://github.com/ellessenne/SCREAM/issues, including a reproducible example (https://github.com/ellessenne/SCREAM/issues)."
  if (any(names(hospitalisations) != names(claims))) stop(.msg, call. = FALSE)
  if (ncol(hospitalisations) != ncol(claims)) stop(.msg, call. = FALSE)
  if (nrow(hospitalisations) != nrow(claims)) stop(.msg, call. = FALSE)
}

#' @keywords internal
.multimorbidity_codes <- function() {
  list(
    alcohol_misuse = c("E52", "F10", "G621", "I426", "K292", "K700", "K703", "K709", "T51", "Z502", "Z714", "Z721"),
    asthma = c("J45"),
    afib = c("I480"),
    cancer_lymphoma = c("C81", "C82", "C83", "C84", "C85", "C88", "C900", "C902", "C96"),
    cancer_metastatic = c("C77", "C78", "C79", "C80"),
    cancer_nonmetastatic = c("C18", "C19", "C20", "C21", "C33", "C34", "C384", "C450", "C4671", "C50", "C53", "C61", "D010", "D011", "D012", "D013", "D022", "D05", "D06", "D075"),
    chf = c("I099", "I255", "I420", "I425", "I426", "I427", "I428", "I429", "I43", "I50"),
    ckd = c("N00", "N01", "N02", "N03", "N04", "N05", "N06", "N07", "N08", "N10", "N11", "N12", "N13", "N14", "N15", "N16", "N17", "N18", "N19", "N20", "N21", "N22", "N23"),
    cpain = c("F454", "M081", "M255", "M432", "M433", "M434", "M435", "M436", "M45", "M461", "M463", "M464", "M469", "M47", "M480", "M481", "M488", "M489", "M508", "M509", "M51", "M531", "M532", "M533", "M538", "M539", "M54", "M608", "M609", "M633", "M790", "M791", "M792", "M796", "M797", "M961"),
    cpd = c("I278", "I279", "J40", "J41", "J42", "J43", "J44", "J46", "J47", "J60", "J61", "J62", "J63", "J64", "J65", "J66", "J67", "J684", "J701", "J703"),
    cvhepatitis = c("B16", "B180", "B181"),
    cirrhosis1 = c("K703", "K743", "K744", "K745", "K746"),
    cirrhosis2 = c("I850", "I859", "I982", "I983", "K650", "K658", "K659", "K670", "K671", "K672", "K673", "K678", "K767", "K930", "R18"),
    dementia = c("F00", "F01", "F02", "F03", "F051", "G30", "G311"),
    depression = c("F204", "F313", "F314", "F315", "F32", "F33", "F341", "F412", "F432"),
    diabetes = c("E10", "E11", "E12", "E13", "E14"),
    epilepsy = c("G40", "G41"),
    hypertension = c("I10", "I11", "I12", "I13", "I15"),
    hypothyroidism = c("E00", "E01", "E02", "E03", "E890"),
    ibd = c("K50", "K51"),
    ibs = c("K58"),
    multiple_sclerosis = c("G35", "G36", "G37", "H46"),
    mi = c("I21", "I22"),
    parkinson = c("G20", "G21", "G22"),
    pud = c("K257", "K259", "K267", "K269", "K277", "K279", "K287", "K289"),
    pvd = c("I702"),
    psoriasis = c("L400", "L401", "L402", "L403", "L404", "L408", "L409"),
    rheum_arthritis = c("M05", "M06", "M315", "M32", "M33", "M34", "M351", "M353", "M360"),
    schizofrenia = c("F20", "F21", "F232", "F25"),
    severe_constipation = c("K558", "K560", "K564", "K567", "K590", "K631", "K634", "K638", "K928"),
    stroke = c("G450", "G451", "G452", "G453", "G458", "G459", "H341", "I60", "I61", "I63", "I64")
  )
}

#' @keywords internal
.multimorbidity_exclusions <- function() {
  list(
    alcohol_misuse = NULL,
    asthma = NULL,
    afib = NULL,
    cancer_lymphoma = NULL,
    cancer_metastatic = NULL,
    cancer_nonmetastatic = NULL,
    chf = NULL,
    ckd = NULL,
    cpain = NULL,
    cpd = NULL,
    cvhepatitis = NULL,
    cirrhosis1 = NULL,
    cirrhosis2 = NULL,
    dementia = NULL,
    depression = NULL,
    diabetes = NULL,
    epilepsy = NULL,
    hypertension = NULL,
    hypothyroidism = NULL,
    ibd = NULL,
    ibs = c("C18", "C19", "C20", "C21", "C25", "C56", "C785", "C796", "D017", "D019", "D371", "D372", "D373", "D374", "D375", "K50", "K51", "K702", "K703", "K740", "K742", "K746", "K860", "K861", "K90", "K912"),
    multiple_sclerosis = NULL,
    mi = NULL,
    parkinson = NULL,
    pud = NULL,
    pvd = NULL,
    psoriasis = NULL,
    rheum_arthritis = NULL,
    schizofrenia = NULL,
    severe_constipation = c(
      "C17", "C18", "C19", "C20", "C21", "C451", "C48", "C51", "C52",
      "C53", "C54", "C55", "C56", "C57", "C58", "C60", "C61", "C62",
      "C63", "C64", "C65", "C66", "C67", "C68", "C785", "C786",
      "D017", "D019", "D371", "D372", "D373", "D374", "D375",
      "K50", "K51", "K660", "N736", "N994"
    ),
    # These are to be excluded C17-C21, C45.1, C48, C51-C58, C60-C68, C78.5-C78.6, D01.7, D01.9, D37.1-D37.5, K50-K51, K66.0, N73.6, N99.4 (K56.6 if R10.1), and any CCPx surgery listed in claims
    stroke = NULL
  )
}

#' @keywords internal
.multimorbidity_years <- function() {
  list(
    alcohol_misuse = 2,
    asthma = 2,
    afib = 2,
    cancer_lymphoma = 2,
    cancer_metastatic = 2,
    cancer_nonmetastatic = 2,
    chf = 2,
    ckd = 1,
    cpain = NULL, # Here there's a different requirement, e.g. two or more codes at least 30 days apart
    cpd = 2,
    cvhepatitis = 6 / 12, # 6 months
    cirrhosis1 = NULL,
    cirrhosis2 = NULL,
    dementia = 2,
    depression = 2,
    diabetes = 2,
    epilepsy = 2,
    hypertension = 2,
    hypothyroidism = 2,
    ibd = 3,
    ibs = 2,
    multiple_sclerosis = 3,
    mi = NULL,
    parkinson = NULL,
    pud = 2,
    pvd = NULL,
    psoriasis = NULL,
    rheum_arthritis = 2,
    schizofrenia = 2,
    severe_constipation = 2,
    stroke = NULL
  )
}

#' @keywords internal
.multimorbidity_order <- function() {
  c(
    "alcohol_misuse",
    "asthma",
    "afib",
    "cancer_lymphoma",
    "cancer_metastatic",
    "cancer_nonmetastatic",
    "chf",
    "ckd",
    "cpain",
    "cpd",
    "cvhepatitis",
    "cirrhosis",
    "dementia",
    "depression",
    "diabetes",
    "epilepsy",
    "hypertension",
    "hypothyroidism",
    "ibd",
    "ibs",
    "multiple_sclerosis",
    "mi",
    "parkinson",
    "pud",
    "pvd",
    "psoriasis",
    "rheum_arthritis",
    "schizofrenia",
    "severe_constipation",
    "stroke"
  )
}

#' @keywords internal
.multimorbidity_howmany <- function() {
  list(
    alcohol_misuse = c("hospitalisations" = 1, "claims" = 2),
    asthma = c("hospitalisations" = 1, "claims" = NA),
    afib = c("hospitalisations" = 1, "claims" = 2),
    cancer_lymphoma = c("hospitalisations" = 1, "claims" = 2),
    cancer_metastatic = c("hospitalisations" = 1, "claims" = 2),
    cancer_nonmetastatic = c("hospitalisations" = 1, "claims" = 2),
    chf = c("hospitalisations" = 1, "claims" = 2),
    ckd = c("hospitalisations" = 1, "claims" = 3),
    cpain = c("hospitalisations" = 2, "claims" = 2),
    cpd = c("hospitalisations" = 1, "claims" = 2),
    cvhepatitis = c("hospitalisations" = 2, "claims" = 2),
    cirrhosis1 = c("hospitalisations" = 1, "claims" = 2),
    cirrhosis2 = c("hospitalisations" = 1, "claims" = 2),
    dementia = c("hospitalisations" = 1, "claims" = 2),
    depression = c("hospitalisations" = 1, "claims" = 2),
    diabetes = c("hospitalisations" = 1, "claims" = 2),
    epilepsy = c("hospitalisations" = 1, "claims" = 2),
    hypertension = c("hospitalisations" = 1, "claims" = 2),
    hypothyroidism = c("hospitalisations" = 1, "claims" = 2),
    ibd = c("hospitalisations" = 2, "claims" = 2),
    ibs = c("hospitalisations" = 1, "claims" = 2),
    multiple_sclerosis = c("hospitalisations" = 2, "claims" = 2),
    mi = c("hospitalisations" = 1, "claims" = NA),
    parkinson = c("hospitalisations" = 1, "claims" = 1),
    pud = c("hospitalisations" = 1, "claims" = 2),
    pvd = c("hospitalisations" = 1, "claims" = 1),
    psoriasis = c("hospitalisations" = 1, "claims" = 1),
    rheum_arthritis = c("hospitalisations" = 1, "claims" = 2),
    schizofrenia = c("hospitalisations" = 1, "claims" = 2),
    severe_constipation = c("hospitalisations" = 1, "claims" = 2),
    stroke = c("hospitalisations" = 1, "claims" = 1)
  )
}


#' @keywords internal
.collapse_codes <- function(x) paste0("^", paste(x, collapse = "|^"))
