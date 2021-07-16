#' Calculate multimorbidity domains
#'
#' @description Calculate 30 morbidity domains corresponding to chronic conditions, according to the definition of Tonelli et al. (see reference below).
#'
#' @param data Dataset used for data input. Must be in long format and contain a column with subject IDs, ICD-10 codes, dates at which each code was recorded, and an index date at which the conditions are calculated.
#' @param id Name of the column identifying subject IDs in `data`.
#' @param code Name of the column identifying ICD-10 codes in `data`.
#' @param date Name of the column identifying dates at which codes are recorded in `data`.
#' @param index_date Name of the column identifying index date in `data`.
#' @param verbose Print out completed steps, which might be useful with big datasets to track progress.
#'
#' @return A dataset with a row per individual.
#'
#' @references Tonelli, M., Wiebe, N., Fortin, M. et al. _Methods for identifying 30 chronic conditions: application to administrative data._ BMC Med Inform Decis Mak 15, 31 (2016). \doi{10.1186/s12911-015-0155-5}.
#'
#' @export
#'
#' @examples
#' data("icd10", package = "SCREAM")
#' multimorbidity(
#'   data = icd10[icd10$id %in% seq(5), ],
#'   id = "id",
#'   code = "code",
#'   date = "date",
#'   index_date = "index_date",
#'   verbose = TRUE
#' )
multimorbidity <- function(data, id, code, date, index_date, verbose = FALSE) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # x must be a data.frame (or a data.table)
  checkmate::assert_true(x = all(class(data) %in% c("data.frame", "data.table", "tbl", "tbl_df")), add = arg_checks)
  # id, code, date, index_date must be a single string value
  checkmate::assert_string(x = id, add = arg_checks)
  checkmate::assert_string(x = code, add = arg_checks)
  checkmate::assert_string(x = date, add = arg_checks)
  checkmate::assert_string(x = index_date, add = arg_checks)
  # verbose must be a boolean
  checkmate::assert_logical(x = verbose, len = 1, add = arg_checks)
  # id, code, date, index_date must be in x
  checkmate::assert_subset(x = id, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = code, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = date, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = index_date, choices = names(data), add = arg_checks)
  # Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Tidy codes
  data <- comorbidity:::.tidy(x = data, code = code)
  if (verbose) usethis::ui_done("Tidied codes...")

  ### Turn x into a DT
  data.table::setDT(data)

  ### Save a vector of IDs
  IDs <- data[[id]]

  ### Keep only codes that can be used somewhere
  allc <- c(.multimorbidity_codes(), .multimorbidity_exclusions())
  idx <- vapply(X = allc, FUN = function(x) !is.null(x), FUN.VALUE = logical(length = 1L))
  allc <- allc[idx]
  allc <- lapply(X = allc, FUN = .collapse_codes)
  allc <- paste(allc, collapse = "|")
  mv <- c(id, index_date)
  safetydf <- data[, ..mv]
  safetydf <- unique(safetydf)
  data <- data[stringi::stri_detect_regex(str = code, pattern = allc), ]
  data <- merge(data, safetydf, all.y = TRUE, allow.cartesian = TRUE, by = c(id, index_date))
  data.table::set(data, which(is.na(data[[code]])), code, ".NOTACODE!")
  data[[date]][is.na(data[[date]])] <- data[[index_date]][is.na(data[[date]])]
  if (verbose) usethis::ui_done("Subset only relevant codes...")

  ### Apply permanence of codes
  data <- data[(date) <= (index_date), ]
  data[, ...yd := (index_date) - (date)]
  data[, ...yd := as.numeric(...yd) / 365.242]
  data[, ...target := NA]
  for (k in seq_along(.multimorbidity_codes())) {
    if (!isTRUE(.multimorbidity_permanent()[[k]])) {
      idx <- grep(pattern = .collapse_codes(x = .multimorbidity_codes()[[k]]), x = data[[code]])
      data$...target[idx] <- .multimorbidity_permanent()[[k]]
    }
  }
  data <- data[is.na(...target) | ...yd <= ...target]
  data[, ...yd := NULL]
  data[, ...target := NULL]
  if (verbose) usethis::ui_done("Applied permanence of codes...")

  ### Subset only 'id' and 'code' columns
  mv <- c(id, code)
  data <- data[, ..mv]

  ### Apply all regex
  for (k in seq_along(.multimorbidity_codes())) {
    cds <- .collapse_codes(x = .multimorbidity_codes()[[k]])
    data[, (names(.multimorbidity_codes())[k]) := stringi::stri_detect_regex(str = code, pattern = cds)]
    if (verbose) {
      if (names(.multimorbidity_codes())[k] == "cirrhosis1") {
      } else if (names(.multimorbidity_codes())[k] == "cirrhosis2") {
        usethis::ui_done("Done with cirrhosis...")
      } else {
        usethis::ui_done("Done with {names(.multimorbidity_codes())[k]}...")
      }
    }
  }

  ### Summarise by 'id'
  out <- lapply(X = names(.multimorbidity_codes()), FUN = function(n) {
    # Use the following line instead to count the number of matches:
    # data[, stats::setNames(list(sum(get(n))), n), by = id]
    data[, stats::setNames(list(as.numeric(sum(get(n)) > 0)), n), by = id]
  })
  out <- Reduce(function(...) merge(..., all = T, by = id), out)
  data.table::setorderv(out, cols = id)

  ### Process exclusions
  mv <- c(id, code)
  data <- data[, ..mv]
  for (k in seq_along(.multimorbidity_exclusions())) {
    if (!is.null(.multimorbidity_exclusions()[[k]])) {
      cds <- .collapse_codes(x = .multimorbidity_exclusions()[[k]])
      data[, (names(.multimorbidity_codes())[k]) := grepl(pattern = cds, x = code)]
    }
  }
  excl <- lapply(X = names(.multimorbidity_exclusions()), FUN = function(n) {
    if (!is.null(.multimorbidity_exclusions()[[n]])) {
      data[, stats::setNames(list(sum(get(n))), n), by = id]
    }
  })
  excl <- excl[!vapply(X = excl, FUN = is.null, FUN.VALUE = logical(length = 1L))]
  excl <- Reduce(function(...) merge(..., all = T, by = id), excl)
  data.table::setorderv(excl, cols = id)
  for (n in names(excl)) {
    if (n != id) {
      excl[, (n) := ifelse(get(n) > 0, 1, 0)]
    }
  }
  # ->
  out[, ibs := ibs * (1 - excl$ibs)]
  out[, severe_constipation := severe_constipation * (1 - excl$severe_constipation)]
  if (verbose) usethis::ui_done("Applied exclusions...")

  ### Return
  if (verbose) usethis::ui_done("Done!")
  return(out)
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
    rheum_artritis = c("M05", "M06", "M315", "M32", "M33", "M34", "M351", "M353", "M360"),
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
    rheum_artritis = NULL,
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
.multimorbidity_permanent <- function() {
  list(
    alcohol_misuse = TRUE,
    asthma = TRUE,
    afib = TRUE,
    cancer_lymphoma = 5,
    cancer_metastatic = 5,
    cancer_nonmetastatic = 5,
    chf = TRUE,
    ckd = TRUE,
    cpain = 2,
    cpd = TRUE,
    cvhepatitis = TRUE,
    cirrhosis1 = TRUE,
    cirrhosis2 = TRUE,
    dementia = TRUE,
    depression = 2,
    diabetes = TRUE,
    epilepsy = TRUE,
    hypertension = TRUE,
    hypothyroidism = TRUE,
    ibd = TRUE,
    ibs = TRUE,
    multiple_sclerosis = TRUE,
    mi = TRUE,
    parkinson = TRUE,
    pud = 2,
    pvd = TRUE,
    psoriasis = TRUE,
    rheum_artritis = TRUE,
    schizofrenia = TRUE,
    severe_constipation = 2,
    stroke = TRUE
  )
}

#' @keywords internal
.collapse_codes <- function(x) paste0("^", paste(x, collapse = "|^"))
