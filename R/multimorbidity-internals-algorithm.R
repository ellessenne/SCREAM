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
.multimorbidity_drugs_atc <- function() {
  list(
    cpain = c("N02B", "N02A", "N02C", "N02CC", "N02BA01", "N02BA51", "N02BA71", "N02CC05", "N02BA02", "N02BA10", "N02AE01", "N02BA03", "N02CX02", "N02AC01", "N02AC04", "N02AC54", "N02BA11", "N02AA08", "N02AA58", "N02CA01", "N02CC06", "N02CA02", "N02CA52", "N02AB03", "N02CC07", "N02AA03", "N02CA07", "N02AX05", "N02CA04", "N02AA01", "N02BA13", "N02AA09", "N02BG10", "N02AF02", "N02CC02", "N02BG06", "N02AA02", "N02AA10", "N02BE01", "N02BE51", "N02BE71", "N02AB02", "N02CX01", "N02CC04", "N02BA06", "N02BA04", "N02CC01", "N02AX06", "N02AX02", "N02AX52", "N02BG08", "N02CC03"),
    cpain_no_epilepsy = c("N03AF01", "N03AX12", "N03AX16"),
    depression = c("N06AF", "N06AX22", "N06AX12", "N06AB04", "N06AX21", "N06AB10", "N06AB03", "N06AB08", "N06AF05", "N06AF01", "N06AX03", "N06AX11", "N06AG02", "N06AX06", "N06AB05", "N06AF03", "N06AX18", "N06AB06", "N06AF04", "N06AX05", "N06AX02", "N06AX16", "N06AX09", "N06AX26")
  )
}

#' @keywords internal
.multimorbidity_drugs_years <- function() {
  list(
    cpain = 1,
    cpain_no_epilepsy = 1,
    depression = 1
  )
}

#' @keywords internal
.multimorbidity_drugs_howmany <- function() {
  list(
    cpain = 4,
    cpain_no_epilepsy = 4,
    depression = 4
  )
}
