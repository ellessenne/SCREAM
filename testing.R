devtools::load_all()
data("icd10", package = "SCREAM")
multimorbidity(
  data_hospitalisations = icd10$hospitalisations,
  data_claims = icd10$claims,
  id = "id",
  code = "code",
  date = "date",
  index_date = "index_date"
)
