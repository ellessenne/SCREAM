### Test that codes are detected
for (mi in seq_along(.multimorbidity_codes())) {
  for (c in .multimorbidity_codes()[[mi]]) {
    df <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, code = c)
    out <- multimorbidity(data = df, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    w <- names(.multimorbidity_codes())[mi]
    testthat::expect_equal(object = out[[w]], expected = 1)
  }
}

### Codes are detected if variables have random names too
.sample_name <- function(length = 10) {
  s <- sample(x = c(letters, LETTERS, seq(9)), size = length, replace = TRUE)
  s <- paste(s, collapse = "")
  s <- make.names(s)
  s
}
for (mi in seq_along(.multimorbidity_codes())) {
  for (c in .multimorbidity_codes()[[mi]]) {
    df <- data.frame(.tmp = 1)
    .id <- .sample_name()
    .code <- .sample_name()
    .date <- .sample_name()
    .index_date <- .sample_name()
    df[[.id]] <- 1
    df[[.index_date]] <- Sys.Date()
    df[[.date]] <- Sys.Date() - 1
    df[[.code]] <- c
    out <- multimorbidity(data = df, id = .id, code = .code, date = .date, index_date = .index_date, combine_cirrhosis = FALSE)
    w <- names(.multimorbidity_codes())[mi]
    testthat::expect_equal(object = out[[w]], expected = 1)
  }
}
