test_that("ICD data is prep'd properly", {
  for (i in seq(50)) {
    a <- data.frame(.tmp = 1)
    .id <- .sample_name()
    .code <- .sample_name()
    .date <- .sample_name()
    .index_date <- .sample_name()
    a[[.id]] <- 1
    a[[.code]] <- "E10"
    a[[.date]] <- Sys.Date() - 1
    a[[.index_date]] <- Sys.Date()
    b <- .multimorbidity_prep_data(data = a, id = .id, code = .code, date = .date, index_date = .index_date)
    testthat::expect_equal(object = names(b), expected = c("id", "code", "date", "index_date"))
    testthat::expect_equal(object = nrow(b), expected = 1)
    testthat::expect_equal(object = ncol(b), expected = 4)
    testthat::expect_true(object = ncol(a) > ncol(b))
  }
})

test_that("Drugs data is prep'd properly", {
  data("atc", package = "SCREAM")
  for (i in seq(50)) {
    a <- data.frame(.tmp = 1)
    .id <- .sample_name()
    .atc <- .sample_name()
    .npacks <- .sample_name()
    .date <- .sample_name()
    .index_date <- .sample_name()
    a[[.id]] <- 1
    a[[.atc]] <- sample(x = atc$ATC.Code, size = 1)
    a[[.npacks]] <- 1
    a[[.date]] <- Sys.Date() - 1
    a[[.index_date]] <- Sys.Date()
    b <- .multimorbidity_prep_drugs(data = a, id = .id, atc = .atc, npacks = .npacks, date = .date, index_date = .index_date)
    testthat::expect_equal(object = names(b), expected = c("id", "atc", "npacks", "date", "index_date"))
    testthat::expect_equal(object = nrow(b), expected = 1)
    testthat::expect_equal(object = ncol(b), expected = 5)
    testthat::expect_true(object = ncol(a) > ncol(b))
  }
})
