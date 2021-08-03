testthat::test_that("The function to collapse codes and return a regex works as expected", {
  for (i in seq(50)) {
    A <- sample(x = LETTERS, size = 1)
    B <- sample(x = letters, size = 1)
    testthat::expect_equal(object = .collapse_codes(x = c(A, B)), expected = paste0("^", A, "|^", B))
  }
})
