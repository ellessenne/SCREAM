testthat::test_that("CKD-EPI equation returns correct results", {
  # Comparing this with results from https://www.kidney.org/professionals/kdoqi/gfr_calculator

  testthat::expect_equal(
    object = floor(ckd_epi(creatinine = 50, age = 40, female = 0)),
    expected = 129
  )
  testthat::expect_equal(
    object = floor(ckd_epi(creatinine = 50, age = 40, female = 1)),
    expected = 116
  )

  testthat::expect_equal(
    object = floor(ckd_epi(creatinine = 30, age = 50, female = 0)),
    expected = 148
  )
  testthat::expect_equal(
    object = floor(ckd_epi(creatinine = 30, age = 50, female = 1)),
    expected = 128
  )

  testthat::expect_equal(
    object = floor(ckd_epi(creatinine = 60, age = 60, female = 0)),
    expected = 104
  )
  testthat::expect_equal(
    object = floor(ckd_epi(creatinine = 60, age = 60, female = 1)),
    expected = 95
  )

  testthat::expect_equal(
    object = floor(ckd_epi(creatinine = 100, age = 70, female = 0)),
    expected = 65
  )
  testthat::expect_equal(
    object = floor(ckd_epi(creatinine = 100, age = 70, female = 1)),
    expected = 49
  )
})
