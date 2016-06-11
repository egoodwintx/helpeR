context("as.numeric.com tests")
test_that("Test comma-string to numeric", {
  x = "1,010"
  y = 1010
  expect_equal(y, as.numeric.com(x))
})