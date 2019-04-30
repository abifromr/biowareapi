context("test-programs")

test_that("slimprints works", {
  res <- slimprints(uniprotid = 'P04637')

  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 1)
})
