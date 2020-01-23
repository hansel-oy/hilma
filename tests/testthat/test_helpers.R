context("helpers")

test_that("construct_headers constructs an object with the correct api_key", {
  headers <- construct_headers("apikey")
  expect_equal(length(headers$headers), 1L)
  expect_equal(headers$headers[[1]], "apikey")
})

test_that("split_into_chunks handles numbers close to a multiple of 1000 correctly", {
  expect_equal(split_into_chunks(9999L), (0:9)*1000L)
  expect_equal(split_into_chunks(10000L), (0:9)*1000L)
  expect_equal(split_into_chunks(10001L), (0:10)*1000L)
})

test_that("split_into_chunks handles 0 chunks correctly", {
  expect_equal(split_into_chunks(0), integer(0))
})
