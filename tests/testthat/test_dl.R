context("dl")

test_that("Invalid apikey is handled correctly", {
  expect_error(fetch_notices(list(), "not_valid_apikey"),
               "invalid subscription key")
})
