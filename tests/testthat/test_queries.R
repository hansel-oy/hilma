context("queries")

test_that("generic filter returns the expected result", {
  gf <- generic_pred_filter("var", "eq", "val1")
  expect_identical(gf, list(filter = glue("(var eq val1)")))
})

test_that("generic search returns the expected result", {
  gf <- generic_search("var", "val1 must match")
  expect_identical(gf, list(search = glue("var:\"val1 must match\"")))
})

test_that("organisation_name_search works as expected", {
  expect_identical(organisation_name_search("Hansel Oy"),
                   list(search = glue("organisationName:\"Hansel Oy\"")))
})
test_that("all_notices_modified_since_filter works as expected", {
  expect_identical(all_notices_modified_since_filter(ymd("2019-01-01")),
                   list(filter = glue("(dateModified ge {ymd('2019-01-01')})")))
})

test_that("all_notices_published_until_filter works as expected", {
  expect_identical(all_notices_published_until_filter(),
                   list(filter = glue('(datePublished le {lubridate::today()})')))
})


test_that("is_national_filter works as expected", {
  expect_identical(is_national_filter(TRUE),
                   list(filter = glue('(isNationalProcurement)')))
  expect_identical(is_national_filter(FALSE),
                   list(filter = glue('(not isNationalProcurement)')))
})

test_that("combining queries works as expected", {
  q1 <- is_national_filter(FALSE)
  q2 <- all_notices_modified_until_filter()
  q12 <- query_and(q1, q2)
  expect_equal(q12, list(filter = str_c(q1$filter, " and ", q2$filter)))
  q3 <- organisation_name_search("Hansel Oy")
  q4 <- organisation_name_search("Hanssel Oy")
  q34 <- query_or(q3, q4)
  expect_equal(q34, list(search = str_c(q3$search, " OR ", q4$search)))
  q_all <- query_and(q12, q34)
  expect_equal(q_all, list(search = q34$search, filter = q12$filter))
})
