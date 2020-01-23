context("process")

test_that("process_notice_tibble works even when not all date columns are present", {
  date_str <- "2011-01-01T12:12:12.013"
  tbl_proc1 <- tibble(id = "1", datePublished = date_str) %>%
    process_notice_tibble()
  tbl_res1 <- tibble(id = 1L, datePublished = ymd_hms(date_str, tz = "Europe/Helsinki"))
  expect_identical(tbl_proc1, tbl_res1)
  # works with other fields as well
  tbl_proc2 <- tibble(id = c("1337", "42"),
                      dateModified = c(date_str, date_str),
                      tendersOrRequestsToParticipateDueDateTime = c(date_str, date_str)) %>%
    process_notice_tibble()
  tbl_res2 <- tibble(id = c(1337L, 42L),
                     dateModified = ymd_hms(c(date_str, date_str), tz = "Europe/Helsinki")) %>%
    mutate(tendersOrRequestsToParticipateDueDateTime = dateModified)
  expect_identical(tbl_proc2, tbl_res2)
})
