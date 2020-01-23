#' Process the data returned from the API
#'
#' Fixes the types of timestamp-strings to [date-times][DateTimeClasses] and
#' sets the type of `id` to integer.
#'
#' @param tbl_raw The unprocessed tibble returned from the API.
#'
#' @return A tibble with same dimensions and some times fixed.
process_notice_tibble <- function(tbl_raw) {
  ints <- "id"
  timestamps <- c("datePublished", "dateModified",
                  "tendersOrRequestsToParticipateDueDateTime")
  timestamps_in_data <- intersect(timestamps, colnames(tbl_raw))
  timestamp_fi <- function(ts) {
    ymd_hms(str_c(ts, "+02"), tz = "Europe/Helsinki", quiet = TRUE)
  }
  tbl_raw %>%
    mutate_at(ints, as.integer) %>%
    mutate_at(timestamps_in_data, timestamp_fi)
}
