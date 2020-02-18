#' Fetch the data from the API
#'
#' Fetches the data given the query parameters.
#'
#' @param skip How many notices should be skipped from the beginning. The notices
#' are arranged by `id` before the skipping.
#' @param query The query parameters. Can be constructed with [query-helpers].
#' @param headers The headers of the request. Constructed with [construct_headers()].
#' @param verbose Whether to print the progress or not.
#' @param api_url Url to the hilma_api.
#' @param n_fetch The (maximum) number of notices to fetch. Defaults to `1000L`
#' which is the limit of the API.
#' @param delay Delay in seconds after the query.
#'
#' @return The `value`-field of the result in a tibble.
fetch_hilma_data <- function(skip, query, headers, verbose,
                             api_url, n_fetch = 1000L, delay) {
  body <- c(query, list(orderby = "id", top = n_fetch, skip = skip))

  if (verbose) print(glue("Iteration {iter}/{iters}."))
  res <- POST(api_url, body = body, encode = "json", config = headers) %>%
    handle_errors() %>% # check if there are errors
    content(as = "parsed", simplifyDataFrame = TRUE) %>%
    pluck("value") %>%
    as_tibble()
  Sys.sleep(delay)
  res
}


#' Fetch the number of results
#'
#' Similar to [fetch_hilma_data()] but returns the number of results rather
#' than the result itself.
#'
#' @param query The query parameters. Can be constructed with [query-helpers].
#' @param headers The headers of the request. Constructed with [construct_headers()].
#' @param api_url Url to the hilma_api.
#'
#' @return An integer, the number of results.
fetch_number_of_results <- function(query, headers, api_url) {
  n_results_body <- c(query, list(top = 0L, count = TRUE))
  POST(api_url, body = n_results_body, encode = "json", config = headers) %>%
    handle_errors() %>% # check if there are errors
    content(as = "parsed") %>%
    pluck("@odata.count")
}


#' Fetch notices from the Hilma API
#'
#' Fetches the notices specified in the [`query`][query-helpers]-argument
#' from the API and formats the result in a tibble. Errors if the results changes
#' (ie. someone publishes a notice that matches the `query`) during the
#' execution.
#'
#' @param query The query parameters as a list that will be encoded as
#' JSON with [httr::POST()] using `encode = "json"`. Can be constructed with
#' [query-helpers].
#' @param api_key Your personal API-key. Can be obtained from
#' <https://hns-hilma-prod-apim.portal.azure-api.net>.
#' @param include_search_score Whether to return the search score (ie. how well
#' the result matches the query) or not. Defaults to `FALSE`.
#' @param verbose Whether to print the progress or not. Defaults to `FALSE`.
#' @param delay Delay between the fetches in seconds if the query needs to be
#' split into multiple fetches. Defaults to `1`.
#'
#' @return A tibble that contains the result of the query.
#' @export
fetch_notices <- function(query,
                          api_key,
                          include_search_score = FALSE,
                          verbose = FALSE,
                          delay = 1) {

  api_url <- "https://api.hankintailmoitukset.fi/avp/notices/docs/search"
  headers <- construct_headers(api_key)

  # fetch the number of notices
  n_notices <- fetch_number_of_results(query, headers, api_url)

  batch_size <- 1000L
  skips <- split_into_chunks(n_notices, batch_size = batch_size)
  if (verbose) {
    print(glue("Fetching {n_notices} notices in {length(skips)} iterations:"))
  }

  results <- skips %>%
    map_df(fetch_hilma_data, query = query, headers = headers,
           verbose = verbose, api_url = api_url, n_fetch = batch_size, delay = delay)

  # if there are not results that match the query, the api doesnt return anything
  # -> set the result to be a tibble with correct columns but 0 rows
  if (nrow(results) == 0L) results <- empty_tbl_raw

  # Checking that the number of results has not changed during the loop
  # This would cause problems if the IDs are not assigned in ascending order
  # or if the number of notices has increased above a multiple of 1000
  # (e.g. from 1999 to 2001) which means that the loop would not fetch the
  # last two notices
  n_notices_new <- fetch_number_of_results(query, headers, api_url)

  if (n_notices_new != n_notices) {
    stop ("The number of notices changed during the execution, please try again.")
  }

  res_tbl <- process_notice_tibble(results)

  if (nrow(res_tbl) != n_notices_new) {
    stop (glue("Something went wrong. The number of notices fetched ({nrow(res_tbl)})",
               " does not equal the number of notices expected ({n_notices})."))
  }

  if (include_search_score) res_tbl else select(res_tbl, -matches("@search.score"))
}

#' Fetch api documentation
#'
#' Fetches the the fields available from the api.
#'
#' @param api_key Your personal API-key. Can be obtained from
#' <https://hns-hilma-prod-apim.portal.azure-api.net>.
#'
#' @return A tibble with three columns:
#' * `name`: The name of the field
#' * `type`: The type of the field
#' * `searchable`: Whether the field is searchable or not
#' * `filterable`: Whether the field is filterable or not
#'
#' @export
fetch_fields <- function(api_key) {
  headers <- construct_headers(api_key)
  api_url <- "https://api.hankintailmoitukset.fi/avp/notices"

  GET(api_url, config = headers) %>%
    handle_errors() %>% # check if there are errors
    content(as = "parsed", simplifyDataFrame = TRUE) %>%
    pluck("fields") %>%
    as_tibble() %>%
    select("name", "type", "searchable", "filterable")
}
