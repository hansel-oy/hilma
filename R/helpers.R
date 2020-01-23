#' Construct the headers for the query
#'
#' Creates the header needed for a [httr::GET()]/[httr::POST()]-request.
#'
#' @param api_key The API-key for Hilma.
#'
#' @return An object for the `config`-argument of the [httr::GET()]/[httr::POST()]-function.
construct_headers <- function(api_key) {
  add_headers(`Ocp-Apim-Subscription-Key` = api_key)
}


#' Split the fetch into multiple chunks
#'
#' A helper function for splitting the fetch into chunks that the api accepts.
#'
#' @param n_notices The number of notices to be fetched
#' @param batch_size The desired batch size, the current default is `1000`,
#' which is the limit of the current API.
#'
#' @return An integer vector that contains the number of elements to
#' skip in each query.
split_into_chunks <- function(n_notices, batch_size = 1000L) {
  # if 0 notices, no fetches
  if (n_notices <= 0L) return(integer(0))
  # fetch at most {bach_size} notices at a time
  n_fetches <- ceiling(n_notices/batch_size)
  # array for the number of notices to skip on each fetch, starting from 0
  ((1 : n_fetches) - 1L) * batch_size
}

handle_errors <- function(result) {
  if (http_error(result)) {
    error_msg <- content(result, as = "parsed") %>% flatten() %>% pluck("message")
    status <- http_status(result) %>% pluck("message")
    stop (glue("HTTP query returned with {status}:", error_msg, .sep = "\n"))
  }

  result
}
