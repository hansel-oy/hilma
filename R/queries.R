#' Helper functions for constructing the queries
#'
#' Functions for constructing the `query_params`-argument of the
#' [fetch_notices()]-function. Functions suffixed with `_filter` and `_search`
#' use the respective functionalities of the
#' [Azure Cognitive Search](https://docs.microsoft.com/en-us/azure/search/query-lucene-syntax).
#'
#' `query_and` and `_or` can be used to combine queries (see example below).
#'
#' @return A list containing the query that can be passed to [fetch_notices()].
#'
#' @examples
#' \donttest{
#' # A query that returns all notices modified 2020-01-01 or later
#' # from a certain organisation
#' q <- organisation_id_filter("1234567-8") %>%
#' query_and(all_notices_modified_since_filter(lubridate::ymd("2020-01-01")))
#' fetch_notices(q, api_key)
#' }
#' @name query-helpers
NULL

#' Combine queries
#'
#' Combines two queries with filters and searches using a predicate within the
#' filter and/or search.
#'
#' @param q1 The second query
#' @param q2 The first query
#' @param pred The predicate used for combining the queries.
#'
#' @return A list containing the new query.
query_combine <- function(q1, q2, pred) {
  pred <- str_c(" ", pred, " ")
  list(search = str_c(q1$search, q2$search, sep = toupper(pred)),
       filter = str_c(q1$filter, q2$filter, sep = pred)) %>%
    compact()
}

#' @rdname query-helpers
#' @param q1 First query
#' @param q2 Second query
#' @export
query_and <- function(q1, q2) {
  query_combine(q1, q2, "and")
}

#' @rdname query-helpers
#' @param q1 First query
#' @param q2 Second query
#' @export
query_or <- function(q1, q2) {
  res <- query_combine(q1, q2, "or")
  if (!is.null(res$search) && !is.null(res$filter)) {
    warning ("Search and filter are always combined using the and-predicate.")
  }
  res
}


#' A generic filter for boolean value
#'
#' Searches the `variable` for an exact match of the `value`.
#'
#' @param variable The name of the variable
#' @param is_true Whether to include the notices that match the filter or not.
#'
#' @return A list containing the query that can be passed to [fetch_notices()].
generic_boolean_filter <- function(variable, is_true) {
  list(filter = glue("({if(is_true) '' else 'not '}{variable})"))
}

#' @rdname query-helpers
#' @param is_dps Whether to filter notices that are for DPSs or notices that
#' are not for DPSs.
#' @export
dps_filter <- function(is_dps = TRUE) {
  generic_boolean_filter("includesDynamicPurcharingSystem", is_dps)
}

#' @rdname query-helpers
#' @param is_fa Whether to filter notices that are for FAs or notices that
#' are not for FAs.
#' @export
framework_agreement_filter <- function(is_fa = TRUE) {
  generic_boolean_filter("includesFrameworkAgreement", is_fa)
}

#' @rdname query-helpers
#' @param is_national Whether to filter notices that are for FAs or notices that
#' are not for FAs.
#' @export
is_national_filter <- function(is_national = TRUE) {
  generic_boolean_filter("isNationalProcurement", is_national)
}


#' A generic filter for maching a variable with a value
#'
#' The `variable` should `pred` the `value` (e.g. `name eq Hansel`).
#'
#' @param variable The name of the variable
#' @param pred The predicate that is used for filtering
#' @param value The value of the varbiable
#'
#' @return A list containing the query that can be passed to [fetch_notices()]
generic_pred_filter <- function(variable, pred, value) {
  list(filter = glue("({variable} {pred} {value})"))
}

#' @rdname query-helpers
#' @param notice_number The notice number. E.g. `"2020-000001"`.
#' @export
notice_number_filter <- function(notice_number) {
  generic_pred_filter("noticeNumber", "eq", notice_number)
}

#' @rdname query-helpers
#' @param id The (internal) id of the notice. E.g. `1L`.
#' @export
id_filter <- function(id) {
  generic_pred_filter("id", "eq", id)
}

#' @rdname query-helpers
#' @param since A [date]-object. Includes the boundary and defaults to yesterday.
#' @export
all_notices_modified_since_filter <- function(since = today() %m-% days(1)) {
  if (!is.Date(since)) stop ("\"since\" must be a date object or NULL.")

  generic_pred_filter("dateModified", "ge", since)
}

#' @rdname query-helpers
#' @param until A [date]-object. Includes the boundary and defaults to today.
#' @export
all_notices_modified_until_filter <- function(until = today()) {
  if (!is.Date(until)) stop ("\"until\" must be a date object or NULL.")

  generic_pred_filter("dateModified", "le", until)
}

#' @rdname query-helpers
#' @param since A [date]-object. Includes the boundary and defaults to yesterday.
#' @export
all_notices_published_since_filter <- function(since = today() %m-% days(1)) {
  if (!is.Date(since)) stop ("\"since\" must be a date object or NULL.")

  generic_pred_filter("datePublished", "ge", since)
}

#' @rdname query-helpers
#' @param until A [date]-object. Includes the boundary and defaults to today.
#' @export
all_notices_published_until_filter <- function(until = today()) {
  if (!is.Date(until)) stop ("\"until\" must be a date object or NULL.")

  generic_pred_filter("datePublished", "le", until)
}


#' A generic search for maching a variable with a value
#'
#' Searches the `variable` for an exact match of the `value`.
#'
#' @param variable The name of the variable
#' @param value The value of the varbiable
#'
#' @return A list containing the query that can be passed to [fetch_notices()].
generic_search <- function(variable, value) {
  list(search = glue("{variable}:\"{value}\""))
}

#' @rdname query-helpers
#' @param name The name of the procuring organisation. E.g. `"Hansel Oy"`.
#' @export
organisation_name_search <- function(name) {
  generic_search("organisationName", name)
}

#' @rdname query-helpers
#' @param organisation_id The national registration id of the procuring organisation.
#' E.g. `"1234567-8"`.
#' @export
organisation_id_search <- function(organisation_id) {
  generic_search("organisationNationalRegistrationNumber", organisation_id)
}

