#' Parse the model's JSON reply into an R list
#'
#' Converts the raw JSON string returned by [call_openai()] into a named R
#' list. The engine makes no assumptions about which keys are present —
#' validation of domain-specific fields (names, value ranges) belongs in the
#' calling script or downstream analysis, not here.
#'
#' @param raw Character scalar. The raw JSON string from [call_openai()].
#'
#' @return A named list of the fields the model returned.
#' @export
#'
#' @examples
#' parse_response('{"dimension_a": 3, "dimension_b": 1}')
parse_response <- function(raw) {
  stopifnot(is.character(raw), length(raw) == 1L)
  result <- jsonlite::fromJSON(raw, simplifyVector = FALSE)
  if (!is.list(result) || is.null(names(result)))
    stop("Model reply did not parse to a JSON object.")
  result
}
