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
  if (!is.character(raw) || length(raw) != 1L)
    stop(sprintf(
      "'raw' must be a single character string (the JSON returned by call_openai()), got %s.",
      class(raw)[1L]
    ))

  result <- tryCatch(
    jsonlite::fromJSON(raw, simplifyVector = FALSE),
    error = function(e) {
      stop(sprintf(
        "Model reply could not be parsed as JSON.\n  Raw reply (first 300 chars): %s\n  Tip: ensure your rubric template explicitly instructs the model to respond in JSON.",
        substr(raw, 1L, 300L)
      ), call. = FALSE)
    }
  )

  if (!is.list(result) || is.null(names(result)))
    stop(sprintf(
      "Model reply parsed as a JSON %s, but a JSON object with named fields is required.\n  Raw reply (first 300 chars): %s\n  Tip: instruct the model to return a JSON object, e.g. {\"score\": 3, \"rationale\": \"...\"}.",
      if (is.list(result)) "array" else class(result)[1L],
      substr(raw, 1L, 300L)
    ))

  result
}
