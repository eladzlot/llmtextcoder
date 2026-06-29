#' Print a compiled prompt to the console
#'
#' Builds a prompt from a template and text, then prints it with a header
#' showing character and word counts. Use this *before* making any API call
#' to verify that the participant text was injected correctly and the full
#' prompt reads as intended.
#'
#' @param template Character scalar. The template string (from [read_template()]).
#' @param text Character scalar. The participant text to inject.
#'
#' @return The compiled prompt string, invisibly.
#' @export
#'
#' @examples
#' template <- "Please rate this text on a 1-5 scale: {{text}}"
#' preview_prompt(template, "I can't stop thinking about yesterday's meeting.")
preview_prompt <- function(template, text) {
  prompt <- build_prompt(template, text)
  nchars <- nchar(prompt)
  nwords <- length(strsplit(trimws(prompt), "\\s+")[[1]])

  cat(strrep("-", 60), "\n")
  cat(sprintf("COMPILED PROMPT  |  %d chars  |  %d words\n", nchars, nwords))
  cat(strrep("-", 60), "\n")
  cat(prompt, "\n")
  cat(strrep("-", 60), "\n")

  invisible(prompt)
}

#' Pretty-print a model result to the console
#'
#' Displays both the raw JSON string and the parsed scores side by side.
#' Use after [score_one()] to inspect the model's output before committing
#' to a full batch run.
#'
#' @param raw Character scalar. The raw JSON string from [call_openai()].
#' @param scores Named list. The parsed scores from [parse_response()].
#'
#' @return `scores`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- score_one("prompts/rubric_v1.txt", "Some participant text.", run_params())
#' print_result(result$raw, result$scores)
#' }
print_result <- function(raw, scores) {
  cat(strrep("-", 60), "\n")
  cat("RAW JSON\n")
  cat(strrep("-", 60), "\n")
  cat(raw, "\n\n")

  cat(strrep("-", 60), "\n")
  cat("PARSED SCORES\n")
  cat(strrep("-", 60), "\n")
  for (key in names(scores)) {
    cat(sprintf("  %-25s %s\n", key, scores[[key]]))
  }
  cat(strrep("-", 60), "\n")

  invisible(scores)
}
