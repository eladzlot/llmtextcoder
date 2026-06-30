#' Print a compiled prompt to the console
#'
#' Builds a prompt from a template and data, then prints it with a header
#' showing character count, word count, and approximate token count (~4 chars
#' per token). Use this *before* making any API call to verify that the
#' data was injected correctly and the full prompt reads as intended.
#'
#' @param template Character scalar. The template string (from [read_template()]).
#' @param data Named list. Values for each `{{placeholder}}` in the template.
#'
#' @return The compiled prompt string, invisibly.
#' @export
#'
#' @examples
#' template <- "Please rate this response on a 1-5 scale: {{response}}"
#' preview_prompt(template, list(response = "I can't stop thinking about yesterday's meeting."))
preview_prompt <- function(template, data) {
  prompt  <- build_prompt(template, data)
  nchars  <- nchar(prompt)
  nwords  <- length(strsplit(trimws(prompt), "\\s+")[[1]])
  ntokens <- round(nchars / 4)

  cat(strrep("-", 60), "\n")
  cat(sprintf("COMPILED PROMPT  |  %d chars  |  %d words  |  ~%d tokens\n",
              nchars, nwords, ntokens))
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
