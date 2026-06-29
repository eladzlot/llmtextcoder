#' Build a prompt by injecting participant text into a template
#'
#' Reads a template containing the literal placeholder `{{text}}` and
#' substitutes the participant text in its place. The template is the
#' scientific instrument — keep it in `prompts/` as a plain-text file,
#' versioned by name (e.g. `rubric_v1.txt`, `rubric_v2.txt`).
#'
#' @param template Character scalar. The full template string containing
#'   `{{text}}`. Load from disk with [read_template()].
#' @param text Character scalar. The participant text to evaluate.
#'
#' @return Character scalar: the compiled prompt with `{{text}}` replaced.
#' @export
#'
#' @examples
#' template <- "Please rate the following text: {{text}}"
#' build_prompt(template, "I keep thinking about what went wrong.")
build_prompt <- function(template, text) {
  stopifnot(
    is.character(template), length(template) == 1L,
    is.character(text),     length(text) == 1L
  )
  if (!grepl("{{text}}", template, fixed = TRUE)) {
    stop("Template does not contain the placeholder '{{text}}'.")
  }
  gsub("{{text}}", text, template, fixed = TRUE)
}

#' Read a rubric template from disk
#'
#' Reads a plain-text `.txt` file and returns its contents as a single
#' character string, suitable for passing to [build_prompt()].
#'
#' @param path Character scalar. Path to the template file (typically in
#'   `prompts/`).
#'
#' @return Character scalar: the file contents collapsed into one string.
#' @export
#'
#' @examples
#' \dontrun{
#' template <- read_template("prompts/rubric_v1.txt")
#' }
read_template <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}
