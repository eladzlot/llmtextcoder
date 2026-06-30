#' Extract placeholder names from a template
#'
#' @noRd
.template_placeholders <- function(template) {
  m <- gregexpr("\\{\\{([^}]+)\\}\\}", template, perl = TRUE)
  if (m[[1]][1L] == -1L) return(character(0))
  unique(gsub("\\{\\{|\\}\\}", "", regmatches(template, m)[[1]]))
}

#' Build a prompt by injecting data into a template
#'
#' Reads a template containing `{{placeholder}}` markers and substitutes each
#' one from the named list `data`. All placeholders present in the template
#' must have a matching name in `data`.
#'
#' @param template Character scalar. The full template string. Load from disk
#'   with [read_template()].
#' @param data Named list (or named character vector). Values to inject,
#'   one element per `{{placeholder}}` in the template.
#'
#' @return Character scalar: the compiled prompt with all placeholders replaced.
#' @export
#'
#' @examples
#' template <- "Rate this response on clarity: {{response}}"
#' build_prompt(template, list(response = "I keep thinking about what went wrong."))
build_prompt <- function(template, data) {
  if (!is.character(template) || length(template) != 1L)
    stop(sprintf(
      "'template' must be a single character string.\n  Got %s of length %d.\n  Load your template with read_template(\"prompts/rubric_v1.txt\").",
      class(template)[1L], length(template)
    ))

  if (is.character(data)) data <- as.list(data)
  if (!is.list(data))
    stop(sprintf(
      "'data' must be a named list with one element per {{placeholder}} in the template.\n  Got %s.\n  Example: list(text = \"participant response here\")",
      class(data)[1L]
    ))

  placeholders <- .template_placeholders(template)
  if (length(placeholders) == 0L)
    stop(paste0(
      "The template contains no {{placeholder}} patterns — nothing to inject.\n",
      "  Placeholders are written as {{column_name}} and mark where participant data\n",
      "  is inserted. Example: \"Please rate the following text:\\n{{text}}\""
    ))

  missing <- setdiff(placeholders, names(data))
  if (length(missing) > 0L)
    stop(sprintf(
      "The template requires placeholder(s) not found in 'data': %s\n  Template placeholders : %s\n  'data' names provided : %s",
      paste(sprintf("{{%s}}", missing), collapse = ", "),
      paste(sprintf("{{%s}}", placeholders), collapse = ", "),
      if (length(names(data)) == 0L) "(none — list has no names)"
      else paste(names(data), collapse = ", ")
    ))

  result <- template
  for (nm in placeholders)
    result <- gsub(sprintf("{{%s}}", nm), data[[nm]], result, fixed = TRUE)
  result
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
  if (!is.character(path) || length(path) != 1L || !nzchar(path))
    stop("'path' must be a non-empty character string pointing to a .txt rubric file.")
  if (!file.exists(path))
    stop(sprintf(
      "Template file not found: \"%s\"\n  Check the file path (working directory is \"%s\").",
      path, getwd()
    ))
  paste(readLines(path, warn = FALSE), collapse = "\n")
}
