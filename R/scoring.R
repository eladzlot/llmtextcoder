#' @noRd
.output_path <- function(template_path, output_dir = "data") {
  stem <- tools::file_path_sans_ext(basename(template_path))
  file.path(output_dir, paste0(stem, ".csv"))
}

#' @noRd
.error_path <- function(template_path, output_dir = "data") {
  sub("\\.csv$", "_errors.csv", .output_path(template_path, output_dir))
}

#' @noRd
.to_row <- function(id, text, raw, scores, prompt_version, params) {
  cbind(
    data.frame(id = id, text = text, stringsAsFactors = FALSE),
    as.data.frame(scores, stringsAsFactors = FALSE),
    data.frame(
      raw            = raw,
      prompt_version = prompt_version,
      model          = params$model,
      temperature    = params$temperature,
      scored_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      stringsAsFactors = FALSE
    )
  )
}

#' @noRd
.append_csv <- function(row, path) {
  write.table(row, file = path, append = file.exists(path),
              sep = ",", col.names = !file.exists(path),
              row.names = FALSE, qmethod = "double")
}

#' @noRd
.check_df <- function(df) {
  if (!is.data.frame(df))
    stop("`df` must be a data frame.")
  missing_cols <- setdiff(c("id", "text"), names(df))
  if (length(missing_cols) > 0)
    stop(sprintf("`df` is missing required column(s): %s",
                 paste(missing_cols, collapse = ", ")))
}

#' Score a single text against a rubric template
#'
#' A convenience wrapper for interactive and testing use. Reads the template,
#' builds the prompt, calls the API, and parses the response in one step.
#' Pass the result to [print_result()] to display it, or inspect `$raw` and
#' `$scores` directly.
#'
#' @param template_path Character scalar. Path to the `.txt` rubric template.
#' @param text Character scalar. The participant text to score.
#' @param params A `run_params` object (from [run_params()]).
#' @param api_key Character. OpenAI API key.
#'
#' @return A list with two elements:
#'   - `raw`: the raw JSON string returned by the model.
#'   - `scores`: the parsed named list of dimension scores.
#' @export
#'
#' @examples
#' \dontrun{
#' readRenviron(".env")
#' result <- score_one("rubric_v1.txt",
#'                     "I keep worrying about the same thing.",
#'                     run_params())
#' print_result(result$raw, result$scores)
#' }
score_one <- function(template_path, text, params = run_params(),
                      api_key = Sys.getenv("OPENAI_API_KEY")) {
  template <- read_template(template_path)
  prompt   <- build_prompt(template, text)
  raw      <- call_openai(prompt, params, api_key)
  scores   <- parse_response(raw)
  list(raw = raw, scores = scores)
}

#' Score a data frame of texts against a rubric, with incremental output
#'
#' Scores each row of `df` against the rubric template at `template_path`,
#' appending one row to the output CSV immediately after each API call. If
#' the run is interrupted, restart with the same call — already-scored rows
#' are skipped automatically.
#'
#' @section PII check:
#' By default, `score_many()` scans all texts for potentially identifying
#' information (emails, phone numbers, name disclosures, etc.) before any API
#' call is made. Flagged texts are written to `<output_dir>/<stem>_pii.csv`
#' and a summary is printed, but scoring proceeds regardless. Review the PII
#' file after the run (or run [scan_pii()] beforehand) and re-score a cleaned
#' data frame if needed. Set `pii_check = FALSE` once you have reviewed.
#'
#' @section Output files:
#' Results go to `<output_dir>/<stem>.csv` and failures to
#' `<output_dir>/<stem>_errors.csv`, where `<stem>` is the template filename
#' without its extension. Keeping failures separate means the main CSV is
#' always clean for analysis, and failed rows are automatically retried on
#' the next run (because they are absent from the main CSV).
#'
#' @section Skip logic:
#' A row is skipped if the same `id` **and** `model` already appear in the
#' output CSV. This means:
#' - Restarting an interrupted run picks up where it left off.
#' - Scoring with a different model appends new rows alongside the old ones,
#'   which is useful for model comparison studies.
#'
#' @section Partial runs:
#' Use `n` to score a small subset first (e.g. `n = 10`) to verify results
#' before committing to the full dataset. Run again without `n` to complete
#' the remaining rows.
#'
#' @param df A data frame with at minimum columns `id` (unique row identifier)
#'   and `text` (the participant text to score).
#' @param template_path Character scalar. Path to the `.txt` rubric template.
#' @param params A `run_params` object (from [run_params()]).
#' @param n Integer. Maximum number of *unscored* rows to process in this run.
#'   Default `Inf` scores all remaining rows.
#' @param output_dir Character scalar. Directory where the output CSV is
#'   written. Created automatically if it does not exist. Default `"data"`.
#' @param pii_check Logical. Scan all texts for PII before scoring. Flagged
#'   texts are written to `<stem>_pii.csv` and a warning is printed, but
#'   scoring proceeds. Set to `FALSE` once you have reviewed. Default `TRUE`.
#' @param pii_auto_patterns Named character vector of auto-redactable patterns
#'   passed to [scan_pii()]. Default: [auto_pii_patterns()].
#' @param pii_disclosure_patterns Named character vector of disclosure patterns
#'   passed to [scan_pii()]. Default: [disclosure_pii_patterns()].
#' @param api_key Character. OpenAI API key.
#'
#' @return The path to the output CSV, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' library(llmtextcoder)
#' readRenviron(".env")
#' df     <- read.csv("participants.csv")
#' params <- run_params(model = "gpt-4o", temperature = 0)
#'
#' # Pilot: sanity-check 10 rows first
#' score_many(df, "rubric_v1.txt", params, n = 10, output_dir = "results")
#'
#' # Full run — already-scored rows are skipped automatically
#' score_many(df, "rubric_v1.txt", params, output_dir = "results")
#'
#' # Skip PII check if you have already reviewed
#' score_many(df_clean, "rubric_v1.txt", params, pii_check = FALSE, output_dir = "results")
#' }
score_many <- function(df, template_path, params = run_params(), n = Inf,
                       output_dir = "data",
                       pii_check = TRUE,
                       pii_auto_patterns       = auto_pii_patterns(),
                       pii_disclosure_patterns = disclosure_pii_patterns(),
                       api_key = Sys.getenv("OPENAI_API_KEY")) {
  .check_df(df)

  if (pii_check) {
    pii_path <- sub("\\.csv$", "_pii.csv",
                    .output_path(template_path, output_dir))
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    pii_result <- scan_pii(df,
                           auto_patterns       = pii_auto_patterns,
                           disclosure_patterns = pii_disclosure_patterns)
    if (nrow(pii_result) > 0) {
      n_flagged <- length(unique(pii_result$id))
      warning(sprintf(
        "%d text(s) flagged for potential PII — review %s before sharing data.\n  Set pii_check = FALSE once reviewed.",
        n_flagged, pii_path
      ), call. = FALSE)
      write.csv(pii_result, pii_path, row.names = FALSE)
    }
  }

  template       <- read_template(template_path)
  prompt_version <- tools::file_path_sans_ext(basename(template_path))
  out_path       <- .output_path(template_path, output_dir)
  err_path       <- .error_path(template_path, output_dir)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  done_keys <- character(0)
  if (file.exists(out_path)) {
    existing  <- read.csv(out_path, stringsAsFactors = FALSE)
    done_keys <- paste(existing$id, existing$model, sep = "\t")
  }

  row_keys <- paste(as.character(df$id), params$model, sep = "\t")
  pending  <- df[!row_keys %in% done_keys, , drop = FALSE]
  pending  <- head(pending, n)

  if (nrow(pending) == 0) {
    message("Nothing to score — all rows already present in ", out_path)
    return(invisible(out_path))
  }

  message(sprintf("Scoring %d text(s) → %s", nrow(pending), out_path))
  pb     <- txtProgressBar(min = 0, max = nrow(pending), style = 3)
  n_ok   <- 0L
  n_fail <- 0L

  for (i in seq_len(nrow(pending))) {
    id   <- as.character(pending$id[i])
    text <- pending$text[i]

    result <- tryCatch({
      raw    <- call_openai(build_prompt(template, text), params, api_key)
      scores <- parse_response(raw)
      list(ok = TRUE, raw = raw, scores = scores)
    }, error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    })

    if (result$ok) {
      .append_csv(.to_row(id, text, result$raw, result$scores, prompt_version, params),
                  out_path)
      n_ok <- n_ok + 1L
    } else {
      .append_csv(
        data.frame(id = id, text = text, error = result$message,
                   model = params$model,
                   scored_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                   stringsAsFactors = FALSE),
        err_path
      )
      n_fail <- n_fail + 1L
    }

    setTxtProgressBar(pb, i)
  }

  close(pb)

  if (n_fail == 0) {
    message(sprintf("Done. %d scored.", n_ok))
  } else {
    message(sprintf("Done. %d scored, %d failed — see %s.", n_ok, n_fail, err_path))
  }

  invisible(out_path)
}

#' Report batch scoring progress for a dataset
#'
#' Prints a summary of how many rows in `df` have been scored, failed, or are
#' still pending for the given rubric and model. Useful for checking the status
#' of an in-progress or interrupted batch run.
#'
#' @param df A data frame with at minimum columns `id` and `text` — the same
#'   data frame passed to [score_many()].
#' @param template_path Character scalar. Path to the `.txt` rubric template.
#' @param params A `run_params` object (from [run_params()]). Used to filter
#'   status by model.
#' @param output_dir Character scalar. Directory where output files are written.
#'   Must match the `output_dir` used in [score_many()]. Default `"data"`.
#'
#' @return A named list with elements `total`, `scored`, `failed`, `pending`,
#'   invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- read.csv("participants.csv")
#' status(df, "rubric_v1.txt", run_params(), output_dir = "results")
#' }
status <- function(df, template_path, params = run_params(), output_dir = "data") {
  .check_df(df)

  out_path <- .output_path(template_path, output_dir)
  err_path <- .error_path(template_path, output_dir)
  row_keys <- paste(as.character(df$id), params$model, sep = "\t")

  scored <- 0L
  if (file.exists(out_path)) {
    existing  <- read.csv(out_path, stringsAsFactors = FALSE)
    done_keys <- paste(existing$id, existing$model, sep = "\t")
    scored    <- sum(row_keys %in% done_keys)
  }

  failed <- 0L
  if (file.exists(err_path)) {
    errors   <- read.csv(err_path, stringsAsFactors = FALSE)
    err_keys <- paste(errors$id, errors$model, sep = "\t")
    failed   <- sum(row_keys %in% err_keys)
  }

  pending <- nrow(df) - scored - failed

  cat(sprintf("Rubric     : %s\n", basename(template_path)))
  cat(sprintf("Model      : %s\n", params$model))
  cat(sprintf("Output dir : %s\n", output_dir))
  cat(sprintf("Total      : %d\n", nrow(df)))
  cat(sprintf("Scored     : %d\n", scored))
  cat(sprintf("Failed     : %d\n", failed))
  cat(sprintf("Pending    : %d\n", pending))

  invisible(list(total   = nrow(df), scored  = scored,
                 failed  = failed,   pending = pending))
}
