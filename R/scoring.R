
#' @noRd
.flatten_scores <- function(scores) {
  result <- list()
  for (nm in names(scores)) {
    val <- scores[[nm]]
    if (is.null(val)) {
      result[[nm]] <- NA_character_
    } else if (is.list(val)) {
      for (sub_nm in names(val)) {
        result[[paste0(nm, "_", sub_nm)]] <- if (is.null(val[[sub_nm]])) NA_character_ else val[[sub_nm]]
      }
    } else {
      result[[nm]] <- val
    }
  }
  result
}

#' @noRd
.fmt_elapsed <- function(secs) {
  if (secs < 60) sprintf("%ds", round(secs))
  else sprintf("%dm %02ds", floor(secs / 60), round(secs %% 60))
}

#' @noRd
.to_row <- function(id, data, raw, scores, prompt_version, params) {
  cbind(
    data.frame(id = id, stringsAsFactors = FALSE),
    as.data.frame(.flatten_scores(scores), stringsAsFactors = FALSE),
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
.check_df <- function(df, required_cols = character(0)) {
  if (!is.data.frame(df))
    stop(sprintf(
      "'df' must be a data frame, got %s.\n  Load your data with read.csv() or pass an existing data frame.",
      class(df)[1L]
    ))
  all_required <- c("id", required_cols)
  missing_cols <- setdiff(all_required, names(df))
  if (length(missing_cols) > 0L)
    stop(sprintf(
      "'df' is missing required column(s): %s\n  Columns found in df: %s",
      paste(missing_cols, collapse = ", "),
      if (length(names(df)) == 0L) "(none)"
      else paste(names(df), collapse = ", ")
    ))
}

#' Score a single row of data against a rubric template
#'
#' A convenience wrapper for interactive and testing use. Builds the prompt,
#' calls the API, and parses the response in one step. Pass the result to
#' [print_result()] to display it, or inspect `$raw` and `$scores` directly.
#'
#' @param template Character scalar. The rubric template string with
#'   `{{placeholder}}` markers. Load from a file with
#'   `paste(readLines("prompts/rubric.txt"), collapse = "\n")`.
#' @param data Named list. Values for each `{{placeholder}}` in the template.
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
#' rubric <- paste(readLines("prompts/rubric_v1.txt"), collapse = "\n")
#' result <- score_one(rubric,
#'                     list(text = "I keep worrying about the same thing."),
#'                     run_params())
#' print_result(result$raw, result$scores)
#' }
score_one <- function(template, data, params = run_params(),
                      api_key = Sys.getenv("OPENAI_API_KEY")) {
  prompt <- build_prompt(template, data)
  raw    <- call_openai(prompt, params, api_key)
  scores <- parse_response(raw)
  list(raw = raw, scores = scores)
}

#' Score a data frame of rows against a rubric, with incremental output
#'
#' Scores each row of `df` against the rubric `template`, appending one row to
#' the output CSV immediately after each API call. If the run is interrupted,
#' restart with the same call — already-scored rows are skipped automatically.
#'
#' The template determines which columns of `df` are used: every
#' `{{placeholder}}` in the template must correspond to a column in `df`.
#' Input texts are not written to the output CSV.
#'
#' @section PII check:
#' By default, `score_many()` scans all template placeholder columns for
#' potentially identifying information before any API call is made. Flagged
#' rows are written to `<output_dir>/<output_name>_pii.csv` and a warning is
#' printed, but scoring proceeds. Review the PII file (or run [scan_pii()]
#' beforehand) and re-score a cleaned data frame if needed. Set
#' `pii_check = FALSE` once you have reviewed.
#'
#' @section Output files:
#' Results go to `<output_dir>/<output_name>.csv` and failures to
#' `<output_dir>/<output_name>_errors.csv`. Failures are retried automatically
#' on the next run.
#'
#' @section Skip logic:
#' A row is skipped if the same `id` **and** `model` already appear in the
#' output CSV. Restarting an interrupted run picks up where it left off; scoring
#' with a different model appends new rows alongside the old ones.
#'
#' @param df A data frame with column `id` and one column per `{{placeholder}}`
#'   in the template.
#' @param template Character scalar. The rubric template string with
#'   `{{placeholder}}` markers. Use an inline string or load from a file with
#'   `paste(readLines("prompts/rubric.txt"), collapse = "\n")`.
#' @param output_name Character scalar. Stem used to name output files:
#'   `<output_dir>/<output_name>.csv`. Pick a name that identifies the rubric
#'   version, e.g. `"clinical_v1"`.
#' @param params A `run_params` object (from [run_params()]).
#' @param n Integer. Maximum number of *unscored* rows to process. Default
#'   `Inf`.
#' @param output_dir Character scalar. Directory for output files. Default
#'   `"data"`.
#' @param force Logical. Delete existing output and error CSVs before scoring,
#'   so all rows are treated as pending. Default `FALSE`.
#' @param pii_check Logical. Scan placeholder columns for PII before scoring.
#'   Default `TRUE`.
#' @param pii_auto_patterns Named character vector passed to [scan_pii()].
#' @param pii_disclosure_patterns Named character vector passed to [scan_pii()].
#' @param api_key Character. OpenAI API key.
#'
#' @return The path to the output CSV, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' library(llmtextcoder)
#' readRenviron(".env")
#' rubric <- paste(readLines("prompts/rubric_v1.txt"), collapse = "\n")
#' df     <- varieties_testimonials()
#' params <- run_params(model = "gpt-4o", temperature = 0)
#'
#' score_many(df, rubric, "rubric_v1", params, n = 10, output_dir = "results")
#' score_many(df, rubric, "rubric_v1", params, output_dir = "results")
#' }
score_many <- function(df, template, output_name, params = run_params(), n = Inf,
                       output_dir = "data",
                       force = FALSE,
                       pii_check = TRUE,
                       pii_auto_patterns       = auto_pii_patterns(),
                       pii_disclosure_patterns = disclosure_pii_patterns(),
                       api_key = Sys.getenv("OPENAI_API_KEY")) {
  placeholders <- .template_placeholders(template)
  .check_df(df, required_cols = placeholders)

  out_path <- file.path(output_dir, paste0(output_name, ".csv"))
  err_path <- file.path(output_dir, paste0(output_name, "_errors.csv"))

  if (force) {
    if (file.exists(out_path)) file.remove(out_path)
    if (file.exists(err_path)) file.remove(err_path)
  }

  if (pii_check && length(placeholders) > 0L) {
    pii_path <- file.path(output_dir, paste0(output_name, "_pii.csv"))
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    pii_results <- lapply(placeholders, function(col) {
      scan_df        <- df[, c("id", col), drop = FALSE]
      names(scan_df)[2L] <- "text"
      scan_pii(scan_df,
               auto_patterns       = pii_auto_patterns,
               disclosure_patterns = pii_disclosure_patterns)
    })
    pii_result <- do.call(rbind, Filter(function(r) nrow(r) > 0L, pii_results))
    if (!is.null(pii_result) && nrow(pii_result) > 0L) {
      n_flagged <- length(unique(pii_result$id))
      warning(sprintf(
        "%d row(s) flagged for potential PII — review %s before sharing data.\n  Set pii_check = FALSE once reviewed.",
        n_flagged, pii_path
      ), call. = FALSE)
      write.csv(pii_result, pii_path, row.names = FALSE)
    }
  }

  prompt_version <- output_name

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  done_keys <- character(0)
  if (file.exists(out_path)) {
    existing  <- read.csv(out_path, stringsAsFactors = FALSE)
    done_keys <- paste(existing$id, existing$model, sep = "\t")
  }

  row_keys <- paste(as.character(df$id), params$model, sep = "\t")
  pending  <- df[!row_keys %in% done_keys, , drop = FALSE]
  pending  <- head(pending, n)

  if (nrow(pending) == 0L) {
    message("Nothing to score — all rows already present in ", out_path)
    return(invisible(out_path))
  }

  message(sprintf("Scoring %d row(s) → %s", nrow(pending), out_path))
  t0     <- Sys.time()
  n_ok   <- 0L
  n_fail <- 0L
  total  <- nrow(pending)
  width  <- 30L

  for (i in seq_len(total)) {
    id   <- as.character(pending$id[i])
    data <- as.list(pending[i, placeholders, drop = FALSE])

    result <- tryCatch({
      raw    <- call_openai(build_prompt(template, data), params, api_key)
      scores <- parse_response(raw)
      list(ok = TRUE, raw = raw, scores = scores)
    }, error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    })

    if (result$ok) {
      .append_csv(
        .to_row(id, data, result$raw, result$scores, prompt_version, params),
        out_path
      )
      n_ok <- n_ok + 1L
    } else {
      .append_csv(
        cbind(data.frame(id = id, stringsAsFactors = FALSE),
              data.frame(error     = result$message,
                         model     = params$model,
                         scored_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                         stringsAsFactors = FALSE)),
        err_path
      )
      n_fail <- n_fail + 1L
    }

    filled  <- round(width * i / total)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cat(sprintf("\r  [%s%s] %d/%d (%d%%) | %s elapsed  ",
                strrep("=", filled), strrep(" ", width - filled),
                i, total, round(100 * i / total),
                .fmt_elapsed(elapsed)))
    flush.console()
  }

  cat("\n")
  total_elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  if (n_fail == 0L) {
    message(sprintf("Done. %d scored in %s.", n_ok, .fmt_elapsed(total_elapsed)))
  } else {
    message(sprintf("Done. %d scored, %d failed in %s — see %s.",
                    n_ok, n_fail, .fmt_elapsed(total_elapsed), err_path))
  }

  invisible(out_path)
}

#' Report batch scoring progress for a dataset
#'
#' Prints a summary of how many rows in `df` have been scored, failed, or are
#' still pending for the given rubric and model.
#'
#' @param df A data frame with at minimum column `id`.
#' @param output_name Character scalar. Must match the `output_name` used in
#'   [score_many()].
#' @param params A `run_params` object (from [run_params()]).
#' @param output_dir Character scalar. Must match the `output_dir` used in
#'   [score_many()]. Default `"data"`.
#'
#' @return A named list with elements `total`, `scored`, `failed`, `pending`,
#'   invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- varieties_testimonials()
#' status(df, "clinical_v1", run_params(), output_dir = "results")
#' }
status <- function(df, output_name, params = run_params(), output_dir = "data") {
  .check_df(df)

  out_path <- file.path(output_dir, paste0(output_name, ".csv"))
  err_path <- file.path(output_dir, paste0(output_name, "_errors.csv"))
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

  cat(sprintf("Rubric     : %s\n", output_name))
  cat(sprintf("Model      : %s\n", params$model))
  cat(sprintf("Output dir : %s\n", output_dir))
  cat(sprintf("Total      : %d\n", nrow(df)))
  cat(sprintf("Scored     : %d\n", scored))
  cat(sprintf("Failed     : %d\n", failed))
  cat(sprintf("Pending    : %d\n", pending))

  invisible(list(total   = nrow(df), scored  = scored,
                 failed  = failed,   pending = pending))
}
