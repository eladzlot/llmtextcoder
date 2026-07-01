#' @noRd
.batch_state_path <- function(template_path, params, output_dir = "data") {
  stem  <- tools::file_path_sans_ext(basename(template_path))
  model <- gsub("[^A-Za-z0-9._-]", "_", params$model)
  file.path(output_dir, paste0(stem, "_batch_", model, ".json"))
}

#' @noRd
.build_batch_jsonl <- function(df, template, params, placeholders) {
  lines <- vapply(seq_len(nrow(df)), function(i) {
    data <- as.list(df[i, placeholders, drop = FALSE])
    body <- list(
      model           = params$model,
      temperature     = params$temperature,
      response_format = list(type = "json_object"),
      messages        = list(list(role    = "user",
                                  content = build_prompt(template, data)))
    )
    jsonlite::toJSON(list(
      custom_id = as.character(df$id[i]),
      method    = "POST",
      url       = "/v1/chat/completions",
      body      = body
    ), auto_unbox = TRUE)
  }, character(1))
  paste(lines, collapse = "\n")
}

#' @noRd
.upload_jsonl <- function(jsonl_text, api_key, base_url) {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp))
  writeLines(jsonl_text, tmp, useBytes = TRUE)

  httr2::request(base_url) |>
    httr2::req_url_path_append("files") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_multipart(
      purpose = "batch",
      file    = curl::form_file(tmp, type = "application/jsonl")
    ) |>
    httr2::req_retry(max_tries = 4,
                     is_transient = \(r) httr2::resp_status(r) %in%
                       c(429, 500, 502, 503)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' @noRd
.create_batch <- function(file_id, completion_window, api_key, base_url) {
  httr2::request(base_url) |>
    httr2::req_url_path_append("batches") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(list(
      input_file_id     = file_id,
      endpoint          = "/v1/chat/completions",
      completion_window = completion_window
    )) |>
    httr2::req_retry(max_tries = 4,
                     is_transient = \(r) httr2::resp_status(r) %in%
                       c(429, 500, 502, 503)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' @noRd
.get_batch_status <- function(batch_id, api_key, base_url) {
  httr2::request(base_url) |>
    httr2::req_url_path_append("batches", batch_id) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' @noRd
.download_file_content <- function(file_id, api_key, base_url) {
  httr2::request(base_url) |>
    httr2::req_url_path_append("files", file_id, "content") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

#' Submit a batch scoring job to the OpenAI Batch API
#'
#' Uploads all pending rows (those not already in the output CSV) as a single
#' asynchronous batch job. Results are available within 24 hours at 50% of the
#' standard per-token cost. Use [collect_batch()] to retrieve results once the
#' job completes.
#'
#' A state file (`<stem>_batch_<model>.json`) is written to `output_dir`
#' recording the batch ID, submitted row IDs, and original data values. This
#' file is required by [collect_batch()] and is removed automatically when
#' results are collected. If a state file already exists, `submit_batch()`
#' errors — collect the pending batch first.
#'
#' The template determines which columns of `df` are required: every
#' `{{placeholder}}` must correspond to a column in `df`.
#'
#' @section Multiple concurrent batches:
#' Each rubric–model combination has its own state file, so batches for
#' different rubrics or different models can run simultaneously without
#' interference.
#'
#' @param df A data frame with column `id` and one column per `{{placeholder}}`
#'   in the template.
#' @param template_path Character scalar. Path to the `.txt` rubric template.
#' @param params A `run_params` object (from [run_params()]).
#' @param n Integer. Maximum number of pending rows to include. Default `Inf`.
#' @param output_dir Character scalar. Directory for output files. Default
#'   `"data"`.
#' @param completion_window Character. `"24h"` (default) or `"1h"`.
#' @param api_key Character. OpenAI API key.
#' @param base_url Character. OpenAI API base URL.
#'
#' @return The batch ID string, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' readRenviron(".env")
#' df     <- read.csv("participants.csv")
#' params <- run_params(model = "gpt-4o")
#'
#' submit_batch(df, "rubric_v1.txt", params, output_dir = "results")
#' # Close R, come back in up to 24 hours:
#' collect_batch("rubric_v1.txt", params, output_dir = "results")
#' }
submit_batch <- function(df, template_path, params = run_params(),
                         n                 = Inf,
                         output_dir        = "data",
                         completion_window = "24h",
                         api_key  = Sys.getenv("OPENAI_API_KEY"),
                         base_url = "https://api.openai.com/v1") {
  template     <- read_template(template_path)
  placeholders <- .template_placeholders(template)
  .check_df(df, required_cols = placeholders)

  state_path <- .batch_state_path(template_path, params, output_dir)
  if (file.exists(state_path)) {
    stop(sprintf(paste0(
      "A pending batch already exists:\n  %s\n",
      "Collect it with collect_batch() before submitting a new one."
    ), state_path))
  }

  prompt_version <- tools::file_path_sans_ext(basename(template_path))
  out_path       <- file.path(output_dir, paste0(prompt_version, ".csv"))

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
    message("Nothing to submit — all rows already present in ", out_path)
    return(invisible(NULL))
  }

  message(sprintf("Building batch of %d row(s)…", nrow(pending)))
  jsonl <- .build_batch_jsonl(pending, template, params, placeholders)

  message("Uploading input file…")
  file_resp <- .upload_jsonl(jsonl, api_key, base_url)

  message("Submitting batch job…")
  batch_resp <- .create_batch(file_resp$id, completion_window, api_key,
                              base_url)

  # Store id → {col: val, ...} so collect_batch() can reconstruct rows
  rows <- setNames(
    lapply(seq_len(nrow(pending)), function(i)
      as.list(pending[i, placeholders, drop = FALSE])),
    as.character(pending$id)
  )

  state <- list(
    batch_id          = batch_resp$id,
    input_file_id     = file_resp$id,
    rows              = rows,
    placeholders      = placeholders,
    prompt_version    = prompt_version,
    model             = params$model,
    temperature       = params$temperature,
    completion_window = completion_window,
    submitted_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  writeLines(jsonlite::toJSON(state, auto_unbox = TRUE), state_path)

  message(sprintf(
    "Batch submitted: %s\nCheck status with collect_batch() in up to %s.",
    batch_resp$id, completion_window
  ))
  invisible(batch_resp$id)
}

#' Collect results from a pending OpenAI Batch API job
#'
#' Checks the status of the batch submitted by [submit_batch()]. If complete,
#' results are written to the same output CSV as [score_many()] and the state
#' file is removed. If still in progress, the current counts are printed and
#' the function returns `NULL` — call again later.
#'
#' Failures are written to `_errors.csv` and the run continues, consistent
#' with [score_many()] behaviour.
#'
#' @param template_path Character scalar. Path to the `.txt` rubric template.
#'   Must match what was passed to [submit_batch()].
#' @param params A `run_params` object. Must match what was passed to
#'   [submit_batch()].
#' @param output_dir Character scalar. Must match what was passed to
#'   [submit_batch()]. Default `"data"`.
#' @param api_key Character. OpenAI API key.
#' @param base_url Character. OpenAI API base URL.
#'
#' @return The output CSV path invisibly if the batch completed, `NULL` if
#'   still in progress.
#' @export
#'
#' @examples
#' \dontrun{
#' collect_batch("rubric_v1.txt", run_params(), output_dir = "results")
#' }
collect_batch <- function(template_path, params = run_params(),
                          output_dir = "data",
                          api_key  = Sys.getenv("OPENAI_API_KEY"),
                          base_url = "https://api.openai.com/v1") {
  state_path <- .batch_state_path(template_path, params, output_dir)
  if (!file.exists(state_path))
    stop(sprintf(paste0(
      "No pending batch state found for this rubric + model combination.\n",
      "  Expected: %s\n",
      "  Possible reasons:\n",
      "    - The batch was never submitted (call submit_batch() first).\n",
      "    - The batch was already collected and the state file was cleaned up.\n",
      "    - template_path, params$model, or output_dir does not match what was\n",
      "      passed to submit_batch()."
    ), state_path))

  state    <- jsonlite::fromJSON(readLines(state_path, warn = FALSE),
                                 simplifyVector = FALSE)
  batch_id <- state$batch_id

  message(sprintf("Checking batch %s…", batch_id))
  info   <- .get_batch_status(batch_id, api_key, base_url)
  counts <- info$request_counts

  message(sprintf("Status: %s  (%d completed, %d failed, %d total)",
                  info$status,
                  counts$completed %||% 0L,
                  counts$failed    %||% 0L,
                  counts$total     %||% 0L))

  terminal <- c("completed", "failed", "expired", "cancelled")
  if (!info$status %in% terminal) {
    message("Not ready yet — call collect_batch() again later.")
    return(invisible(NULL))
  }

  stem     <- tools::file_path_sans_ext(basename(template_path))
  out_path <- file.path(output_dir, paste0(stem, ".csv"))
  err_path <- file.path(output_dir, paste0(stem, "_errors.csv"))

  if (info$status != "completed") {
    message(sprintf("Batch ended with status '%s'. No results to collect.",
                    info$status))
    file.remove(state_path)
    return(invisible(NULL))
  }

  message("Downloading results…")
  raw_jsonl <- .download_file_content(info$output_file_id, api_key, base_url)
  lines     <- strsplit(trimws(raw_jsonl), "\n")[[1]]
  lines     <- lines[nzchar(lines)]

  fake_params <- structure(
    list(model = state$model, temperature = state$temperature),
    class = "run_params"
  )

  placeholders <- state$placeholders %||% character(0)

  n_ok   <- 0L
  n_fail <- 0L

  for (line in lines) {
    entry <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    id    <- entry$custom_id
    data  <- state$rows[[id]] %||%
      setNames(as.list(rep(NA_character_, length(placeholders))), placeholders)

    result <- tryCatch({
      if (!is.null(entry$error))
        stop(entry$error$message)
      raw    <- entry$response$body$choices[[1]]$message$content
      scores <- parse_response(raw)
      list(ok = TRUE, raw = raw, scores = scores)
    }, error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    })

    if (result$ok) {
      .append_csv(
        .to_row(id, data, result$raw, result$scores,
                state$prompt_version, fake_params),
        out_path
      )
      n_ok <- n_ok + 1L
    } else {
      .append_csv(
        cbind(data.frame(id = id, stringsAsFactors = FALSE),
              data.frame(error     = result$message,
                         model     = state$model,
                         scored_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                         stringsAsFactors = FALSE)),
        err_path
      )
      n_fail <- n_fail + 1L
    }
  }

  file.remove(state_path)

  if (n_fail == 0L) {
    message(sprintf("Done. %d scored.", n_ok))
  } else {
    message(sprintf("Done. %d scored, %d failed — see %s.", n_ok, n_fail,
                    err_path))
  }

  invisible(out_path)
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
