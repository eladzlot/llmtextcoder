# Helpers ─────────────────────────────────────────────────────────────────────

make_df <- function(ids = c("P1", "P2"), texts = c("text one", "text two")) {
  data.frame(id = ids, text = texts, stringsAsFactors = FALSE)
}

make_state <- function(dir, stem = "rubric", model = "gpt-4o",
                       batch_id = "batch_abc",
                       ids = c("P1", "P2"), texts = c("text one", "text two")) {
  rows <- setNames(
    lapply(texts, function(t) list(text = t)),
    ids
  )
  state <- list(
    batch_id          = batch_id,
    input_file_id     = "file_xyz",
    rows              = rows,
    placeholders      = "text",
    prompt_version    = stem,
    model             = model,
    temperature       = 0,
    completion_window = "24h",
    submitted_at      = "2026-01-01T00:00:00"
  )
  path <- file.path(dir, paste0(stem, "_batch_", model, ".json"))
  writeLines(jsonlite::toJSON(state, auto_unbox = TRUE), path)
  path
}

# Build a JSONL string from a list of result entry lists
make_output_jsonl <- function(entries) {
  paste(
    vapply(entries, function(e) jsonlite::toJSON(e, auto_unbox = TRUE, null = "null"),
           character(1)),
    collapse = "\n"
  )
}

# Mock .get_batch_status and .download_file_content; restore on test exit.
mock_collect <- function(status_fn, download_fn) {
  old_status   <- get(".get_batch_status",     envir = globalenv())
  old_download <- get(".download_file_content", envir = globalenv())
  parent <- parent.env(environment())
  do.call("on.exit", list(substitute({
    assign(".get_batch_status",     old_status,   envir = globalenv())
    assign(".download_file_content", old_download, envir = globalenv())
  }), add = TRUE), envir = parent)
  assign(".get_batch_status",      status_fn,   envir = globalenv())
  assign(".download_file_content", download_fn, envir = globalenv())
}

# ── .batch_state_path ─────────────────────────────────────────────────────────

test_that("state path encodes stem and model", {
  path <- .batch_state_path("prompts/rubric_v1.txt", run_params("gpt-4o"),
                             "results")
  expect_equal(path, "results/rubric_v1_batch_gpt-4o.json")
})

test_that("state path sanitises unusual model name characters", {
  path <- .batch_state_path("rubric.txt", run_params("org/model:latest"), "out")
  expect_false(grepl("[/:]", basename(path)))
})

# ── .build_batch_jsonl ────────────────────────────────────────────────────────

test_that("build_batch_jsonl produces one line per row", {
  lines <- strsplit(
    .build_batch_jsonl(make_df(), "Rate: {{text}}", run_params(), "text"), "\n"
  )[[1]]
  expect_equal(length(lines), 2)
})

test_that("each jsonl line has custom_id matching row id", {
  lines  <- strsplit(
    .build_batch_jsonl(make_df(), "Rate: {{text}}", run_params(), "text"), "\n"
  )[[1]]
  parsed <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
  expect_equal(parsed[[1]]$custom_id, "P1")
  expect_equal(parsed[[2]]$custom_id, "P2")
})

test_that("jsonl lines contain injected text in message content", {
  df    <- make_df(texts = c("hello world", "goodbye"))
  lines <- strsplit(
    .build_batch_jsonl(df, "Say: {{text}}", run_params(), "text"), "\n"
  )[[1]]
  parsed <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
  expect_true(grepl("hello world",
                    parsed[[1]]$body$messages[[1]]$content))
})

test_that("jsonl lines request json_object response format", {
  lines  <- strsplit(
    .build_batch_jsonl(make_df(), "Rate: {{text}}", run_params(), "text"), "\n"
  )[[1]]
  parsed <- jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE)
  expect_equal(parsed$body$response_format$type, "json_object")
})

# ── submit_batch ──────────────────────────────────────────────────────────────

test_that("submit_batch errors if state file already exists", {
  withr::with_tempdir({
    writeLines("Rate: {{text}}", "rubric.txt")
    make_state(".")
    expect_error(
      submit_batch(make_df(), "rubric.txt", run_params(), output_dir = "."),
      "pending batch"
    )
  })
})

test_that("submit_batch messages when nothing is pending", {
  withr::with_tempdir({
    writeLines("Rate: {{text}}", "rubric.txt")
    p  <- run_params()
    df <- make_df()
    out <- data.frame(id = c("P1", "P2"), text = c("a", "b"), score = 1,
                      raw = "{}", prompt_version = "rubric",
                      model = p$model, params = "{}",
                      scored_at = "2026-01-01", stringsAsFactors = FALSE)
    write.csv(out, "rubric.csv", row.names = FALSE)
    expect_message(
      submit_batch(df, "rubric.txt", p, output_dir = "."),
      "Nothing to submit"
    )
  })
})

test_that("submit_batch writes state file with rows and batch_id", {
  withr::with_tempdir({
    writeLines("Rate: {{text}}", "rubric.txt")

    old_upload <- get(".upload_jsonl",  envir = globalenv())
    old_create <- get(".create_batch",  envir = globalenv())
    on.exit({
      assign(".upload_jsonl", old_upload, envir = globalenv())
      assign(".create_batch", old_create, envir = globalenv())
    }, add = TRUE)
    assign(".upload_jsonl", function(...) list(id = "file_test"),
           envir = globalenv())
    assign(".create_batch", function(...) list(id = "batch_test"),
           envir = globalenv())

    suppressMessages(
      submit_batch(make_df(), "rubric.txt", run_params(), output_dir = ".")
    )

    state_file <- "rubric_batch_gpt-4o.json"
    expect_true(file.exists(state_file))
    state <- jsonlite::fromJSON(readLines(state_file, warn = FALSE),
                                simplifyVector = FALSE)
    expect_equal(state$batch_id, "batch_test")
    expect_equal(state$rows[["P1"]][["text"]], "text one")
    expect_equal(state$rows[["P2"]][["text"]], "text two")
    expect_equal(state$placeholders, "text")
  })
})

test_that("submit_batch respects n parameter", {
  withr::with_tempdir({
    writeLines("Rate: {{text}}", "rubric.txt")
    df <- make_df(ids   = c("P1", "P2", "P3"),
                  texts = c("a",  "b",  "c"))

    captured_jsonl <- NULL
    old_upload <- get(".upload_jsonl", envir = globalenv())
    old_create <- get(".create_batch", envir = globalenv())
    on.exit({
      assign(".upload_jsonl", old_upload, envir = globalenv())
      assign(".create_batch", old_create, envir = globalenv())
    }, add = TRUE)
    assign(".upload_jsonl",
           function(jsonl_text, ...) { captured_jsonl <<- jsonl_text
                                       list(id = "f") },
           envir = globalenv())
    assign(".create_batch", function(...) list(id = "b"), envir = globalenv())

    suppressMessages(
      submit_batch(df, "rubric.txt", run_params(), n = 2, output_dir = ".")
    )
    lines <- strsplit(captured_jsonl, "\n")[[1]]
    expect_equal(length(lines), 2)
  })
})

# ── collect_batch ─────────────────────────────────────────────────────────────

test_that("collect_batch errors when no state file exists", {
  withr::with_tempdir({
    writeLines("Rate: {{text}}", "rubric.txt")
    expect_error(
      collect_batch("rubric.txt", run_params(), output_dir = "."),
      "No pending batch"
    )
  })
})

test_that("collect_batch returns NULL and preserves state when not ready", {
  withr::with_tempdir({
    writeLines("Rate: {{text}}", "rubric.txt")
    make_state(".")

    old_status <- get(".get_batch_status", envir = globalenv())
    on.exit(assign(".get_batch_status", old_status, envir = globalenv()),
            add = TRUE)
    assign(".get_batch_status",
           function(...) list(status = "in_progress",
                              request_counts = list(completed = 1L,
                                                    failed    = 0L,
                                                    total     = 2L)),
           envir = globalenv())

    result <- suppressMessages(
      collect_batch("rubric.txt", run_params(), output_dir = ".")
    )
    expect_null(result)
    expect_true(file.exists("rubric_batch_gpt-4o.json"))
  })
})

test_that("collect_batch writes CSV, restores data, removes state when complete", {
  withr::with_tempdir({
    writeLines("Rate: {{text}}", "rubric.txt")
    make_state(".")

    output_jsonl <- make_output_jsonl(list(
      list(custom_id = "P1",
           response  = list(body = list(choices = list(
             list(message = list(content = '{"score":4}'))
           ))),
           error = NULL),
      list(custom_id = "P2",
           response  = list(body = list(choices = list(
             list(message = list(content = '{"score":2}'))
           ))),
           error = NULL)
    ))

    old_status   <- get(".get_batch_status",     envir = globalenv())
    old_download <- get(".download_file_content", envir = globalenv())
    on.exit({
      assign(".get_batch_status",     old_status,   envir = globalenv())
      assign(".download_file_content", old_download, envir = globalenv())
    }, add = TRUE)
    assign(".get_batch_status",
           function(...) list(status = "completed", output_file_id = "f_out",
                              request_counts = list(completed = 2L,
                                                    failed = 0L, total = 2L)),
           envir = globalenv())
    assign(".download_file_content",
           function(...) output_jsonl,
           envir = globalenv())

    suppressMessages(
      collect_batch("rubric.txt", run_params(), output_dir = ".")
    )

    expect_false(file.exists("rubric_batch_gpt-4o.json"))
    expect_true(file.exists("rubric.csv"))
    out <- read.csv("rubric.csv", stringsAsFactors = FALSE)
    expect_equal(nrow(out), 2)
    expect_true("score" %in% names(out))
    expect_false("text" %in% names(out))
  })
})

test_that("collect_batch routes API errors to _errors.csv", {
  withr::with_tempdir({
    writeLines("Rate: {{text}}", "rubric.txt")
    make_state(".")

    output_jsonl <- make_output_jsonl(list(
      list(custom_id = "P1",
           response  = list(body = list(choices = list(
             list(message = list(content = '{"score":4}'))
           ))),
           error = NULL),
      list(custom_id = "P2",
           response  = NULL,
           error     = list(message = "rate limit exceeded"))
    ))

    old_status   <- get(".get_batch_status",     envir = globalenv())
    old_download <- get(".download_file_content", envir = globalenv())
    on.exit({
      assign(".get_batch_status",     old_status,   envir = globalenv())
      assign(".download_file_content", old_download, envir = globalenv())
    }, add = TRUE)
    assign(".get_batch_status",
           function(...) list(status = "completed", output_file_id = "f_out",
                              request_counts = list(completed = 1L,
                                                    failed = 1L, total = 2L)),
           envir = globalenv())
    assign(".download_file_content",
           function(...) output_jsonl,
           envir = globalenv())

    suppressMessages(
      collect_batch("rubric.txt", run_params(), output_dir = ".")
    )

    expect_true(file.exists("rubric.csv"))
    expect_true(file.exists("rubric_errors.csv"))
    out  <- read.csv("rubric.csv",        stringsAsFactors = FALSE)
    errs <- read.csv("rubric_errors.csv", stringsAsFactors = FALSE)
    expect_equal(nrow(out),  1)
    expect_equal(out$id,     "P1")
    expect_equal(nrow(errs), 1)
    expect_equal(errs$id,    "P2")
    expect_true(grepl("rate limit", errs$error))
  })
})
