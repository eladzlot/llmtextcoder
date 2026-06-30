# Tests for scoring.R — call_openai is mocked inline per test so no API calls are made.

sample_df <- data.frame(
  id   = c("a", "b", "c"),
  text = c("text one", "text two", "text three"),
  stringsAsFactors = FALSE
)

# --- .output_path / .error_path -------------------------------------------

test_that(".output_path() derives CSV path from template path", {
  expect_equal(.output_path("prompts/rumination_v1.txt"), "data/rumination_v1.csv")
  expect_equal(.output_path("prompts/foo_v2.txt"),        "data/foo_v2.csv")
})

test_that(".output_path() respects output_dir", {
  expect_equal(.output_path("rubric_v1.txt", "results"), "results/rubric_v1.csv")
  expect_equal(.output_path("rubric_v1.txt", "/tmp/out"), "/tmp/out/rubric_v1.csv")
})

test_that(".error_path() is parallel to output path", {
  expect_equal(.error_path("prompts/rubric_v1.txt"), "data/rubric_v1_errors.csv")
  expect_equal(.error_path("rubric_v1.txt", "results"), "results/rubric_v1_errors.csv")
})

# --- Input validation ---------------------------------------------------------

test_that(".check_df() rejects non-data-frames", {
  expect_error(.check_df(list(id = 1, text = "x")), "data frame")
})

test_that(".check_df() always requires id", {
  expect_error(.check_df(data.frame(text = "x")), "id")
})

test_that(".check_df() accepts df with only id when no required_cols set", {
  expect_silent(.check_df(data.frame(id = 1)))
})

test_that(".check_df() reports missing required_cols by name", {
  expect_error(.check_df(data.frame(id = 1), required_cols = "text"), "text")
  expect_error(.check_df(data.frame(id = 1, text = "x"),
                         required_cols = c("text", "mood")), "mood")
})

# --- score_many: core behaviour -------------------------------------------

test_that("score_many() writes all rows when n = Inf", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")
    old_fn <- call_openai
    assign("call_openai", function(...) '{"score": 3}', envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    score_many(sample_df, "prompts/test_v1.txt", run_params())
    out <- read.csv("data/test_v1.csv", stringsAsFactors = FALSE)
    expect_equal(nrow(out), 3)
  })
})

test_that("score_many() respects output_dir", {
  withr::with_tempdir({
    dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")
    old_fn <- call_openai
    assign("call_openai", function(...) '{"score": 3}', envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    score_many(sample_df, "prompts/test_v1.txt", run_params(), output_dir = "myresults")
    expect_true(file.exists("myresults/test_v1.csv"))
    expect_false(file.exists("data/test_v1.csv"))
  })
})

test_that("score_many() n parameter limits rows processed", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")
    old_fn <- call_openai
    assign("call_openai", function(...) '{"score": 3}', envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    score_many(sample_df, "prompts/test_v1.txt", run_params(), n = 1)
    out <- read.csv("data/test_v1.csv", stringsAsFactors = FALSE)
    expect_equal(nrow(out), 1)
    expect_equal(out$id, "a")
  })
})

test_that("score_many() skips rows already scored with the same model", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")
    old_fn <- call_openai
    assign("call_openai", function(...) '{"score": 3}', envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    params <- run_params()
    score_many(sample_df, "prompts/test_v1.txt", params, n = 2)
    score_many(sample_df, "prompts/test_v1.txt", params)

    out <- read.csv("data/test_v1.csv", stringsAsFactors = FALSE)
    expect_equal(nrow(out), 3)
    expect_equal(sort(out$id), c("a", "b", "c"))
  })
})

test_that("score_many() does not skip rows when model changes", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")
    old_fn <- call_openai
    assign("call_openai", function(...) '{"score": 3}', envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    score_many(sample_df, "prompts/test_v1.txt", run_params(model = "gpt-4o"))
    score_many(sample_df, "prompts/test_v1.txt", run_params(model = "gpt-4o-mini"))

    out <- read.csv("data/test_v1.csv", stringsAsFactors = FALSE)
    expect_equal(nrow(out), 6)
  })
})

test_that("score_many() output has required provenance columns", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")
    old_fn <- call_openai
    assign("call_openai", function(...) '{"score": 3}', envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    score_many(sample_df[1, ], "prompts/test_v1.txt", run_params())
    out <- read.csv("data/test_v1.csv", stringsAsFactors = FALSE)

    expect_true(all(c("id", "text", "prompt_version", "model",
                      "temperature", "scored_at", "raw") %in% names(out)))
    expect_equal(out$prompt_version, "test_v1")
    expect_equal(out$model, "gpt-4o")
  })
})

test_that("score_many() writes all placeholder columns to output", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Q: {{question}}\nA: {{answer}}", "prompts/multi_v1.txt")
    old_fn <- call_openai
    assign("call_openai", function(...) '{"score": 5}', envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    df <- data.frame(id = "r1", question = "Why?", answer = "Because.",
                     stringsAsFactors = FALSE)
    score_many(df, "prompts/multi_v1.txt", run_params())
    out <- read.csv("data/multi_v1.csv", stringsAsFactors = FALSE)
    expect_true("question" %in% names(out))
    expect_true("answer"   %in% names(out))
    expect_equal(out$question, "Why?")
    expect_equal(out$answer,   "Because.")
  })
})

test_that("score_many() errors when df is missing a placeholder column", {
  withr::with_tempdir({
    dir.create("prompts")
    writeLines("Q: {{question}}\nA: {{answer}}", "prompts/multi_v1.txt")
    df <- data.frame(id = "r1", question = "Why?", stringsAsFactors = FALSE)
    expect_error(
      score_many(df, "prompts/multi_v1.txt", run_params()),
      "answer"
    )
  })
})

# --- Error handling -------------------------------------------------------

test_that("score_many() writes failures to error CSV and continues", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")

    old_fn <- call_openai
    assign("call_openai", function(prompt, ...) {
      if (grepl("text two", prompt)) stop("simulated API error")
      '{"score": 3}'
    }, envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    score_many(sample_df, "prompts/test_v1.txt", run_params())

    out <- read.csv("data/test_v1.csv",        stringsAsFactors = FALSE)
    err <- read.csv("data/test_v1_errors.csv", stringsAsFactors = FALSE)

    expect_equal(nrow(out), 2)
    expect_false("b" %in% out$id)
    expect_equal(nrow(err), 1)
    expect_equal(err$id, "b")
    expect_true(nzchar(err$error))
  })
})

test_that("score_many() retries failed rows on next run", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")

    old_fn <- call_openai
    assign("call_openai", function(...) stop("down"), envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)
    score_many(sample_df, "prompts/test_v1.txt", run_params())

    assign("call_openai", function(...) '{"score": 1}', envir = globalenv())
    score_many(sample_df, "prompts/test_v1.txt", run_params())

    out <- read.csv("data/test_v1.csv", stringsAsFactors = FALSE)
    expect_equal(nrow(out), 3)
  })
})

# --- status() -------------------------------------------------------------

test_that("status() returns correct counts", {
  withr::with_tempdir({
    dir.create("data"); dir.create("prompts")
    writeLines("Rate: {{text}}", "prompts/test_v1.txt")

    old_fn <- call_openai
    assign("call_openai", function(prompt, ...) {
      if (grepl("text two", prompt)) stop("fail")
      '{"score": 3}'
    }, envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    score_many(sample_df, "prompts/test_v1.txt", run_params())
    s <- status(sample_df, "prompts/test_v1.txt", run_params())

    expect_equal(s$total,   3)
    expect_equal(s$scored,  2)
    expect_equal(s$failed,  1)
    expect_equal(s$pending, 0)
  })
})
