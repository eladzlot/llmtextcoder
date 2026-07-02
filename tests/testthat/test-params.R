test_that("run_params() defaults are sensible", {
  p <- run_params()
  expect_equal(p$model, "gpt-4o")
  expect_equal(p$extras, list())
})

test_that("run_params() stores extras", {
  p <- run_params(model = "gpt-4o-mini", temperature = 0.5, seed = 42)
  expect_equal(p$model, "gpt-4o-mini")
  expect_equal(p$extras$temperature, 0.5)
  expect_equal(p$extras$seed, 42)
})

test_that("run_params() works with no extras (reasoning models)", {
  p <- run_params(model = "o4-mini")
  expect_equal(p$extras, list())
  expect_s3_class(p, "run_params")
})

test_that("run_params() rejects empty model", {
  expect_error(run_params(model = ""), "non-empty")
})

test_that("run_params() rejects non-character model", {
  expect_error(run_params(model = 42), "non-empty character")
})

test_that("run_params() result has class run_params", {
  expect_s3_class(run_params(), "run_params")
})

test_that("score_many() writes params as JSON column", {
  withr::with_tempdir({
    dir.create("data")
    old_fn <- get("call_openai", envir = globalenv())
    assign("call_openai", function(...) '{"score": 1}', envir = globalenv())
    on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

    df <- data.frame(id = "r1", text = "x", stringsAsFactors = FALSE)
    score_many(df, "Rate: {{text}}", "test_v1",
               run_params(model = "gpt-4o", temperature = 0),
               pii_check = FALSE)
    out <- read.csv("data/test_v1.csv", stringsAsFactors = FALSE)
    expect_true("params" %in% names(out))
    expect_false("temperature" %in% names(out))
    p <- jsonlite::fromJSON(out$params)
    expect_equal(p$temperature, 0)
  })
})
