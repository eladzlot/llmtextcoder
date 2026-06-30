test_that("run_params() defaults are sensible", {
  p <- run_params()
  expect_equal(p$model, "gpt-4o")
  expect_equal(p$temperature, 0)
})

test_that("run_params() stores custom values", {
  p <- run_params(model = "gpt-4o-mini", temperature = 0.5)
  expect_equal(p$model, "gpt-4o-mini")
  expect_equal(p$temperature, 0.5)
})

test_that("run_params() rejects temperature out of [0, 2]", {
  expect_error(run_params(temperature = -0.1), "between 0 and 2")
  expect_error(run_params(temperature = 2.1),  "between 0 and 2")
})

test_that("run_params() rejects empty model", {
  expect_error(run_params(model = ""), "non-empty")
})

test_that("run_params() rejects non-character model", {
  expect_error(run_params(model = 42), "non-empty character")
})

test_that("run_params() rejects non-numeric temperature", {
  expect_error(run_params(temperature = "high"), "single number")
})

test_that("run_params() result has class run_params", {
  expect_s3_class(run_params(), "run_params")
})
