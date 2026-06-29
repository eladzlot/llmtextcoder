test_that("build_prompt() substitutes {{text}}", {
  result <- build_prompt("Rate this: {{text}}", "hello world")
  expect_equal(result, "Rate this: hello world")
})

test_that("build_prompt() replaces all occurrences", {
  result <- build_prompt("{{text}} and {{text}}", "X")
  expect_equal(result, "X and X")
})

test_that("build_prompt() errors when placeholder is absent", {
  expect_error(build_prompt("no placeholder here", "text"))
})

test_that("read_template() reads a file into a single string", {
  tmp <- tempfile()
  writeLines(c("line one", "line two"), tmp)
  result <- read_template(tmp)
  expect_equal(result, "line one\nline two")
})
