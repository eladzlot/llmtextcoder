# --- read_template -----------------------------------------------------------

test_that("read_template() reads a file into a single string", {
  tmp <- tempfile()
  writeLines(c("line one", "line two"), tmp)
  expect_equal(read_template(tmp), "line one\nline two")
})

test_that("read_template() errors with informative message when file not found", {
  err <- tryCatch(read_template("/no/such/file.txt"), error = conditionMessage)
  expect_match(err, "not found")
  expect_match(err, "/no/such/file.txt")
})

# --- .template_placeholders --------------------------------------------------

test_that(".template_placeholders() extracts single placeholder", {
  expect_equal(.template_placeholders("Rate: {{text}}"), "text")
})

test_that(".template_placeholders() extracts multiple placeholders", {
  result <- .template_placeholders("{{prompt}} and {{response}}")
  expect_equal(sort(result), c("prompt", "response"))
})

test_that(".template_placeholders() deduplicates repeated placeholders", {
  expect_equal(.template_placeholders("{{x}} plus {{x}}"), "x")
})

test_that(".template_placeholders() returns character(0) for no placeholders", {
  expect_equal(.template_placeholders("no placeholders here"), character(0))
})

# --- build_prompt ------------------------------------------------------------

test_that("build_prompt() substitutes a single placeholder", {
  result <- build_prompt("Rate this: {{text}}", list(text = "hello world"))
  expect_equal(result, "Rate this: hello world")
})

test_that("build_prompt() replaces all occurrences of the same placeholder", {
  result <- build_prompt("{{text}} and {{text}}", list(text = "X"))
  expect_equal(result, "X and X")
})

test_that("build_prompt() substitutes multiple distinct placeholders", {
  result <- build_prompt("Q: {{question}}\nA: {{answer}}",
                         list(question = "Why?", answer = "Because."))
  expect_equal(result, "Q: Why?\nA: Because.")
})

test_that("build_prompt() accepts a named character vector", {
  result <- build_prompt("Say: {{msg}}", c(msg = "hello"))
  expect_equal(result, "Say: hello")
})

test_that("build_prompt() errors when template has no placeholders", {
  expect_error(build_prompt("no placeholder here", list(text = "x")),
               "no.*placeholder")
})

test_that("build_prompt() errors when required placeholder is missing from data", {
  expect_error(build_prompt("{{text}} and {{other}}", list(text = "x")),
               "\\{\\{other\\}\\}")
})

# --- read_template -----------------------------------------------------------

test_that("read_template() reads a file into a single string", {
  tmp <- tempfile()
  writeLines(c("line one", "line two"), tmp)
  result <- read_template(tmp)
  expect_equal(result, "line one\nline two")
})

test_that("read_template() errors with informative message when file not found", {
  err <- tryCatch(read_template("/no/such/file.txt"), error = conditionMessage)
  expect_match(err, "not found")
  expect_match(err, "/no/such/file.txt")
})
