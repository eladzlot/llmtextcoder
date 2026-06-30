test_that("parse_response() returns a named list from valid JSON", {
  result <- parse_response('{"a": 1, "b": 2}')
  expect_type(result, "list")
  expect_equal(result$a, 1)
  expect_equal(result$b, 2)
})

test_that("parse_response() errors on JSON array with informative message", {
  err <- tryCatch(parse_response("[1, 2, 3]"), error = conditionMessage)
  expect_match(err, "array")
  expect_match(err, "\\[1, 2, 3\\]")
})

test_that("parse_response() errors on non-object scalar JSON", {
  expect_error(parse_response("42"), "object")
})

test_that("parse_response() errors on invalid JSON and shows raw reply", {
  err <- tryCatch(parse_response("not json at all"), error = conditionMessage)
  expect_match(err, "not json at all")
  expect_match(err, "JSON")
})

test_that("parse_response() errors on wrong input type", {
  expect_error(parse_response(list(a = 1)), "character string")
})
