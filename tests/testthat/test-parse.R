test_that("parse_response() returns a named list from valid JSON", {
  result <- parse_response('{"a": 1, "b": 2}')
  expect_type(result, "list")
  expect_equal(result$a, 1)
  expect_equal(result$b, 2)
})

test_that("parse_response() errors on non-object JSON", {
  expect_error(parse_response("[1, 2, 3]"))
  expect_error(parse_response("42"))
})

test_that("parse_response() errors on invalid JSON", {
  expect_error(parse_response("not json at all"))
})
