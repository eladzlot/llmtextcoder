test_that("call_openai() errors clearly when api_key is empty", {
  err <- tryCatch(
    call_openai("some prompt", run_params(), api_key = ""),
    error = conditionMessage
  )
  expect_match(err, "No OpenAI API key")
  expect_match(err, "OPENAI_API_KEY")
  expect_match(err, "\\.env")
})

