test_that("varieties_testimonials() returns a data frame with expected structure", {
  df <- varieties_testimonials()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("id", "text", "theme", "word_count", "source") %in% names(df)))
  expect_gt(nrow(df), 30)
})

test_that("varieties_testimonials() ids are unique", {
  df <- varieties_testimonials()
  expect_equal(length(unique(df$id)), nrow(df))
})

test_that("varieties_testimonials() texts are non-empty strings", {
  df <- varieties_testimonials()
  expect_true(all(nchar(df$text) > 100))
})

test_that("varieties_testimonials() word counts match text", {
  df <- varieties_testimonials()
  computed <- sapply(strsplit(df$text, "\\s+"), length)
  expect_equal(df$word_count, computed)
})
