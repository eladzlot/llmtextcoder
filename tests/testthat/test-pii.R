# ── pattern constructors ──────────────────────────────────────────────────────

test_that("auto_pii_patterns returns named character vector", {
  p <- auto_pii_patterns()
  expect_type(p, "character")
  expect_true(!is.null(names(p)))
  expect_true("email" %in% names(p))
  expect_false("name_disclosure" %in% names(p))
})

test_that("disclosure_pii_patterns returns named character vector", {
  p <- disclosure_pii_patterns()
  expect_type(p, "character")
  expect_true("name_disclosure" %in% names(p))
  expect_false("email" %in% names(p))
})

test_that("default_pii_patterns is union of both tiers", {
  d <- default_pii_patterns()
  expect_true(all(names(auto_pii_patterns())        %in% names(d)))
  expect_true(all(names(disclosure_pii_patterns()) %in% names(d)))
})

# ── .match_with_context ───────────────────────────────────────────────────────

test_that("context window includes text after the match", {
  m <- .match_with_context("my name is Sarah Johnson today", "my name is")
  expect_true(grepl("Sarah", m))
})

test_that("context window is truncated at a word boundary and marked with ellipsis", {
  long_text <- paste0("my name is ", paste(rep("word", 20), collapse = " "))
  m <- .match_with_context(long_text, "my name is", after = 20)
  expect_true(grepl("…$", m))
  expect_false(grepl("\\s…$", m))
})

test_that("returns character(0) when pattern not found", {
  expect_equal(.match_with_context("hello world", "my name is"), character(0))
})

test_that("returns multiple matches", {
  m <- .match_with_context("a@b.com and c@d.com",
                           "[\\w._%+\\-]+@[\\w.\\-]+\\.[a-zA-Z]{2,}")
  expect_equal(length(m), 2)
})

# ── scan_pii ──────────────────────────────────────────────────────────────────

test_that("scan_pii returns pii_scan class", {
  df <- data.frame(id = "P1", text = "email me at x@y.com",
                   stringsAsFactors = FALSE)
  expect_s3_class(scan_pii(df), "pii_scan")
})

test_that("scan_pii result has tier and occurrence columns", {
  df <- data.frame(id = "P1", text = "email me at x@y.com",
                   stringsAsFactors = FALSE)
  result <- scan_pii(df)
  expect_true(all(c("tier", "occurrence") %in% names(result)))
})

test_that("auto patterns are tagged tier=auto", {
  df <- data.frame(id = "P1", text = "user@example.com",
                   stringsAsFactors = FALSE)
  result <- scan_pii(df)
  expect_equal(unique(result$tier[result$pattern == "email"]), "auto")
})

test_that("disclosure patterns are tagged tier=disclosure", {
  df <- data.frame(id = "P1", text = "My name is Alice",
                   stringsAsFactors = FALSE)
  result <- scan_pii(df)
  expect_equal(unique(result$tier[result$pattern == "name_disclosure"]),
               "disclosure")
})

test_that("occurrence numbers multiple matches within same id+pattern", {
  df <- data.frame(
    id   = "P1",
    text = "My name is Alice. A friend: my name is Bob.",
    stringsAsFactors = FALSE
  )
  result <- scan_pii(df)
  disc <- result[result$pattern == "name_disclosure", ]
  expect_equal(sort(disc$occurrence), c(1L, 2L))
})

test_that("scan_pii context includes words after disclosure trigger", {
  df <- data.frame(id = "P1", text = "My name is Sarah Johnson and I felt lost.",
                   stringsAsFactors = FALSE)
  result <- scan_pii(df)
  expect_true(grepl("Sarah", result$match[result$pattern == "name_disclosure"]))
})

test_that("scan_pii returns empty structure for clean text", {
  df <- data.frame(id = "P1", text = "Today was a good day.",
                   stringsAsFactors = FALSE)
  result <- suppressMessages(scan_pii(df))
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "pii_scan")
})

test_that("scan_pii respects custom auto and disclosure patterns", {
  df <- data.frame(id = "P1", text = "ID: 123456789",
                   stringsAsFactors = FALSE)
  result <- scan_pii(df,
                     auto_patterns       = c(nine_digit = "\\b\\d{9}\\b"),
                     disclosure_patterns = character(0))
  expect_true("nine_digit" %in% result$pattern)
  expect_equal(unique(result$tier), "auto")
})

# ── pii_approve ───────────────────────────────────────────────────────────────

test_that("pii_approve creates a pii_approvals object", {
  a <- pii_approve(id = "P01", pattern = "name_disclosure")
  expect_s3_class(a, "pii_approvals")
  expect_equal(nrow(a), 1L)
  expect_equal(a$id, "P01")
  expect_equal(a$pattern, "name_disclosure")
  expect_true(is.na(a$occurrence))
})

test_that("pii_approve accumulates rows", {
  a <- pii_approve(id = "P01", pattern = "name_disclosure")
  a <- pii_approve(a, id = "P02", pattern = "location_phrase", occurrence = 1L)
  expect_equal(nrow(a), 2L)
  expect_equal(a$id, c("P01", "P02"))
})

test_that("pii_approve errors when approved is not a pii_approvals object", {
  expect_error(pii_approve(list(), id = "P01"), "pii_approvals")
})

test_that("scan_pii suppresses approved findings from main result", {
  df <- data.frame(
    id   = "P01",
    text = "My name is Sarah Johnson and I live in London.",
    stringsAsFactors = FALSE
  )
  approved <- pii_approve(id = "P01", pattern = "name_disclosure")
  result   <- scan_pii(df, approved = approved)
  expect_false(any(result$pattern == "name_disclosure"))
  expect_true(any(result$pattern == "location_phrase"))
})

test_that("approved findings appear in approved_findings attribute", {
  df <- data.frame(id = "P01", text = "My name is Alice.",
                   stringsAsFactors = FALSE)
  approved <- pii_approve(id = "P01", pattern = "name_disclosure", reason = "test")
  result   <- scan_pii(df, approved = approved)
  appr     <- attr(result, "approved_findings")
  expect_equal(nrow(appr), 1L)
  expect_equal(appr$pattern, "name_disclosure")
})

test_that("scan_pii messages when all findings are approved", {
  df <- data.frame(id = "P01", text = "My name is Alice.",
                   stringsAsFactors = FALSE)
  approved <- pii_approve(id = "P01", pattern = "name_disclosure")
  expect_message(scan_pii(df, approved = approved), "approved")
})

test_that("pii_approve with NULL occurrence approves all occurrences", {
  df <- data.frame(
    id   = "P01",
    text = "My name is Alice. Also my name is Bob.",
    stringsAsFactors = FALSE
  )
  approved <- pii_approve(id = "P01", pattern = "name_disclosure")
  result   <- scan_pii(df, approved = approved)
  expect_false(any(result$pattern == "name_disclosure"))
})

test_that("pii_approve with specific occurrence suppresses only that occurrence", {
  df <- data.frame(
    id   = "P01",
    text = "My name is Alice. Also my name is Bob.",
    stringsAsFactors = FALSE
  )
  approved <- pii_approve(id = "P01", pattern = "name_disclosure", occurrence = 1L)
  result   <- scan_pii(df, approved = approved)
  disc <- result[result$pattern == "name_disclosure", ]
  expect_equal(nrow(disc), 1L)
  expect_equal(disc$occurrence, 2L)
})

# ── redact_pii ────────────────────────────────────────────────────────────────

test_that("redact_pii replaces email", {
  df  <- data.frame(id = "P1", text = "Email: user@example.com",
                    stringsAsFactors = FALSE)
  out <- redact_pii(df)
  expect_false(grepl("user@example.com", out$text))
  expect_true(grepl("\\[email redacted\\]", out$text))
})

test_that("redact_pii does NOT redact disclosure patterns by default", {
  df  <- data.frame(id = "P1", text = "My name is Alice Smith.",
                    stringsAsFactors = FALSE)
  out <- redact_pii(df)
  expect_true(grepl("My name is Alice Smith", out$text))
})

test_that("redact_pii replaces multiple occurrences in one text", {
  df  <- data.frame(id = "P1", text = "a@b.com and c@d.com",
                    stringsAsFactors = FALSE)
  out <- redact_pii(df)
  expect_equal(lengths(regmatches(out$text,
                                  gregexpr("\\[email redacted\\]", out$text))), 2L)
})

test_that("redact_pii leaves clean text unchanged", {
  df  <- data.frame(id = "P1", text = "Nothing sensitive here.",
                    stringsAsFactors = FALSE)
  out <- redact_pii(df)
  expect_equal(out$text, df$text)
})

# ── redact_words ──────────────────────────────────────────────────────────────

test_that("redact_words removes words after trigger, keeps trigger intact", {
  df  <- data.frame(id = "P1", text = "It was hard. My name is Sarah Johnson.",
                    stringsAsFactors = FALSE)
  out <- redact_words(df, id = "P1", pattern = "name_disclosure", n_words = 2)
  expect_false(grepl("Sarah", out$text))
  expect_true(grepl("My name is", out$text))
  expect_true(grepl("\\[name disclosure redacted\\]", out$text))
})

test_that("redact_words original value does not appear in function call", {
  df  <- data.frame(id = "P1", text = "My name is Alice Bob Carol.",
                    stringsAsFactors = FALSE)
  out <- redact_words(df, id = "P1", pattern = "name_disclosure", n_words = 3)
  expect_false(grepl("Alice|Bob|Carol", out$text))
})

test_that("redact_words replaces all occurrences when occurrence is NULL", {
  df  <- data.frame(
    id   = "P1",
    text = "My name is Alice here. Later my name is Bob too.",
    stringsAsFactors = FALSE
  )
  out <- redact_words(df, id = "P1", pattern = "name_disclosure", n_words = 1)
  expect_false(grepl("Alice|Bob", out$text))
  expect_equal(
    lengths(regmatches(out$text,
                       gregexpr("\\[name disclosure redacted\\]", out$text))), 2L
  )
})

test_that("redact_words targets specific occurrence", {
  df  <- data.frame(
    id   = "P1",
    text = "My name is Alice here. Later my name is Bob too.",
    stringsAsFactors = FALSE
  )
  out <- redact_words(df, id = "P1", pattern = "name_disclosure",
                      n_words = 1, occurrence = 2)
  expect_true(grepl("Alice", out$text))
  expect_false(grepl("Bob", out$text))
})

test_that("redact_words errors on unknown id", {
  df <- data.frame(id = "P1", text = "text", stringsAsFactors = FALSE)
  expect_error(redact_words(df, id = "P99", pattern = "name_disclosure",
                             n_words = 1), "P99")
})

test_that("redact_words errors on out-of-range occurrence", {
  df <- data.frame(id = "P1", text = "My name is Alice.",
                   stringsAsFactors = FALSE)
  expect_error(
    redact_words(df, id = "P1", pattern = "name_disclosure",
                 n_words = 1, occurrence = 5),
    "Occurrence 5"
  )
})

# ── score_many PII integration ────────────────────────────────────────────────

test_that("score_many writes _pii.csv when PII detected", {
  df <- data.frame(id = "P1", text = "email me at test@example.com",
                   stringsAsFactors = FALSE)

  old_fn <- get("call_openai", envir = globalenv())
  assign("call_openai", function(...) '{"score":1}', envir = globalenv())
  on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

  withr::with_tempdir({
    writeLines("Rate this: {{text}}", "rubric.txt")
    expect_warning(
      score_many(df, "rubric.txt", run_params(), output_dir = "out",
                 pii_check = TRUE),
      "PII"
    )
    expect_true(file.exists("out/rubric_pii.csv"))
    pii <- read.csv("out/rubric_pii.csv")
    expect_true("email" %in% pii$pattern)
    expect_true("tier"  %in% names(pii))
  })
})

test_that("score_many skips PII check when pii_check = FALSE", {
  df <- data.frame(id = "P1", text = "email me at test@example.com",
                   stringsAsFactors = FALSE)

  old_fn <- get("call_openai", envir = globalenv())
  assign("call_openai", function(...) '{"score":1}', envir = globalenv())
  on.exit(assign("call_openai", old_fn, envir = globalenv()), add = TRUE)

  withr::with_tempdir({
    writeLines("Rate this: {{text}}", "rubric.txt")
    expect_no_warning(
      score_many(df, "rubric.txt", run_params(), output_dir = "out",
                 pii_check = FALSE)
    )
    expect_false(file.exists("out/rubric_pii.csv"))
  })
})
