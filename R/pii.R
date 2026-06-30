#' PII patterns that can be redacted automatically
#'
#' Returns a named character vector of patterns where the regex captures the
#' complete identifying value (email address, phone number, etc.). These are
#' safe to replace globally with [redact_pii()] without human review.
#'
#' @return A named character vector of Perl-compatible regex patterns.
#' @export
#' @seealso [disclosure_pii_patterns()], [default_pii_patterns()]
auto_pii_patterns <- function() {
  c(
    email         = "\\b[\\w._%+\\-]+@[\\w.\\-]+\\.[a-zA-Z]{2,}\\b",
    phone         = "\\b(\\+?\\d[\\d\\s\\-().]{6,}\\d)\\b",
    prolific_id   = "\\b[0-9a-f]{24}\\b",
    credit_card   = "\\b(?:\\d[ \\-]?){13,16}\\b",
    ip_address    = "\\b\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\b",
    url           = "https?://\\S+|www\\.\\S+",
    social_handle = "@[A-Za-z0-9_]{2,}"
  )
}

#' PII patterns that require per-row review before redaction
#'
#' Returns a named character vector of patterns that match a trigger phrase
#' ("my name is", "I live in") but not the identifying words that follow.
#' How many words to redact depends on context, so these require human judgment
#' via [redact_words()] rather than global replacement.
#'
#' @return A named character vector of Perl-compatible regex patterns.
#' @export
#' @seealso [auto_pii_patterns()], [default_pii_patterns()], [redact_words()]
disclosure_pii_patterns <- function() {
  c(
    name_disclosure = "(?i)\\b(my name is|I(?:'m| am) called|call me|I(?:'m| am) known as)\\b",
    location_phrase = "(?i)\\b(I live (in|at|on)|I(?:'m| am) from|my address is|I(?:'m| am) based in)\\b"
  )
}

#' All default PII patterns
#'
#' Returns the union of [auto_pii_patterns()] and [disclosure_pii_patterns()].
#' Used by [scan_pii()] to scan for all pattern types at once.
#'
#' To customise, modify the two constituent sets and pass them separately to
#' [scan_pii()]:
#'
#' ```r
#' auto <- auto_pii_patterns()
#' auto["national_id"] <- "\\b\\d{9}\\b"
#'
#' disc <- disclosure_pii_patterns()
#' disc <- disc[names(disc) != "location_phrase"]
#'
#' scan_pii(df, auto_patterns = auto, disclosure_patterns = disc)
#' ```
#'
#' @return A named character vector of Perl-compatible regex patterns.
#' @export
default_pii_patterns <- function() {
  c(auto_pii_patterns(), disclosure_pii_patterns())
}

#' Record PII findings that have been reviewed and need no redaction
#'
#' Creates or extends an approvals list. Pass the result to [scan_pii()] via
#' the `approved` argument to suppress known false-positives from the output.
#' The approvals list belongs in your analysis script — it is the permanent
#' record that each finding was reviewed.
#'
#' @param approved An existing `pii_approvals` object to extend, or `NULL` to
#'   start a new one.
#' @param id The participant identifier of the approved finding.
#' @param pattern Pattern name to approve, or `NULL` to approve all patterns
#'   for this id.
#' @param occurrence Integer occurrence to approve, or `NULL` to approve all
#'   occurrences of the pattern.
#' @param reason A brief note explaining why the finding is not identifying.
#'   Optional but recommended for the audit trail.
#'
#' @return A `pii_approvals` data frame.
#' @export
#' @seealso [scan_pii()]
#'
#' @examples
#' # First approval — approved starts as NULL
#' approved <- pii_approve(id = "P041", pattern = "location_phrase",
#'                         reason = "city name only, not identifying")
#' # Add another
#' approved <- pii_approve(approved, id = "P023", pattern = "name_disclosure",
#'                         occurrence = 2, reason = "refers to fictional character")
pii_approve <- function(approved = NULL, id, pattern = NULL, occurrence = NULL,
                        reason = "") {
  new_row <- data.frame(
    id         = as.character(id),
    pattern    = if (is.null(pattern)) NA_character_ else as.character(pattern),
    occurrence = if (is.null(occurrence)) NA_integer_  else as.integer(occurrence),
    reason     = as.character(reason),
    stringsAsFactors = FALSE
  )
  if (is.null(approved)) {
    return(structure(new_row, class = c("pii_approvals", "data.frame")))
  }
  if (!inherits(approved, "pii_approvals"))
    stop(sprintf(
      "'approved' must be a pii_approvals object returned by pii_approve(), got %s.\n  Start with: approved <- pii_approve(id = \"P01\", pattern = \"name_disclosure\", reason = \"...\")",
      class(approved)[1L]
    ))
  structure(rbind(approved, new_row), class = c("pii_approvals", "data.frame"))
}

#' @noRd
.is_approved <- function(id, pattern, occurrence, approved) {
  if (is.null(approved) || nrow(approved) == 0) return(FALSE)
  any(
    approved$id == id &
    (is.na(approved$pattern)    | approved$pattern    == pattern) &
    (is.na(approved$occurrence) | approved$occurrence == occurrence)
  )
}

#' @noRd
.match_with_context <- function(text, pattern, after = 50) {
  m <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (m[1] == -1) return(character(0))

  lengths <- attr(m, "match.length")
  mapply(function(start, len) {
    matched  <- substr(text, start, start + len - 1)
    tail_raw <- substr(text, start + len, start + len + after - 1)
    tail_str <- if (nchar(tail_raw) == after) {
      last_space <- max(c(0, gregexpr("\\s", tail_raw)[[1]]))
      if (last_space > 0) paste0(substr(tail_raw, 1, last_space - 1), "…")
      else paste0(tail_raw, "…")
    } else {
      tail_raw
    }
    paste0(matched, tail_str)
  }, m, lengths, USE.NAMES = FALSE)
}

#' @noRd
.scan_tier <- function(df, patterns, tier) {
  rows <- lapply(seq_len(nrow(df)), function(i) {
    id   <- as.character(df$id[i])
    text <- df$text[i]
    do.call(rbind, lapply(names(patterns), function(pat_name) {
      matches <- .match_with_context(text, patterns[[pat_name]])
      if (length(matches) == 0) return(NULL)
      data.frame(
        id         = id,
        pattern    = pat_name,
        tier       = tier,
        occurrence = seq_along(matches),
        match      = matches,
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

#' Scan a data frame of texts for potentially identifying information
#'
#' Searches each text against two sets of patterns:
#' - **Auto** patterns (emails, phones, etc.) capture the full identifying
#'   value and can be cleared with [redact_pii()].
#' - **Disclosure** patterns (name and location phrases) capture only a trigger
#'   phrase; the words that follow require per-row human judgment via
#'   [redact_words()].
#'
#' The printed output separates the two tiers and generates ready-to-fill
#' [redact_words()] and [pii_approve()] calls for all disclosure matches.
#'
#' Once you have reviewed a finding and decided it does not need redaction,
#' record that decision with [pii_approve()] and pass the result back here via
#' `approved`. Approved findings are suppressed from the main output and shown
#' in a separate "Reviewed and approved" section.
#'
#' @param df A data frame with at minimum columns `id` and `text`.
#' @param approved A `pii_approvals` object from [pii_approve()], or `NULL`
#'   (default). Matching findings are excluded from the actionable output.
#' @param auto_patterns Named character vector of auto-redactable patterns.
#'   Default: [auto_pii_patterns()].
#' @param disclosure_patterns Named character vector of disclosure patterns.
#'   Default: [disclosure_pii_patterns()].
#'
#' @return A `pii_scan` data frame with columns `id`, `pattern`, `tier`
#'   (`"auto"` or `"disclosure"`), `occurrence` (integer, 1-based per
#'   id–pattern pair), and `match` (trigger plus trailing context). Approved
#'   findings are stored in the `"approved_findings"` attribute.
#' @export
#' @seealso [pii_approve()], [redact_pii()], [redact_words()]
#'
#' @examples
#' df <- data.frame(
#'   id   = c("P01", "P02"),
#'   text = c("Contact me at jane@example.com",
#'            "My name is Sarah and I live in London"),
#'   stringsAsFactors = FALSE
#' )
#' scan_pii(df)
scan_pii <- function(df,
                     approved            = NULL,
                     auto_patterns       = auto_pii_patterns(),
                     disclosure_patterns = disclosure_pii_patterns()) {
  .check_df(df, required_cols = "text")

  empty_scan <- function() {
    structure(
      data.frame(id = character(), pattern = character(), tier = character(),
                 occurrence = integer(), match = character(),
                 stringsAsFactors = FALSE),
      class = c("pii_scan", "data.frame")
    )
  }
  empty_findings <- data.frame(
    id = character(), pattern = character(), tier = character(),
    occurrence = integer(), match = character(),
    stringsAsFactors = FALSE
  )

  result <- rbind(
    .scan_tier(df, auto_patterns,       "auto"),
    .scan_tier(df, disclosure_patterns, "disclosure")
  )

  if (is.null(result) || nrow(result) == 0) {
    message("No PII patterns detected.")
    out <- empty_scan()
    attr(out, "approved_findings") <- empty_findings
    return(invisible(out))
  }

  # Split into approved and unapproved
  if (!is.null(approved)) {
    mask <- mapply(
      .is_approved,
      result$id, result$pattern, result$occurrence,
      MoreArgs = list(approved = approved)
    )
    approved_findings <- result[mask,  , drop = FALSE]
    result            <- result[!mask, , drop = FALSE]
  } else {
    approved_findings <- empty_findings
  }

  if (nrow(result) == 0) {
    n <- nrow(approved_findings)
    message(sprintf("No unreviewed PII detected (%d finding(s) approved).", n))
    out <- empty_scan()
    attr(out, "approved_findings") <- approved_findings
    return(invisible(out))
  }

  out <- structure(result, class = c("pii_scan", "data.frame"))
  attr(out, "approved_findings") <- approved_findings
  out
}

#' @noRd
.pii_suggested_code <- function(x) {
  disc <- x[x$tier == "disclosure", , drop = FALSE]
  if (nrow(disc) == 0) return("")

  redact_lines  <- character(nrow(disc))
  approve_lines <- character(nrow(disc))

  for (i in seq_len(nrow(disc))) {
    row   <- disc[i, ]
    multi <- sum(disc$id == row$id & disc$pattern == row$pattern) > 1
    if (multi) {
      redact_lines[i] <- sprintf(
        'df <- redact_words(df, id = "%s", pattern = "%s", n_words = ?, occurrence = %d)',
        row$id, row$pattern, row$occurrence)
      approve_lines[i] <- sprintf(
        'approved <- pii_approve(approved, id = "%s", pattern = "%s", occurrence = %d, reason = "?")',
        row$id, row$pattern, row$occurrence)
    } else {
      redact_lines[i] <- sprintf(
        'df <- redact_words(df, id = "%s", pattern = "%s", n_words = ?)',
        row$id, row$pattern)
      approve_lines[i] <- sprintf(
        'approved <- pii_approve(approved, id = "%s", pattern = "%s", reason = "?")',
        row$id, row$pattern)
    }
  }

  paste(c(redact_lines, "", approve_lines), collapse = "\n")
}

#' Extract suggested redaction and approval code from a PII scan
#'
#' Returns the suggested `redact_words()` and `pii_approve()` calls from a
#' [scan_pii()] result as a single character string. Useful for copying the
#' code to your script programmatically rather than transcribing from the
#' printed output.
#'
#' @param x A `pii_scan` object returned by [scan_pii()].
#' @return A character string of R code, one call per line. Returns `""`
#'   invisibly if there are no disclosure findings.
#' @export
#' @seealso [scan_pii()], [redact_words()], [pii_approve()]
#'
#' @examples
#' df <- data.frame(
#'   id   = "P01",
#'   text = "My name is Sarah and I live in London.",
#'   stringsAsFactors = FALSE
#' )
#' result <- scan_pii(df)
#' code   <- pii_code(result)
#' cat(code)
#' # Copy to clipboard (requires the clipr package):
#' # clipr::write_clip(code)
pii_code <- function(x) {
  if (!inherits(x, "pii_scan"))
    stop(sprintf(
      "'x' must be a pii_scan object returned by scan_pii(), got %s.",
      class(x)[1L]
    ))
  code <- .pii_suggested_code(x)
  if (!nzchar(code)) return(invisible(""))
  code
}

#' @export
print.pii_scan <- function(x, ...) {
  appr       <- attr(x, "approved_findings")
  n_approved <- if (!is.null(appr)) nrow(appr) else 0L

  if (nrow(x) == 0) {
    cat("No PII detected.\n")
    return(invisible(x))
  }

  n_texts   <- length(unique(x$id))
  n_matches <- nrow(x)
  if (n_approved > 0) {
    cat(sprintf("PII scan: %d match(es) across %d text(s)  [%d approved]\n",
                n_matches, n_texts, n_approved))
  } else {
    cat(sprintf("PII scan: %d match(es) across %d text(s)\n", n_matches, n_texts))
  }

  auto <- x[x$tier == "auto",       , drop = FALSE]
  disc <- x[x$tier == "disclosure", , drop = FALSE]

  if (nrow(auto) > 0) {
    cat("\nAuto-redactable — use redact_pii() to clear:\n")
    for (i in seq_len(nrow(auto))) {
      cat(sprintf("  [%s] %s\n    \"%s\"\n",
                  auto$id[i], auto$pattern[i], auto$match[i]))
    }
  }

  if (nrow(disc) > 0) {
    cat("\nNeeds review — use redact_words() per row:\n")
    for (i in seq_len(nrow(disc))) {
      cat(sprintf("  [%s] %s #%d\n    \"%s\"\n",
                  disc$id[i], disc$pattern[i], disc$occurrence[i], disc$match[i]))
    }
    cat("\nSuggested calls — fill in n_words (or use pii_code() to copy all at once):\n")
    code <- .pii_suggested_code(x)
    for (line in strsplit(code, "\n")[[1]]) {
      cat(if (nzchar(line)) sprintf("  %s\n", line) else "\n")
    }
  }

  if (n_approved > 0) {
    cat("\nReviewed and approved — no action needed:\n")
    for (i in seq_len(nrow(appr))) {
      reason_str <- if (nzchar(appr$reason[i])) sprintf("  (%s)", appr$reason[i]) else ""
      cat(sprintf("  [%s] %s #%d%s\n    \"%s\"\n",
                  appr$id[i], appr$pattern[i], appr$occurrence[i],
                  reason_str, appr$match[i]))
    }
  }

  invisible(x)
}

#' Redact structural PII across all texts
#'
#' Replaces every match of each pattern in `patterns` with a labelled
#' placeholder such as `[email redacted]`. Replacement is done by regex —
#' the original values never appear in your code.
#'
#' Defaults to [auto_pii_patterns()] (emails, phones, Prolific IDs, etc.),
#' which capture the full identifying value and are safe to replace globally.
#' Disclosure patterns (name phrases, location phrases) are intentionally
#' excluded from the default; handle those with [redact_words()].
#'
#' @param df A data frame with at minimum columns `id` and `text`.
#' @param patterns Named character vector of patterns to redact. Default:
#'   [auto_pii_patterns()].
#'
#' @return A copy of `df` with matching strings replaced in `text`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id   = "P01",
#'   text = "Email me at jane@example.com or call 07911 123456.",
#'   stringsAsFactors = FALSE
#' )
#' redact_pii(df)
redact_pii <- function(df, patterns = auto_pii_patterns()) {
  .check_df(df, required_cols = "text")
  result <- df
  for (pat_name in names(patterns)) {
    placeholder <- sprintf("[%s redacted]", gsub("_", " ", pat_name))
    result$text <- gsub(patterns[[pat_name]], placeholder,
                        result$text, perl = TRUE)
  }
  result
}

#' Redact a disclosure phrase and the words that follow it in a single row
#'
#' For patterns that match only a trigger phrase ("my name is", "I live in"),
#' this function finds the match in one specific row and replaces the trigger
#' plus `n_words` following words with a placeholder. Use after reviewing the
#' output of [scan_pii()].
#'
#' The `occurrence` argument targets a specific match when a pattern fires
#' more than once in the same text. When `occurrence` is `NULL` (the default),
#' all occurrences are replaced with the same `n_words`.
#'
#' @param df A data frame with at minimum columns `id` and `text`.
#' @param id The participant identifier. Must match exactly one row.
#' @param pattern Name of the pattern to redact (must be a key in `patterns`).
#' @param n_words Number of words to consume after the trigger phrase.
#' @param occurrence Integer. Which occurrence to target when the pattern fires
#'   multiple times. `NULL` (default) replaces all occurrences.
#' @param patterns Named character vector of patterns. Default:
#'   [disclosure_pii_patterns()].
#'
#' @return A copy of `df` with the targeted text replaced in `text`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id   = "P01",
#'   text = "It was hard. My name is Sarah Johnson and I felt lost.",
#'   stringsAsFactors = FALSE
#' )
#' # Keeps "My name is", removes "Sarah Johnson":
#' # → "It was hard. My name is [name disclosure redacted] and I felt lost."
#' redact_words(df, id = "P01", pattern = "name_disclosure", n_words = 2)
redact_words <- function(df, id, pattern, n_words = 3, occurrence = NULL,
                         patterns = disclosure_pii_patterns()) {
  .check_df(df, required_cols = "text")

  idx <- which(as.character(df$id) == as.character(id))
  if (length(idx) == 0)
    stop(sprintf(
      "No row with id '%s' found in df.\n  Available ids (first 10): %s",
      id, paste(head(as.character(df$id), 10L), collapse = ", ")
    ))
  if (length(idx) > 1)
    stop(sprintf(
      "Found %d rows with id '%s'; ids must be unique.\n  Remove duplicate rows before calling redact_words().",
      length(idx), id
    ))

  pat <- patterns[[pattern]]
  if (is.null(pat))
    stop(sprintf(
      "Pattern '%s' not found.\n  Available patterns: %s\n  Pass a custom 'patterns' argument if you need a different pattern.",
      pattern, paste(names(patterns), collapse = ", ")
    ))

  text        <- df$text[idx]
  placeholder <- sprintf("[%s redacted]", gsub("_", " ", pattern))
  words_pat   <- paste0("^(?:\\s+\\S+){1,", n_words, "}")

  trig_m   <- gregexpr(pat, text, perl = TRUE)[[1]]
  trig_len <- attr(trig_m, "match.length")

  if (trig_m[1] == -1)
    stop(sprintf(
      "Pattern '%s' did not match anything in the text for id '%s'.\n  Text (first 150 chars): \"%s\"",
      pattern, id, substr(df$text[idx], 1L, 150L)
    ))

  targets <- if (is.null(occurrence)) {
    seq_along(trig_m)
  } else {
    if (occurrence > length(trig_m))
      stop(sprintf(
        "Pattern '%s' occurs %d time(s) in id '%s', but occurrence = %d was requested.\n  Use occurrence = NULL to replace all occurrences, or a value from 1 to %d.",
        pattern, length(trig_m), id, occurrence, length(trig_m)
      ))
    occurrence
  }

  for (occ in rev(targets)) {
    trig_end <- trig_m[occ] + trig_len[occ] - 1
    rest      <- substr(text, trig_end + 1, nchar(text))
    wm        <- regexpr(words_pat, rest, perl = TRUE)
    if (wm == -1) next
    wlen <- attr(wm, "match.length")
    text <- paste0(
      substr(text, 1, trig_end),
      placeholder,
      substr(text, trig_end + wlen + 1, nchar(text))
    )
  }

  df$text[idx] <- text
  df
}
