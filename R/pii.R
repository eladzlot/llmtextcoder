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
#' [redact_words()] calls for all disclosure matches, with `n_words = ?` as
#' the only blank to complete.
#'
#' @param df A data frame with at minimum columns `id` and `text`.
#' @param auto_patterns Named character vector of auto-redactable patterns.
#'   Default: [auto_pii_patterns()].
#' @param disclosure_patterns Named character vector of disclosure patterns.
#'   Default: [disclosure_pii_patterns()].
#'
#' @return A `pii_scan` data frame with columns `id`, `pattern`, `tier`
#'   (`"auto"` or `"disclosure"`), `occurrence` (integer, 1-based per
#'   id–pattern pair), and `match` (trigger plus trailing context).
#' @export
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
                     auto_patterns       = auto_pii_patterns(),
                     disclosure_patterns = disclosure_pii_patterns()) {
  .check_df(df)

  result <- rbind(
    .scan_tier(df, auto_patterns,       "auto"),
    .scan_tier(df, disclosure_patterns, "disclosure")
  )

  if (is.null(result) || nrow(result) == 0) {
    message("No PII patterns detected.")
    return(invisible(
      structure(
        data.frame(id = character(), pattern = character(), tier = character(),
                   occurrence = integer(), match = character(),
                   stringsAsFactors = FALSE),
        class = c("pii_scan", "data.frame")
      )
    ))
  }

  structure(result, class = c("pii_scan", "data.frame"))
}

#' @export
print.pii_scan <- function(x, ...) {
  if (nrow(x) == 0) {
    cat("No PII detected.\n")
    return(invisible(x))
  }

  n_texts   <- length(unique(x$id))
  n_matches <- nrow(x)
  cat(sprintf("PII scan: %d match(es) across %d text(s)\n", n_matches, n_texts))

  auto <- x[x$tier == "auto",        , drop = FALSE]
  disc <- x[x$tier == "disclosure",  , drop = FALSE]

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

    cat("\nSuggested redact_words() calls — fill in n_words:\n")
    for (i in seq_len(nrow(disc))) {
      row   <- disc[i, ]
      multi <- sum(disc$id == row$id & disc$pattern == row$pattern) > 1
      if (multi) {
        cat(sprintf(
          '  df <- redact_words(df, id = "%s", pattern = "%s", n_words = ?, occurrence = %d)\n',
          row$id, row$pattern, row$occurrence
        ))
      } else {
        cat(sprintf(
          '  df <- redact_words(df, id = "%s", pattern = "%s", n_words = ?)\n',
          row$id, row$pattern
        ))
      }
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
  .check_df(df)
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
#' redact_words(df, id = "P01", pattern = "name_disclosure", n_words = 2)
redact_words <- function(df, id, pattern, n_words = 3, occurrence = NULL,
                         patterns = disclosure_pii_patterns()) {
  .check_df(df)

  idx <- which(as.character(df$id) == as.character(id))
  if (length(idx) == 0) stop(sprintf("No row with id '%s'", id))
  if (length(idx) > 1) stop(sprintf("Multiple rows with id '%s'; ids must be unique", id))

  pat <- patterns[[pattern]]
  if (is.null(pat)) stop(sprintf("Pattern '%s' not found in patterns", pattern))

  text         <- df$text[idx]
  extended_pat <- paste0("(?:", pat, ")(?:\\s+\\S+){1,", n_words, "}")
  placeholder  <- sprintf("[%s redacted]", gsub("_", " ", pattern))

  if (is.null(occurrence)) {
    df$text[idx] <- gsub(extended_pat, placeholder, text, perl = TRUE)
  } else {
    m <- gregexpr(extended_pat, text, perl = TRUE)[[1]]
    if (m[1] == -1 || occurrence > length(m)) {
      stop(sprintf("Occurrence %d of pattern '%s' not found in id '%s'",
                   occurrence, pattern, id))
    }
    start        <- m[occurrence]
    len          <- attr(m, "match.length")[occurrence]
    df$text[idx] <- paste0(
      substr(text, 1, start - 1),
      placeholder,
      substr(text, start + len, nchar(text))
    )
  }

  df
}
