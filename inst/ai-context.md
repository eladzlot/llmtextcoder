# llmtextcoder — AI context

R package for LLM-assisted qualitative text coding. The researcher writes a
rubric as a plain-text prompt template; the package injects participant texts,
calls OpenAI, and returns structured scores as a flat CSV.

**Install:** `remotes::install_github("eladzlot/llmtextcoder")`

---

## Function reference

### Setup
```r
run_params(model = "gpt-4o", temperature = 0)
# Returns a run_params list. Pass to every API-calling function.
```

### Prompt
```r
build_prompt(template, data)
# Inject named list `data` into {{placeholder}} patterns in template string.

preview_prompt(template, data)
# Print compiled prompt with char/word counts. No API call.
```

### Single-row scoring
```r
score_one(template, data, params, api_key)
# Returns list(raw = "<json string>", scores = list(...))

print_result(raw, scores)
# Pretty-print raw JSON and parsed scores side by side.
```

### Batch scoring
```r
score_many(df, template, output_name, params, n = Inf,
           output_dir = "data", pii_check = TRUE, ...)
# Score a data frame. Appends one row per API call to
# <output_dir>/<output_name>.csv. Skip logic on id+model.
# Failures go to <output_dir>/<output_name>_errors.csv.

status(df, output_name, params, output_dir = "data")
# Print and return scored/failed/pending counts.
```

### Async batch (large datasets, 50% cost reduction)
```r
submit_batch(df, template_path, params, output_dir = "data")
# Upload all pending rows to OpenAI Batch API.

collect_batch(template_path, params, output_dir = "data")
# Poll/collect results. Call again later if still running.
# Note: submit_batch/collect_batch take a file *path*, not an inline string.
```

### PII
```r
scan_pii(df, auto_patterns, disclosure_patterns, approved)
# Scan df$text for PII. Returns pii_scan object (data frame).
# Two tiers: "auto" (emails, phones — safe to replace) and
# "disclosure" ("my name is" — needs manual review).

pii_code(scan_result)
# Print copy-pasteable redact_words() calls for disclosure findings.

redact_pii(df, patterns)
# Replace auto-tier PII globally with [pattern redacted] tokens.

redact_words(df, id, pattern, n_words, occurrence = NULL)
# Remove n_words after a disclosure trigger for a specific row.

pii_approve(approved = NULL, id, pattern, occurrence = NULL, reason = NULL)
# Record a deliberate decision not to redact a finding.
```

### Example dataset
```r
varieties_testimonials()
# 47 first-person narratives from William James (1902, public domain).
# Columns: id, text, theme, word_count, source
# Good for demos and testing rubrics without real participant data.
```

---

## Workflow

```r
library(llmtextcoder)
readRenviron(".env")   # loads OPENAI_API_KEY

# 1. Write a rubric (template string with {{placeholder}} markers)
rubric <- "
Rate the excerpt on emotional valence (1=positive, 5=negative).
Respond ONLY with JSON: {\"valence\": <1-5>}
Excerpt: {{text}}
"

# 2. Inspect the compiled prompt (free)
df <- varieties_testimonials()
preview_prompt(rubric, list(text = df$text[1]))

# 3. Pilot on one row
params <- run_params(model = "gpt-4o", temperature = 0)
result <- score_one(rubric, list(text = df$text[1]), params)
print_result(result$raw, result$scores)

# 4. Scan and redact PII (skip for public-domain data)
batch <- df[, c("id", "text")]
batch <- redact_pii(batch)
# For disclosure patterns: scan_pii(batch) → pii_code() → redact_words()

# 5. Batch score (n limits rows; safe to restart)
score_many(batch, rubric, "valence_v1", params, n = 5, pii_check = FALSE,
           output_dir = "results")
score_many(batch, rubric, "valence_v1", params, pii_check = FALSE,
           output_dir = "results")

# 6. Check progress
status(batch, "valence_v1", params, output_dir = "results")

# 7. Read results and merge with original data for analysis
scores <- read.csv("results/valence_v1.csv", stringsAsFactors = FALSE)
merged <- merge(scores, df[, c("id", "theme")], by = "id")
```

---

## Output schema

`results/<output_name>.csv` — one row per successful API call:

| Column | Content |
|--------|---------|
| `id` | From input `df$id` |
| *(score columns)* | One per JSON key the model returned |
| `raw` | Exact JSON string from the model |
| `prompt_version` | The `output_name` argument |
| `model` | e.g. `gpt-4o` |
| `temperature` | From `run_params()` |
| `scored_at` | ISO 8601 timestamp |

Input texts are **not** written to the CSV (privacy). Merge on `id` to
recover them.

`results/<output_name>_errors.csv` — failed rows: `id`, `error`, `model`,
`scored_at`. These are retried automatically on the next `score_many()` call.

---

## Key rules

- **template** is always an inline string. Load from file with
  `paste(readLines("prompts/rubric.txt"), collapse = "\n")`.
- The rubric **must** tell the model to respond in JSON (OpenAI requirement).
- Every `{{placeholder}}` must match a column name in `df`.
- **Never edit a rubric in place** once data has been collected. Save a new
  version (`rubric_v2.txt`) and use a new `output_name`.
- **Never mix rubric versions** in one output CSV.
- Skip logic matches on `id` + `model`. Restarting is always safe.
- API key goes in `.env` (gitignored), never in scripts.
- `raw/` directory with participant data must be gitignored — never commit it.
