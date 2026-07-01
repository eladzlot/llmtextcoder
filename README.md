# llmtextcoder

An R package for scoring short texts against a rubric using an LLM. You write
the rubric as a plain-text template; the package handles the API call and
returns structured scores as a flat CSV.

## Installation

```r
# install.packages("remotes")
remotes::install_github("eladzlot/llmtextcoder")
```

## Quick start

```r
library(llmtextcoder)

# Store your key in a gitignored .env file: OPENAI_API_KEY=sk-...
readRenviron(".env")

params <- run_params(model = "gpt-4o", temperature = 0)

rubric <- paste(readLines("prompts/rubric_v1.txt"), collapse = "\n")

# Preview what the model will see (no API call)
preview_prompt(rubric, list(text = "Some participant text."))

# Score a single row interactively
result <- score_one(rubric, list(text = "Some participant text."), params)
print_result(result$raw, result$scores)

# Score a data frame, results written to results/rubric_v1.csv
df <- read.csv("participants.csv")
score_many(df, rubric, "rubric_v1", params, output_dir = "results")

# Check progress
status(df, "rubric_v1", params, output_dir = "results")
```

See `vignette("running-a-study")` for a full walkthrough.

## Rubric template format

Templates are plain text strings with `{{placeholder}}` markers for each
column of participant data you want to inject. The model must be instructed
to return JSON (required for structured output):

```
Rate the following text on a 1–5 scale for clarity.
Respond ONLY with: {"clarity": <1-5>}

Text: {{text}}
```

You can use as many placeholders as your rubric needs:

```
Condition: {{condition}}
Response: {{text}}

Rate the response above on clarity (1–5).
Respond ONLY with: {"clarity": <1-5>}
```

Each placeholder must match a column in your data frame.

## Key design principles

- **The template is the scientific instrument.** Version it by filename
  (`rubric_v1.txt`, `rubric_v2.txt`); never edit in place once data has been
  collected against it.
- **One output file per rubric version.** `score_many()` writes to
  `<output_dir>/<output_name>.csv` — the filename encodes the rubric used.
- **Input texts are not stored in the output CSV.** Merge on `id` to recover
  them. This prevents accidental leakage when sharing results.
- **Crash-safe.** Results are appended row-by-row. Restart a run and
  already-scored rows are skipped automatically.
- **Provenance on every row.** Each CSV row records `prompt_version`,
  `model`, `temperature`, and `scored_at`.

## Using with an AI assistant

If you want to use an AI assistant (Claude, Copilot, etc.) to help you write
scoring scripts or interpret results, paste the contents of
[`inst/ai-context.md`](inst/ai-context.md) into the chat. It contains the
full function reference, a worked example, and the key rules the AI needs to
give accurate help.
