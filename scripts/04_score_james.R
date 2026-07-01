# 04_score_james.R
# Score testimonials from the bundled James dataset with a clinical rubric.
#
# Demonstrates: varieties_testimonials(), score_one() with an inline template,
# and score_many() for a batch of 10 with incremental output and skip logic.
#
# Run from the project root:
#     Rscript scripts/04_score_james.R

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
if (file.exists(".env")) readRenviron(".env")

params <- run_params()
df     <- varieties_testimonials()

# ── Part 1: single testimonial with an inline rubric ─────────────────────────
entry <- df[df$id == "james_10", ]   # French asylum patient — severe depression
cat("── Single score ──────────────────────────────────────────────────────────\n")
cat("ID:", entry$id, "| theme:", entry$theme, "| words:", entry$word_count, "\n\n")

rubric <- "
You are an expert clinical psychologist coding first-person narrative accounts.
Rate the excerpt below on three psychological dimensions.

Dimension 1 — EMOTIONAL VALENCE (1-5)
1 = strongly positive affect  5 = strongly negative affect

Dimension 2 — HOPELESSNESS (1-5)
1 = clear hope or agency  5 = complete hopelessness, no future orientation

Dimension 3 — SOMATIC DISTRESS (1-5)
1 = no bodily complaints  5 = pervasive physical suffering described

Respond ONLY with a JSON object, nothing else:
{\"valence\": <1-5>, \"hopelessness\": <1-5>, \"somatic_distress\": <1-5>}

Excerpt:
{{text}}
"

preview_prompt(rubric, list(text = entry$text))
result <- score_one(rubric, list(text = entry$text), params)
print_result(result$raw, result$scores)

# ── Part 2: batch-score 10 testimonials ──────────────────────────────────────
cat("\n── Batch score (10 testimonials) ─────────────────────────────────────────\n")
batch <- head(df[, c("id", "text")], 10)

clinical_rubric <- read_template("prompts/clinical_v1.txt")

score_many(batch, clinical_rubric, "clinical_v1",
           params     = params,
           output_dir = "data")

status(batch, "clinical_v1", params, output_dir = "data")
