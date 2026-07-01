# 05_rumination_james.R
# Score all James testimonials with the rumination rubric.
#
# Run from the project root:
#     Rscript scripts/05_rumination_james.R

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
if (file.exists(".env")) readRenviron(".env")

params <- run_params(model = "gpt-4o", temperature = 0)

# ── 1. Load data ──────────────────────────────────────────────────────────────
df <- varieties_testimonials()
batch <- df[, c("id", "text")]

# ── 2. PII scan ───────────────────────────────────────────────────────────────
# James testimonials are public domain and contain no participant data,
# so we skip the scan. For real participant data, replace this block with:
#
#   pii <- scan_pii(batch)
#   pii_code(pii)          # prints redact_words() calls to paste into script
#   batch <- redact_pii(batch)
#   batch <- redact_words(batch, id = "P01", pattern = "name_disclosure", n_words = 2)
#   # ... one call per disclosure finding, then re-run scan_pii() to confirm clean

# ── 3. Load rubric ────────────────────────────────────────────────────────────
rubric <- paste(readLines("prompts/rumination_v1.txt"), collapse = "\n")

# ── 4. Pilot: score 3 rows and inspect ───────────────────────────────────────
cat("── Pilot (3 rows) ────────────────────────────────────────────────────────\n")
score_many(batch, rubric, "rumination_v1", params,
           n = 3, pii_check = FALSE, output_dir = "data")

pilot <- read.csv("data/rumination_v1.csv", stringsAsFactors = FALSE)
score_cols <- c("repetition", "negativity", "stagnation", "passivity", "abstractness")
cat("Pilot scores:\n")
print(pilot[, c("id", score_cols)])
cat("\nDo the scores look right? If not, revise the rubric and rename it v2.\n\n")

# ── 5. Full batch ─────────────────────────────────────────────────────────────
cat("── Full batch ────────────────────────────────────────────────────────────\n")
score_many(batch, rubric, "rumination_v1", params,
           pii_check = FALSE, output_dir = "data")

status(batch, "rumination_v1", params, output_dir = "data")

# ── 6. Quick summary ──────────────────────────────────────────────────────────
results <- read.csv("data/rumination_v1.csv", stringsAsFactors = FALSE)
results <- merge(results, df[, c("id", "theme")], by = "id")

score_cols <- c("repetition", "negativity", "stagnation", "passivity", "abstractness")
results$rumination <- rowMeans(results[, score_cols])

cat("\nMean rumination by theme:\n")
theme_means <- sort(tapply(results$rumination, results$theme, mean), decreasing = TRUE)
print(round(theme_means, 2))
