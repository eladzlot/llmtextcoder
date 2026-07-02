for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
if (file.exists(".env")) readRenviron(".env")

PROVENANCE <- c("id", "raw", "prompt_version", "model", "temperature", "scored_at", "coder_notes")

score_cols <- function(df) setdiff(names(df), PROVENANCE)

# ── Configuration (change VERSION to test a new rubric) ───────────────────────
VERSION <- "rumination_v1"
N_PILOT <- 5     # set to Inf to skip pilot and score everything in one pass
FORCE   <- TRUE  # set to FALSE to resume an interrupted run

params <- run_params(model = "gpt-4o", temperature = 0)
rubric <- paste(readLines(sprintf("prompts/%s.txt", VERSION)), collapse = "\n")
batch  <- varieties_testimonials()[, c("id", "text")]

# ── Pilot ─────────────────────────────────────────────────────────────────────
score_many(batch, rubric, VERSION, params, n = N_PILOT, force = FORCE, pii_check = FALSE, output_dir = "data")
pilot <- read.csv(sprintf("data/%s.csv", VERSION), stringsAsFactors = FALSE)
print(pilot[, c("id", score_cols(pilot))])

# ── Full batch ────────────────────────────────────────────────────────────────
score_many(batch, rubric, VERSION, params, pii_check = FALSE, output_dir = "data")
status(batch, VERSION, params, output_dir = "data")

# ── Summary by theme ──────────────────────────────────────────────────────────
results  <- read.csv(sprintf("data/%s.csv", VERSION), stringsAsFactors = FALSE)
df       <- varieties_testimonials()
results  <- merge(results, df[, c("id", "theme")], by = "id")
cols     <- score_cols(results)
num_cols <- cols[sapply(results[, cols, drop = FALSE], is.numeric)]
results$rumination <- rowMeans(results[, num_cols])
print(round(sort(tapply(results$rumination, results$theme, mean), decreasing = TRUE), 2))
