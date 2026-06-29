# 03_score_batch.R
# Demonstrates score_many(): incremental batch scoring with skip logic.
# Results are appended to data/rumination_v1.csv as each row completes.
#
# Run from the project root:
#     Rscript scripts/03_score_batch.R

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
if (file.exists(".env")) readRenviron(".env")

# Input data frame — in real use, replace with read.csv("raw/your_data.csv")
df <- data.frame(
  id   = c("mindful_01", "dull_01", "ruminate_01"),
  text = c(
    "I wake just before my alarm and rest for a moment, noticing the gentle rise and fall of my breathing. The room is cool, and pale morning light filters through the curtains. I stretch, feeling stiffness in my shoulders soften. My feet meet the cool floor as I walk to the bathroom. Cold water wakes my face, and the fresh taste of mint lingers while I brush my teeth. In the kitchen, I open the window as the kettle boils. Fresh air carries the scent of damp grass. I hold my warm coffee mug with both hands and watch steam drift upward. My phone buzzes, but I finish my first sip before looking at it. Most messages can wait. A warm shower loosens the last traces of sleep. As I get dressed, I feel alert and ready. I lock the door, take a deep breath of the cool morning air, and begin walking, noticing the quiet sounds of the neighborhood waking up.",
    "My alarm rings at 6:30, and I turn it off. I get out of bed and walk to the bathroom. I brush my teeth, wash my face, and dry it with a towel. Then I go to the kitchen. I boil water, make coffee, and prepare oatmeal with a sliced banana. I sit at the table and eat breakfast while checking my phone. There are a few emails and messages, but none of them need an immediate response. After eating, I rinse my dishes and put them in the sink. I take a shower, wash my hair, and get dressed in a shirt, jeans, and sneakers. I check the weather, pack my wallet, keys, phone, and laptop into my bag, and make sure the lights are off. I lock the front door and walk to the bus stop. Several people are already waiting, and traffic is beginning to get heavier.",
    "My alarm rings, and I immediately think about the meeting I have this afternoon. I tell myself I've probably prepared enough, but I'm not completely convinced. While brushing my teeth, I wonder whether I should have reviewed my notes one more time. I remind myself that worrying won't help, yet a minute later I'm imagining a question I might not be able to answer. In the kitchen, I make coffee and oatmeal, but my attention keeps drifting back to the meeting. Maybe I'm overthinking it. Then again, if I miss something important, I'll wish I had spent more time preparing. I check my email. There's only a reminder, nothing unexpected, but it brings the meeting back to mind again. The warm shower helps me relax for a while, and I decide I'll look over my notes once I arrive at work instead of thinking about them now. As I leave the apartment, I notice the cool morning air and realize I'm feeling a little calmer, even though the meeting is still sitting quietly in the back of my mind."
  ),
  stringsAsFactors = FALSE
)

params <- run_params(model = "gpt-4o", temperature = 0)

# --- Demo 1: score just the first row (n = 1) --------------------------------
cat("\n--- Pass 1: score first row only (n = 1) ---\n")
score_many(df, "prompts/rumination_v1.txt", params = params, n = 1)

# --- Demo 2: score remaining rows (skip logic skips the first row) -----------
cat("\n--- Pass 2: score the rest (first row should be skipped) ---\n")
score_many(df, "prompts/rumination_v1.txt", params = params)

# --- Demo 3: re-run everything (all rows should be skipped) ------------------
cat("\n--- Pass 3: re-run (all rows should be skipped) ---\n")
score_many(df, "prompts/rumination_v1.txt", params = params)

# --- Demo 4: check status ----------------------------------------------------
cat("\n--- Status ---\n")
status(df, "prompts/rumination_v1.txt", params)

# --- Inspect the output CSV --------------------------------------------------
cat("\n--- Output CSV ---\n")
out <- read.csv("data/rumination_v1.csv", stringsAsFactors = FALSE)
print(out[, c("id", "repetition_looping", "progress_stagnation",
              "narrow_focus", "model", "scored_at")])
