#' Load the bundled James testimonials dataset
#'
#' Reads `inst/extdata/varieties_testimonials.csv` into a data frame.
#' The dataset contains 47 first-person psychological testimonials drawn from
#' William James's *The Varieties of Religious Experience* (1902).  Each row is
#' a single testimonial suitable for use as a coding target with
#' [score_many()].
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{id}{Character. Unique identifier (`"james_01"` … `"james_47"`).}
#'     \item{text}{Character. The full first-person testimonial text.}
#'     \item{theme}{Character. Broad psychological theme assigned during
#'       curation (e.g. `"depression"`, `"mystical"`, `"conversion"`).}
#'     \item{word_count}{Integer. Word count of the testimonial.}
#'     \item{source}{Character. Full bibliographic reference.}
#'   }
#'
#' @section Dataset overview:
#' The 47 testimonials span 213–675 words (median ≈ 340 words) and cover
#' themes including depressive episodes, anxiety, depersonalization,
#' scrupulosity, inner conflict, mystical experience, illness and healing,
#' religious conversion, and ascetic practice.  The texts are public domain
#' and were selected to provide psychologically varied material for testing
#' coding rubrics.
#'
#' @examples
#' df <- varieties_testimonials()
#' head(df[, c("id", "theme", "word_count")])
#'
#' # Score a subset with your own rubric:
#' # params <- run_params()
#' # score_many(df[df$theme == "depression", ],
#' #            template_path = "prompts/my_rubric.txt",
#' #            params = params, output_dir = "data")
#'
#' @export
varieties_testimonials <- function() {
  path <- system.file("extdata", "varieties_testimonials.csv",
                      package = "llmtextcoder")
  if (!nzchar(path)) {
    # Dev mode: walk up from cwd to find the package root (contains DESCRIPTION)
    dir <- normalizePath(getwd())
    repeat {
      candidate <- file.path(dir, "inst", "extdata", "varieties_testimonials.csv")
      if (file.exists(candidate)) { path <- candidate; break }
      parent <- dirname(dir)
      if (parent == dir)
        stop("varieties_testimonials.csv not found. ",
             "Is llmtextcoder installed or are you running from the package root?")
      dir <- parent
    }
  }
  read.csv(path, stringsAsFactors = FALSE)
}
