#' Create a set of model run parameters
#'
#' Constructs a validated `run_params` object that bundles the model name and
#' temperature. Pass this object to [call_openai()], [score_one()], and
#' [score_many()] so that every API call carries explicit, reproducible
#' settings — and so those settings are written into every row of the output
#' CSV.
#'
#' @param model Character scalar. OpenAI model name. Default `"gpt-4o"`.
#' @param temperature Numeric scalar in `[0, 2]`. Default `0` for maximally
#'   deterministic output. Higher values increase variability; rarely needed
#'   for rubric-based coding tasks.
#'
#' @return An object of class `run_params` (a named list with fields `model`
#'   and `temperature`).
#' @export
#'
#' @examples
#' run_params()
#' run_params(model = "gpt-4o-mini", temperature = 0.2)
run_params <- function(model = "gpt-4o", temperature = 0) {
  stopifnot(
    is.character(model), nzchar(model),
    is.numeric(temperature), length(temperature) == 1L,
    temperature >= 0, temperature <= 2
  )
  structure(list(model = model, temperature = temperature), class = "run_params")
}

#' @export
print.run_params <- function(x, ...) {
  cat(sprintf("run_params: model=%s  temperature=%g\n", x$model, x$temperature))
  invisible(x)
}
