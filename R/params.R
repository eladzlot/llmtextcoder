#' Create a set of model run parameters
#'
#' Bundles the model name and any additional API parameters into a `run_params`
#' object. Pass this to [call_openai()], [score_one()], and [score_many()].
#' Every API call will forward the extras directly to OpenAI, and they are
#' recorded in the output CSV as a JSON string for reproducibility.
#'
#' @param model Character scalar. OpenAI model name. Default `"gpt-4o"`.
#' @param ... Any additional parameters accepted by the model, e.g.
#'   `temperature = 0`, `seed = 42`, `top_p = 1`. Omit parameters the model
#'   does not support (e.g. reasoning models reject `temperature`).
#'
#' @return An object of class `run_params` (a named list with fields `model`
#'   and `extras`).
#' @export
#'
#' @examples
#' run_params()
#' run_params(model = "gpt-4o", temperature = 0.2)
#' run_params(model = "o4-mini")
#' run_params(model = "gpt-4o", temperature = 0, seed = 42)
run_params <- function(model = "gpt-4o", ...) {
  if (!is.character(model) || length(model) != 1L || !nzchar(model))
    stop(sprintf(
      "'model' must be a non-empty character string (e.g. \"gpt-4o\"), got %s.",
      if (is.character(model)) paste0("\"", model, "\"")
      else sprintf("%s of length %d", class(model)[1L], length(model))
    ))
  structure(list(model = model, extras = list(...)), class = "run_params")
}

#' @export
print.run_params <- function(x, ...) {
  extras_str <- if (length(x$extras) == 0L) ""
                else paste0("  ",
                            paste(names(x$extras), x$extras, sep = "=", collapse = "  "))
  cat(sprintf("run_params: model=%s%s\n", x$model, extras_str))
  invisible(x)
}
