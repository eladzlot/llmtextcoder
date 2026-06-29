#' Call the OpenAI chat-completions endpoint
#'
#' Sends a single user message and returns the raw JSON text of the model's
#' reply. Forces structured JSON output via `response_format`, so the reply
#' is always a valid JSON string ready for [parse_response()]. The rubric
#' template must instruct the model to respond in JSON (OpenAI's requirement
#' for this mode).
#'
#' Transient failures (429 rate-limit, 500/502/503 server errors) are retried
#' up to three times with exponential backoff before an error is raised.
#'
#' @param prompt Character scalar. The fully assembled prompt — typically the
#'   output of [build_prompt()].
#' @param params A `run_params` object created by [run_params()].
#' @param api_key Character. OpenAI API key. Default reads the
#'   `OPENAI_API_KEY` environment variable. Store your key in a gitignored
#'   `.env` file and load it with `readRenviron(".env")`.
#' @param base_url Character. Base URL for the API. Override for non-default
#'   endpoints (e.g. Azure OpenAI deployments).
#'
#' @return Character scalar: the raw JSON string returned by the model.
#' @export
#'
#' @examples
#' \dontrun{
#' readRenviron(".env")
#' template <- read_template("prompts/rubric_v1.txt")
#' raw      <- call_openai(build_prompt(template, "Some text."), run_params())
#' }
call_openai <- function(prompt,
                        params   = run_params(),
                        api_key  = Sys.getenv("OPENAI_API_KEY"),
                        base_url = "https://api.openai.com/v1") {
  if (!nzchar(api_key)) {
    stop("No API key. Set OPENAI_API_KEY in a gitignored .env file.")
  }

  resp <- httr2::request(base_url) |>
    httr2::req_url_path_append("chat/completions") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(list(
      model           = params$model,
      temperature     = params$temperature,
      response_format = list(type = "json_object"),
      messages        = list(list(role = "user", content = prompt))
    )) |>
    httr2::req_retry(
      max_tries    = 4,
      is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 502, 503)
    ) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)$choices[[1]]$message$content
}
