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
#' raw      <- call_openai(build_prompt(template, list(text = "Some text.")), run_params())
#' }
call_openai <- function(prompt,
                        params   = run_params(),
                        api_key  = Sys.getenv("OPENAI_API_KEY"),
                        base_url = "https://api.openai.com/v1") {
  if (!nzchar(api_key))
    stop(paste0(
      "No OpenAI API key found (OPENAI_API_KEY is empty).\n",
      "  1. Obtain your key from the lab Security Manager — do not use a personal account.\n",
      "  2. Save it to a gitignored .env file in your project root:\n",
      "       OPENAI_API_KEY=sk-...\n",
      "  3. Load it at the top of your script:\n",
      "       readRenviron(\".env\")"
    ))

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
    httr2::req_error(body = function(resp) {
      status <- httr2::resp_status(resp)
      body   <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
      msg    <- body$error$message %||% "(no details)"
      hint   <- switch(as.character(status),
        "401" = "\n  Hint: your API key may be invalid or expired — check with the Security Manager.",
        "403" = "\n  Hint: your account may not have access to this model or endpoint.",
        "429" = "\n  Hint: rate limit exceeded. The package retries automatically; if this persists, wait a few minutes.",
        "")
      sprintf("OpenAI API error %d: %s%s", status, msg, hint)
    }) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)$choices[[1]]$message$content
}
