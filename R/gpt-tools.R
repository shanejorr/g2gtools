#' Build a user-prompt for K-12 qualitative coding from a data-frame column
#'
#' Produces the **user-role** prompt that pairs with
#' `g2g_system_prompt_survey()` when calling the OpenAI Chat Completions
#' API.
#' Supply a data frame (or tibble) that has **exactly one column**: the column
#' name is treated as the survey question and each row as a response.
#'
#' The function
#' \enumerate{
#'   \item removes \code{NA} values;
#'   \item filters out non-substantive replies such as “n/a”, “none”, “no”, “.”,
#'         etc. (case-insensitive); and
#'   \item embeds the remaining answers—delimited by `---`—into a prompt that
#'         instructs the model to return its thematic analysis **only** as a
#'         CSV with the required header.
#' }
#'
#'
#' @param question_column A data frame (or tibble) with *one* column.
#'        The column name is taken as the survey question; each row is coerced
#'        to character and treated as a response.
#' @param survey_background Optional character string with context (e.g., who
#'        took the survey, purpose).  Default \code{NULL}.
#' @param num_themes Optional integer.  If provided, the model is told to
#'        identify *no more than* this many themes.  Default \code{NULL}.
#' @param custom_language Optional additional instructions appended to the
#'        prompt.  Default \code{NULL}.
#'
#' @return A length-1 character vector containing the full user prompt.
#' @export
#'
#' @examples
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   df <- tibble::tibble(
#'     "Describe one thing you liked about the class." = c(
#'       "I loved the projects.", "N/A", "The teacher cared.", "none", "Nope."
#'     )
#'   )
#'   cat(g2g_user_prompt_survey(df, num_themes = 4))
#' }
g2g_user_prompt_survey <- function(
    question_column,
    survey_background = NULL,
    num_themes        = NULL,
    custom_language   = NULL
) {

  ## ------------------------------------------------------------------------ ##
  ## Validate input
  if (!is.data.frame(question_column) || ncol(question_column) != 1) {
    stop("`question_column` must be a data frame (or tibble) with exactly one column.")
  }

  question_text <- names(question_column)[1]
  question_text <- stringr::str_trim(question_text)

  if (question_text == "") {
    stop("The column name (survey question) cannot be blank.")
  }

  ## ------------------------------------------------------------------------ ##
  ## Extract and clean responses
  responses_vec <- question_column[[1]]
  responses_vec <- as.character(responses_vec)

  # Drop NA values
  responses_vec <- responses_vec[!is.na(responses_vec)]

  # Trim whitespace
  responses_vec <- stringr::str_trim(responses_vec)

  # Define non-substantive patterns (regex, already lower-case)
  junk_patterns <- c(
    "", "n/?a", "none", "nothing", "nothing to add", "no", "nope",
    "not applicable", "n\\.a\\.", "nil", "\\.", "-", "--", "no comment",
    "no comments?", "nothing else", "nothing really", "blank", "\\?"
  )
  junk_regex <- paste0("^\\s*(", paste(junk_patterns, collapse = "|"), ")\\s*$")

  # Remove junk responses (case-insensitive)
  is_junk <- stringr::str_detect(
    stringr::str_to_lower(responses_vec),
    junk_regex
  )
  responses_vec <- responses_vec[!is_junk]

  if (length(responses_vec) == 0) {
    stop("No substantive responses remain after filtering NA and non-substantive entries.")
  }

  ## ------------------------------------------------------------------------ ##
  ## Assemble responses block (`---` delimiter)
  responses_block <- stringr::str_c("---\n", responses_vec, collapse = "\n")
  responses_block <- stringr::str_c(responses_block, "\n---")

  n_resp <- length(responses_vec)

  ## ------------------------------------------------------------------------ ##
  ## Build prompt
  prompt <- glue::glue(
    "Survey question:\n\"{question_text}\"\n\n",
    if (!is.null(survey_background) && stringr::str_trim(survey_background) != "") {
      glue::glue("Background: {survey_background}\n\n")
    } else "",
    "Here are {n_resp} individual responses, separated by triple dashes (---):\n\n",
    "{responses_block}\n\n",
    "Please identify the key themes across these responses",
    if (!is.null(num_themes)) glue::glue(" (no more than {num_themes})") else "",
    " and return **only** a CSV with the header:\n",
    "`question,theme_name,description,quote1,quote2,quote3,frequency,total_responses`.\n",
    "Do not write any text outside the CSV.\n"
  )

  if (!is.null(custom_language) && stringr::str_trim(custom_language) != "") {
    prompt <- paste(prompt, stringr::str_trim(custom_language), sep = "\n")
  }

  prompt
}

#' System prompt for inductive thematic coding of K-12 survey text (CSV output)
#'
#' Returns the full **system-prompt** string to supply to the OpenAI Chat
#' Completions API when you need grounded (inductive) qualitative coding of
#' open-ended survey answers from students, teachers, or families in **K-12 education**.
#' The model is instructed to extract themes and emit the results as tidy CSV.
#'
#' @details
#' The prompt tells the model to:
#' \itemize{
#'   \item apply grounded coding—derive themes inductively rather than using
#'         a predetermined list;
#'   \item label each theme with a jargon-free name of at most five words;
#'   \item provide a 1–2 sentence description, up to three representative
#'         quotes (each ≤ 25 words, with names masked as \code{"[NAME]"}), and
#'         a frequency count;
#'   \item sort rows by descending frequency; and
#'   \item output nothing but a CSV table using the fixed header
#'         \code{question,theme_name,description,quote1,quote2,quote3,frequency,total_responses}.
#' }
#'
#' The returned character scalar can be passed directly as the
#' \code{system}-role message in \code{openai::createChatCompletion()} (or any
#' wrapper you use).  Pair it with a user prompt that supplies the survey
#' question and responses and asks for analysis following the CSV rules.
#'
#' @return A length-1 character vector containing the complete prompt string.
#' @export
#'
#' @examples
#' prompt_txt <- g2g_system_prompt_survey()
#' cat(substr(prompt_txt, 1, 160), "...\n")   # preview the first 160 chars
g2g_system_prompt_survey <- function() {
  paste(
    "You are a qualitative-analysis specialist focused on K-12 education survey data.",
    "Your task is to read open-ended responses, derive clear, concise themes",
    "inductively, and return the findings in a tidy CSV format suitable for",
    "further quantitative work.",
    "",
    "Operating rules",
    "1. Use grounded, inductive coding; never force a response into a preset bucket unless explicitly told to.",
    "2. After identifying themes, assign each a short, jargon-free name (max 5 words).",
    "3. For every theme gather:",
    "   • description — 1–2 sentences explaining the idea",
    "   • up to 3 representative quotes (≤ 25 words each, names replaced with “[NAME]”)",
    "   • frequency — count of responses in which the theme appears",
    "4. Sort rows from most to least frequent.",
    "5. Output **only** a CSV with the exact header below and one row per theme.",
    "6. Include the survey question text and total_responses in every row so each record is self-contained.",
    "7. Quote snippets only; do not reproduce entire responses.",
    "8. Mask or remove personally identifiable information beyond the allowed quote fragments.",
    "9. Provide no commentary outside the CSV table.",
    "",
    "CSV header (use exactly this order):",
    "",
    "question,theme_name,description,quote1,quote2,quote3,frequency,total_responses",
    sep = "\n"
  )
}

#' Call GPT API
#'
#' This function sends a request to the GPT API, passing the user and system prompts,
#' and returns the model's response. The function uses the API key located in `Sys.getenv("OPENAI_API_KEY")`.
#'
#' @param user_prompt A character string containing the user prompt.
#' @param system_prompt A character string containing the system prompt.
#' @param model A character string representing the model to be used (default is "gpt-4o").
#'
#' @return API text response.
#'
#' @examples
#' \dontrun{
#' g2g_user_prompt_survey <- g2g_user_prompt_survey("What did you like most about the training?", "It was very engaging.")
#' g2g_system_prompt_survey <- g2g_system_prompt_survey()
#' g2g_call_gpt(user_prompt, system_prompt)
#' }
#'
#' @export
g2g_call_gpt <- function(user_prompt, system_prompt, model = "gpt-4.1") {

  api_url <- "https://api.openai.com/v1/chat/completions"
  api_key <- Sys.getenv("OPENAI_API_KEY")

  # Check if API key is available
  if (api_key == "") {
    stop("OPENAI_API_KEY environment variable is not set.")
  }

  # Create request |>
  req <- httr2::request(api_url) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt))
    )
  )

  # Send request and get response
  resp <- httr2::req_perform(req)

  # Check if the response status is not 200 (success)
  if (httr2::resp_status(resp) != 200) {
    stop(glue::glue("API request failed with status {httr2::resp_status(resp)}: {httr2::resp_body_string(resp)}"))
  }

  # Parse and return the response
  resp_body <- httr2::resp_body_json(resp)

  # Check if the expected structure is in the response
  if (is.null(resp_body$choices) || length(resp_body$choices) == 0) {
    stop("Unexpected response structure from API. No choices found.")
  }

  return(resp_body$choices[[1]]$message$content)
}
