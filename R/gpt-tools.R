#' Create a user prompt for GPT API
#'
#' This function constructs a user prompt to be sent to the GPT API, which includes
#' the survey question and the survey responses. The goal of the prompt is to ask
#' the GPT model to identify and summarize the key themes in the qualitative responses
#' provided by survey participants.
#'
#' @param survey_question A character string representing the survey question.
#' @param survey_background Background information on the survey (who took it, purpose, etc.)
#' @param survey_responses A character vector representing the survey responses. Each response is a different element in the vector.
#' @param num_themes An integer representing the number of key themes to identify (default is 3).
#' @param custom_language Additional instructions for the prompt.
#'
#' @return A character string representing the constructed prompt for the GPT model.
#' This prompt will contain the survey question, context, and responses that need to be summarized.
#'
#'
#' @export
g2g_user_prompt_survey <- function(survey_question, survey_background, survey_responses, num_themes = 3, custom_language = " ") {

  # Error handling for empty inputs
  if (missing(survey_question) || survey_question == "") {
    stop("Survey question cannot be empty.")
  }

  if (missing(survey_responses)) {
    stop("Survey responses cannot be empty.")
  }

  survey_responses_flat <- stringr::str_flatten(survey_responses, collapse = "\n", na.rm = TRUE)

  if (survey_responses_flat == "") {
    stop("Survey responses cannot be empty.")
  }

  user_prompt_intro <- "You have been asked to qualitatively code and then summarize the following qualitative responses from an open-ended survey. Please provide a concise summary of responses for the question by picking out the key general themes. "

  num_key_themes <- function(num_themes) {

    glue::glue("Please identify no more than {num_themes} key themes. You do not have to identify at least {num_themes} key themes if there are fewer than {num_themes} key themes present in the responses. ")

  }

  question_and_background <- stringr::str_c(user_prompt_intro, survey_background, "'", survey_question, "'. ", num_key_themes(num_themes))

  response_text <- stringr::str_c("The survey responses are as follows, each is seperated by a line break ('\n'):\n", survey_responses_flat)

  return(stringr::str_flatten(c(custom_language, question_and_background, response_text, collapse = "\n")))

}

#' Create a system prompt for GPT API
#'
#' This function creates a system prompt that provides the GPT model with background information
#' about the task at hand. It sets the role of the model as a social science researcher specializing
#' in qualitative coding. It also specifies the output format, which is a CSV containing key themes
#' extracted from the survey responses.
#'
#' @return A character string representing the system prompt to guide the GPT model in its role and task.
#'
#' @export
g2g_system_prompt_survey <- function() {

  initial_prompt <- glue::glue("
      You are a social science researcher. You specialize in conducting qualitative coding of open-ended survey responses using thematic coding.
      You create concise and thorough summaries of open-ended survey responses using the latest research-based methods for qualitative coding.
      You follow the principals in 'The Coding Manual for Qualitative Researchers' by Johnny Saldana and other best practices.
      You report on the most prevelant key themes, whether they are positive or negative.
      You convert your key themes into sentences and explanations that lay-people can understand by first qualitatively coding the survey responses.
      Then you go back at convert the key themes into sentences understandable to a lay person with little background understanding.
      Each key theme should be summarized into between one and three sentences.
  ")

  output_format_csv <- "\nPlease provide your responses in a CSV format. There should be one column with a column name called `themes`. Each row should contain an explanation of the key theme in the style just mentioned. You should only output the 'csv' formatted response. "

  stringr::str_c(initial_prompt, output_format_csv)

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
g2g_call_gpt <- function(user_prompt, system_prompt, model = "gpt-4o") {

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
