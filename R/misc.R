#' Call GPT Model via OpenAI API
#'
#' This function sends a prompt to the specified OpenAI GPT model and retrieves the response.
#'
#' @param system_prompt A string containing the system's prompt.
#' @param user_prompt A string containing the user's prompt.
#' @param model A string specifying the model to use (default is "gpt-4o").
#'
#' @return A string containing the response from the GPT model.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   response <- call_gpt("You are a funny assistant.", "Tell me a joke.")
#'   print(response)
#' }
call_gpt <- function(system_prompt, user_prompt, model = "gpt-4o") {

  # Predefined list of valid models
  valid_models <- c("gpt-4o-mini", "gpt-4o")

  # Check if the API key is available
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    stop("Error: OPENAI_API_KEY environment variable not set. Please set it before calling the function.")
  }

  # Check if the model is valid
  if (!(model %in% valid_models)) {
    stop(paste("Error: Invalid model. Please choose from the following models:", paste(valid_models, collapse = ", ")))
  }

  api_url <- "https://api.openai.com/v1/chat/completions"

  # Create request
  req <- httr2::request(api_url) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      )
    ))

  # Send request and get response
  resp <- httr2::req_perform(req)

  # Parse and return the response
  resp_body <- httr2::resp_body_json(resp)

  return(resp_body$choices[[1]]$message$content)
}





#' System prompt for the teacher training program key themes
#'
#' @return A string containing the system prompt.
#'
#' @export
system_prompt_teacher <- function() {

  initial_prompt <- "You are a social science researcher. You specialize in conducting qualitative coding of open-ended survey responses using thematic coding. You create concise and thorough summaries of open-ended survey responses using the latest research-based methods for qualitative coding. You follow the principals in The Coding Manual for Qualitative Researchers by Johnny Saldana."

  output_format_csv <- "Please provide your responses in a CSV format. There should be one column called `themes`. Each row should contain a key theme and the key theme should not be more than a sentence, but it should be a complete sentence. You should only output the 'csv' formatted response. "

  example_themes <- c(
    "themes",
    "Teachers enjoyed the teacher training program.",
    "Teachers found value in the student-centered learning techniques that they learned.",
    "The training provided practical strategies for classroom management."
  ) |>
    stringr::str_flatten(collapse = "\n")

  stringr::str_c(initial_prompt, output_format_csv, "Here are some examples of key themes and how the output should appear in csv format: '", example_themes, "'. ")

}

#' User prompt for the teacher training program key themes
#'
#' @param survey_question A string containing the survey question.
#' @param teacher_responses A string containing the teacher responses.
#' @param num_themes An integer specifying the number of key themes to identify.
#'
#' @return A string containing the user prompt.
#'
#' @export
user_prompt_teacher <- function(survey_question, teacher_responses, num_themes) {

  user_prompt_intro <- "You have been asked to code the following qualitative responses from an open-ended survey. Please provide a concise summary of responses for the question by picking out the key general themes. "

  survey_background <- "The qualitative responses come from a survey of teachers after they completed a teacher training program. The question they are responding to is: "

  id_num_themes <- glue::glue("Please identify no more than {num_themes} key themes. You do not have to identify at least {num_themes} key themes if there are fewer than {num_themes} key themes present in the responses. ")

  question_and_background <- stringr::str_c(user_prompt_intro, survey_background, "'", survey_question, "'. ", id_num_themes)

  response_text <- stringr::str_c("The responses are: '", teacher_responses, "'")

  stringr::str_c(question_and_background, response_text)

}
