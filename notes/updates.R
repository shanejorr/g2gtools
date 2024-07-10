# chatgpt

library(httr2)
library(tidyverse)
library(jsonlite)

system_prompt <- function() {

  "You are a social science researcher. You specialize in conducting qualitative coding of open-ended survey responses using thematic coding. You create concise and thorough summaries of open-ended survey responses."

}

cat('dsafasd\n', capture.output(write.csv(mtcars, row.names = FALSE)), "\nafsasdf")

user_prompt <- function(qualitative_responses) {

  introduction <- "You have been asked to code the following qualitative responses from an open-ended survey. Please provide a concise summary of responses for each question by picking out the key general themes. A key general theme is a theme that appears, generally, at least three times in a single question. "

}

qualitative_code_responses <- function(qualitative_responses, model = "gpt-4o") {
  api_url <- "https://api.openai.com/v1/chat/completions"
  api_key <- Sys.getenv("OPENAI_API_KEY")

  # Create request |>
  req <- request(api_url) |>
    req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    ) |>
    req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt()),
        list(role = "user", content = user_prompt(qualitative_responses)))
      )
    )

  # Send request and get response
  resp <- req_perform(req)

  # Parse and return the response
  resp_body <- resp_body_json(resp)
  return(resp_body$choices[[1]]$message$content)
}

# Example of calling the function and printing the result
result <- get_gpt4o_response("df")
print(result)

