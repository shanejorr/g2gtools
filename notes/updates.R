# chatgpt

library(httr2)
library(tidyverse)
library(jsonlite)

system_prompt <- function() {

  initial_prompt <- "You are a social science researcher. You specialize in conducting qualitative coding of open-ended survey responses using thematic coding. You create concise and thorough summaries of open-ended survey responses using the latest research-based methods for qualitative coding. You follow the principals in The Coding Manual for Qualitative Researchers by Johnny Saldana."

  output_format_csv <- "Please provide your responses in a CSV format. There should be one column called `themes`. Each row should contain a key theme and the key theme should not be more than a sentence, but it should be a complete sentence. You should only output the 'csv' formatted response. "

  example_themes <- c(
    "themes",
    "Teachers enjoyed the teacher training program.",
    "Teachers found value in the student-centered learning techniques that they learned.",
    "The training provided practical strategies for classroom management."
  ) |>
    str_flatten(collapse = "\n")

  str_c(initial_prompt, output_format_csv, "Here are some examples of key themes and how the output should appear in csv format: '", example_themes, "'. ")

}

responses <- c(
  "The training provided valuable insights into differentiated instruction.",
  "Improved classroom management techniques were a key highlight.",
  "Emphasis on student-centered learning was particularly beneficial.",
  "I gained a deeper understanding of formative and summative assessments.",
  "The program enhanced my skills in using technology in the classroom.",
  "Collaborative learning strategies were well-explained and demonstrated.",
  "The importance of reflective teaching practices was a major takeaway.",
  "I learned new ways to engage students in active learning.",
  "The training reinforced the significance of culturally responsive teaching.",
  "I now have a better grasp of curriculum design and implementation.",
  "The sessions on student motivation were extremely helpful.",
  "I appreciated the focus on developing critical thinking skills in students.",
  "The program highlighted the role of continuous professional development.",
  "I feel more confident in creating inclusive learning environments.",
  "The training stressed the importance of data-driven instruction.",
  "Effective communication with parents was a key takeaway.",
  "The program provided practical strategies for teaching diverse learners.",
  "I learned about the latest educational research and best practices.",
  "The importance of setting clear learning objectives was emphasized.",
  "I gained new techniques for fostering a growth mindset in students.",
  "The training offered valuable resources for lesson planning.",
  "I now understand better how to assess student learning effectively.",
  "The program highlighted the importance of feedback in the learning process.",
  "I feel more equipped to handle classroom challenges.",
  "The focus on social-emotional learning was particularly useful.",
  "The training emphasized the importance of fostering student autonomy.",
  "I learned about innovative teaching methodologies.",
  "The sessions on time management and organization were very practical.",
  "I now have a better understanding of the teacher's role as a facilitator.",
  "The importance of building strong teacher-student relationships was underscored.",
  "The program provided strategies for enhancing student collaboration.",
  "I gained insights into effective classroom assessment techniques.",
  "The training highlighted the role of technology in modern education.",
  "I feel more prepared to differentiate instruction for varied learning needs.",
  "The focus on inclusive education was very enlightening.",
  "I learned new ways to incorporate project-based learning.",
  "The program stressed the importance of maintaining high expectations for all students.",
  "I gained practical tips for managing classroom behavior.",
  "The sessions on literacy development were particularly informative.",
  "I now understand better the principles of effective lesson design.",
  "The importance of creating a positive classroom culture was emphasized.",
  "The training provided strategies for engaging reluctant learners.",
  "I learned about the impact of trauma on student learning.",
  "The program highlighted the need for ongoing collaboration among educators.",
  "I feel more confident in my ability to support student learning.",
  "The focus on assessment for learning was particularly beneficial.",
  "I gained new perspectives on the role of formative assessment.",
  "The training stressed the importance of building student resilience.",
  "I now have a better understanding of effective questioning techniques.",
  "The importance of fostering a love for learning was a major takeaway."
) |>
  str_flatten(collapse = "', '")

user_prompt_intro <- "You have been asked to code the following qualitative responses from an open-ended survey. Please provide a concise summary of responses for the question by picking out the key general themes. "

survey_background <- "The qualitative responses come from a survey of teachers after they completed a teacher training program. The question they are responding to is: "

survey_question <- "What are the key takeaways from the teacher training program?"

num_key_themes <- function(num_themes) {

  glue::glue("Please identify no more than {num_themes} key themes. You do not have to identify at least {num_themes} key themes if there are fewer than {num_themes} key themes present in the responses. ")

}

question_and_background <- str_c(user_prompt_intro, survey_background, "'", survey_question, "'. ", num_key_themes(3))

response_text <- str_c("The responses are: '", responses, "'")

user_prompt <- str_c(question_and_background, response_text)

call_gpt <- function(user_prompt, system_prompt, model = "gpt-4o") {

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
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt))
      )
    )

  # Send request and get response
  resp <- req_perform(req)

  # Parse and return the response
  resp_body <- resp_body_json(resp)

  return(resp_body$choices[[1]]$message$content)
}
a <- resp_body$choices[[1]]$message$content

str_remove_all(a, "^```\n|```$") |> cat()

read_csv(I(str_remove(a, "^```\n|```$")))

# Example of calling the function and printing the result
result <- get_gpt4o_response("df")
print(result)

