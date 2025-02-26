# chatgpt

library(httr2)
library(tidyverse)
library(jsonlite)

survey_responses <- c(
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
  stringr::str_flatten(collapse = "', '")

survey_question <- "What are the key takeaways from the teacher training program?"

user_prompt_text <- user_prompt_survey(survey_question, survey_responses)


key_themes <- call_gpt(user_prompt_text, system_prompt_survey())

a <- stringr::str_remove(key_themes, "csv\n") |>
  stringr::str_remove_all("`")
a
readr::read_csv(a)

