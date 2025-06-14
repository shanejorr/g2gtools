% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gpt-tools.R
\name{g2g_system_prompt_survey}
\alias{g2g_system_prompt_survey}
\title{System prompt for inductive thematic coding of K-12 survey text (CSV output)}
\usage{
g2g_system_prompt_survey()
}
\value{
A length-1 character vector containing the complete prompt string.
}
\description{
Returns the full \strong{system-prompt} string to supply to the OpenAI Chat
Completions API when you need grounded (inductive) qualitative coding of
open-ended survey answers from students, teachers, or families in \strong{K-12 education}.
The model is instructed to extract themes and emit the results as tidy CSV.
}
\details{
The prompt tells the model to:
\itemize{
\item apply grounded coding—derive themes inductively rather than using
a predetermined list;
\item label each theme with a jargon-free name of at most five words;
\item provide a 1–2 sentence description, up to three representative
quotes (each ≤ 25 words, with names masked as \code{"[NAME]"}), and
a frequency count;
\item sort rows by descending frequency; and
\item output nothing but a CSV table using the fixed header
\code{question,theme_name,description,quote1,quote2,quote3,frequency,total_responses}.
}

The returned character scalar can be passed directly as the
\code{system}-role message in \code{openai::createChatCompletion()} (or any
wrapper you use).  Pair it with a user prompt that supplies the survey
question and responses and asks for analysis following the CSV rules.
}
\examples{
prompt_txt <- g2g_system_prompt_survey()
cat(substr(prompt_txt, 1, 160), "...\n")   # preview the first 160 chars

}
