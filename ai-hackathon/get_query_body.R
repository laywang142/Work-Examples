#' Functionality for when only a single question is provided
#' @description
#' Provides question to gpt API and returns response
#'
#' @param question a character vector of length 1
#'
#' @return a list containing the system prompt, the user question, and the bot
#' response
#'
single_q <- function(question) {
  system_prompt <- list(
    role = "system",
    content = "You are an AI assistant that helps people find information.
    Only answer questions in the biomedical domain.
    Otherwise, respond by saying that the given question is not
    relevant to the interested domain."
  )

  user_question <- list(
    role = "user",
    content = paste(question,
                    "Don’t justify your answers. Don’t give information not
                    mentioned in the CONTEXT INFORMATION.",
                    "Do not answer questions relating to biomedically related television shows.",
                    "Only answer questions that are
                    not significantly unrelated to the biomedical domain.
                    Otherwise, respond by saying that the given question is not relevant
                    to the interested domain.")
  )

  messages <- list(system_prompt, user_question)

  query_body <- list(
    messages = messages,
    max_tokens = 800,
    temperature = 0.7,
    frequency_penalty = 0,
    presence_penalty = 0,
    top_p = 0.95
  )

  json_query_body <- jsonlite::toJSON(query_body, auto_unbox = TRUE)

  # Build the URL
  ChatGPT_url <- file.path(
    Sys.getenv("OPENAI_ENDPOINT"),
    "openai/deployments",
    Sys.getenv("OPENAI_DEPLOYMENT_NAME"),
    paste0(
      "chat/completions?api-version=",
      Sys.getenv("OPENAI_API_VERSION")
    )
  )

  # Post request
  chat_completion <- httr::POST(
    url = ChatGPT_url,
    httr::add_headers(
      `api-key`=Sys.getenv("OPENAI_API_KEY"),
      `Content-Type` = "application/json"
    ),
    body = json_query_body
  ) %>%
    httr::content()

  bot_response <- list(role = "assistant",
                   content = chat_completion$choices[[1]]$message$content)

  res <- append(messages,
                list(bot_response)
                )

  return(res)
}


#' Functionality for when multiple questions are provided
#' @description
#' Provides question to gpt API and returns response
#'
#' @param question a character vector
#'
#' @return a list containing the system prompt, the user question, and the bot
#' response for every question asked
#'

multi_q <- function(question) {
  if(length(question) > 1) {

    messages <- NULL

    if(is.list(question)) {
      unlist(question)
    }

    # Build the URL
    ChatGPT_url <- file.path(
      Sys.getenv("OPENAI_ENDPOINT"),
      "openai/deployments",
      Sys.getenv("OPENAI_DEPLOYMENT_NAME"),
      paste0(
        "chat/completions?api-version=",
        Sys.getenv("OPENAI_API_VERSION")
      )
    )

    messages <- list(list(
      role = "system",
      content = "You are an AI assistant that helps people find information. Only answer questions that are
                    not significantly unrelated to the biomedical domain.
                    Otherwise, respond by saying that the given question is not relevant
                    to the interested domain."
    ))

    for( i in seq_len(length(question))) {

      user_question <- list(
        role = "user",
        content = paste(question[[i]],
                        "Don’t justify your answers. Don’t give information not mentioned
                        in the CONTEXT INFORMATION.",
                        "Do not answer questions relating to biomedically related television shows.",
                        "Only answer questions that are
                    not significantly unrelated to the biomedical domain.
                    Otherwise, respond by saying that the given question is not relevant
                    to the interested domain.")
        )

      messages <- append(messages, list(user_question))

      query_body <- list(
        messages = messages,
        max_tokens = 800,
        temperature = 0.7,
        frequency_penalty = 0,
        presence_penalty = 0,
        top_p = 0.95
      )

      json_query_body <- jsonlite::toJSON(query_body, auto_unbox = TRUE)

      chat_completion <- httr::POST(
        url = ChatGPT_url,
        httr::add_headers(
          `api-key`=Sys.getenv("OPENAI_API_KEY"),
          `Content-Type` = "application/json"
        ),
        body = json_query_body
      ) %>%
        httr::content()

      bot_answer <- list(
        role = "assistant",
        content = chat_completion$choices[[1]]$message$content
      )

      messages <- append(messages, list(bot_answer))

    }

    return(messages)

  }
}


#' Return bot responses to questions
#' @description
#' Provides question to gpt API and returns response only
#'
#' @param question a character vector
#'
#' @return a list containing the bot response for every question asked
#'

get_response <- function(question) {

  q_len <- length(question)

  if (q_len == 1) {

    chat_completion <- single_q(question)

  } else {

    chat_completion <- multi_q(question)

  }

  response_tf <- sapply(chat_completion, simplify = TRUE, function(a) a$role == "assistant")

  chat_completion <- chat_completion[response_tf]

  return(chat_completion)
}
