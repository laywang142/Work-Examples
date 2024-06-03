try(library(httr), silent = TRUE)
try(library(jsonlite), silent = TRUE)
suppressMessages(try(library(dplyr), silent = TRUE))

readRenviron(".Renviron")

source("splitAt.R")

# Define functions
#' Read questions from a txt file
#'
#' @param question_path path to the txt file for question
#'
#' @return question_list a string vector
#' @export
#'
#' @examples
get_questions <- function(question_path) {
  df <- read.delim(question_path, header = FALSE)
  question_list <- as.vector(df[, 1])
  return(question_list)
}

#' Read source txt file and split to chunks
#' Current design: split by level 1 headers
#'
#' @param txt_path path to the txt file, should be the argument 1 in the commandline
#' @return text_chunks vector (length >= 1) of text chunks
document_ingestion <- function(txt_path) {
  # read txt file
  raw_text <- readLines(txt_path)
  raw_text <- paste(raw_text, collapse = "\n")

  # Remove all the blanks
  DF <- unlist(strsplit(raw_text, "\\n{2,}"))
  DF <- DF[grepl("[a-zA-Z]+", DF)]
  chunks <- grepl("^\\d+\\.\\s", DF)

  text_chunks <- splitAt(DF, chunks)
  return(text_chunks)
}

#' Get Embeddings for one text chunk
#'
#' @param text_chunk a single chunk after splitting; can be a single chunk or a question
#'
#' @return embedding a list containing the original text chunk and the embedding vector
get_one_embedding <- function(text_chunk) {
  # Build the URL - same for all three requests
  embedding_url <- file.path(
    Sys.getenv("OPENAI_ENDPOINT"),
    "openai/deployments",
    Sys.getenv("OPENAI_EMBEDDING_NAME"),
    paste0(
      "embeddings?api-version=",
      Sys.getenv("OPENAI_API_VERSION")
    )
  )

  reqbody <- list(
    model = Sys.getenv("OPENAI_EMBEDDING_NAME"),
    input = text_chunk
  )

  jsonbody <- jsonlite::toJSON(reqbody, auto_unbox = TRUE)

  # Post request
  embedding <- httr::POST(
    url = embedding_url,
    httr::add_headers(
      `api-key` = Sys.getenv("OPENAI_API_KEY"),
      `Content-Type` = "application/json"
    ),
    body = jsonbody
  )

  embedding <- httr::content(embedding)

  embedding <- embedding$data[[1]]$embedding |>
    unlist() |>
    list()

  embedding <- cbind(text_chunk, embedding)

  return(embedding)
}

#' Given a list of text chunks, generate a embedding database
#'
#' @param text_list list or character vector contiaining chunked text
#'
#' @return df a dataframe of the followings columns: index, text, embeddings
#' @export
#'
#' @examples
create_embedding_database <- function(text_list) {
  # use apply
  embeddings_list <- sapply(text_list, simplify = FALSE, get_one_embedding)

  embed_df <- do.call("rbind", embeddings_list) |> as.data.frame()
  rownames(embed_df) <- NULL

  return(embed_df)
}

#' Given a question, search for the most relevant chunk in the database
#' Current design is to return one chunk
#'
#' @param question the question
#' @param df the embedding database
#'
#' @return context the relevant text chunk
#' @export
#'
#' @examples
get_relevant_chunk <- function(question, df) {
  question_embedding <- get_one_embedding(question) |> as.data.frame()

  embeddings <- do.call("rbind", list(question_embedding, df))

  embed_value <- do.call("rbind", embeddings[, 2]) |> as.data.frame()

  relatedness <- dist(embed_value) |> as.matrix()

  res <- cbind(embeddings, relatedness[, 1]) |> as.data.frame()
  colnames(res)[3] <- "relatedness"
  res[, 3] <- as.numeric(res[, 3])
  res <- res[order(res[, 3]), ]

  # only want top sections?
  res <- head(res, 2)

  return(res)
}


#' Create list containing user message
#'
#' @param question the question
#' @param context the context
#'
#' @return user_message a combined user message
#' @export
#'
#' @examples
craft_user_message <- function(question, context) {
  content <- paste(
    context,
    "Use the provided CONTEXT information to answer the following question:",
    question
  )

  user_message <- list(
    role = "user",
    content = content
  )

  return(user_message)
}

#' Make API call
#' @param message a list of messages; this should include previous chat history
#' @return bot_answer a list in chat message format (role and content)
#' @export
#'
#' @examples
call_api <- function(message) {
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

  messages <- message

  query_body <- list(
    messages = messages,
    max_tokens = 2000,
    temperature = 0,
    frequency_penalty = 0,
    presence_penalty = 0,
    top_p = 0.95
  )

  json_query_body <- jsonlite::toJSON(query_body, auto_unbox = TRUE)

  chat_completion <- httr::POST(
    url = ChatGPT_url,
    httr::add_headers(
      `api-key` = Sys.getenv("OPENAI_API_KEY"),
      `Content-Type` = "application/json"
    ),
    body = json_query_body
  ) %>%
    httr::content()

  bot_answer <- list(
    role = "assistant",
    content = chat_completion$choices[[1]]$message$content
  )

  return(bot_answer)
}


#### SCRIPT ####
# Parse arguments
args <- commandArgs(trailingOnly = TRUE)
# stop the script if no command line argument
if (length(args) == 0) {
  print("Please include a text file")
  stop("Requires command line argument.")
}

# Ingest documents and questions
text_chunks <- document_ingestion(args[1])
DB <- create_embedding_database(text_chunks)
question_list <- get_questions(args[2])
# Initialize chat history with system message
MESSAGE <- list(
  list(
    role = "system",
    content = "You are a helpful assistant; Only answer question based on the CONTEXT provided;
    If you can't find the answer, say 'I cannot find that answer in the provided file'"
  )
)
for (i in seq_len(length(question_list))) {
  question <- question_list[i]
  context <- get_relevant_chunk(question, DB)[2, 1]
  user_message <- craft_user_message(question, context)
  # append questions to chat history
  MESSAGE <- append(MESSAGE, list(user_message))
  # get answer
  bot_answer <- call_api(MESSAGE)
  # print answer
  cat(paste0(
    "Answer to question ", i, ":", "\n",
    bot_answer$content[[1]], "\n"
  ))
  # remove the context to save tokens
  MESSAGE[[length(MESSAGE)]]$content[[1]] <- question
  MESSAGE <- append(MESSAGE, list(bot_answer))
}
