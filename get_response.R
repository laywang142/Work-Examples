
get_response <- function(messages) {

  response_tf <- sapply(messages, simplify = TRUE, function(a) a$role == "assistant")

  response_only <- messages[response_tf]

  return(response_only)
}

