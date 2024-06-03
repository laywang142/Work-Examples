# If the packages below aren't installed, please install before running the script
#
# Additional functions used are in the file get_query_body.R

source("get_query_body.R")

try(library(httr), silent = TRUE)
try(library(jsonlite), silent = TRUE)
suppressMessages(try(library(dplyr), silent = TRUE))

readRenviron(".Renviron")

args <- commandArgs(trailingOnly = TRUE)

#stop the script if no command line argument
if(length(args)==0){
  print("Please include a text file")
  stop("Requires command line argument.")
}

## Read in the questions

df_import <- read.delim(args[1], header = F)

q_list <- df_import[, 1]

## Get responses

chat_completion <- get_response(q_list)

chat_responses <- sapply(seq_len(length(chat_completion)), simplify = F, function(a) {
    c(paste("Answer to question", paste0(a, ":"), q_list[[a]]),
      chat_completion[[a]]$content[[1]])
  }) %>% unlist()

## Final output

cat(chat_responses, sep = "\n")




