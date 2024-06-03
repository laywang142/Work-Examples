RecursiveCharacterTextSplitter <- setRefClass(
  "RecursiveCharacterTextSplitter",
  fields = list(
    separators = "character",
    keep_separator = "logical",
    is_separator_regex = "logical",
    chunk_size = "numeric"
  ),
  methods = list(
    initialize = function(separators = NULL, keep_separator = TRUE, is_separator_regex = FALSE, chunk_size = 100) {
      separators <- ifelse(is.null(separators), c("\n\n", "\n", " ", ""), separators)
      .self$separators <- separators
      .self$keep_separator <- keep_separator
      .self$is_separator_regex <- is_separator_regex
      .self$chunk_size <- chunk_size
    },
    split_text = function(text) {
      final_chunks <- list()
      separator <- .self$separators[length(.self$separators)]
      new_separators <- list()
      for (i in seq_along(.self$separators)) {
        s_ <- .self$separators[i]
        separator_ <- ifelse(.self$is_separator_regex, s_, gsub("([][|*+.^$(){}\\])", "\\\\\\1", s_))
        if (s_ == "") {
          separator <- s_
          break
        }
        if (grepl(separator_, text)) {
          separator <- s_
          new_separators <- .self$separators[(i + 1):length(.self$separators)]
          break
        }
      }
      separator_ <- ifelse(.self$is_separator_regex, separator, gsub("([][|*+.^$(){}\\])", "\\\\\\1", separator))
      splits <- unlist(strsplit(text, separator_, fixed = TRUE))

      good_splits <- list()
      for (s in splits) {
        if (nchar(s) < .self$chunk_size) {
          good_splits <- c(good_splits, s)
        } else {
          if (length(good_splits) > 0) {
            merged_text <- paste(good_splits, collapse = separator)
            final_chunks <- c(final_chunks, merged_text)
            good_splits <- list()
          }
          if (length(new_separators) == 0) {
            final_chunks <- c(final_chunks, s)
          } else {
            other_info <- .self$split_text(s)
            final_chunks <- c(final_chunks, other_info)
          }
        }
      }
      if (length(good_splits) > 0) {
        merged_text <- paste(good_splits, collapse = separator)
        final_chunks <- c(final_chunks, merged_text)
      }
      return(final_chunks)
    }
  )
)
#
# # Sample usage
# splitter <- RecursiveCharacterTextSplitter()
# text <- "Sample text to split by RecursiveCharacterTextSplitter."
# chunks <- splitter$split_text(raw_text)
# print(chunks)
