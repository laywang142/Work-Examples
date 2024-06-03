# Define helpful functions

#' Determine where to split the chunks
#'
#' @param string the raw vector of chunks
#' @param logical determine where the start of the split
#'
#' @return res a vector of strings that has the correct split
splitAt <- function(string, logical) {
  if (length(string) != length(logical)) {
    stop("string and logical must be the same length")
  } else {
    res <- c(string[1])
    for (i in seq_len(length(logical))[-1]) {
      if (i < length(logical)) {
        if (logical[i]) {
          temp_string <- string[i]
          if (logical[i + 1]) {
            res[i] <- temp_string
          } else {
            ind <- i
          }
        } else {
          if (!logical[i] && !logical[[i + 1]]) {
            temp_string <- paste(c(temp_string, string[i]), collapse = "\n")
          } else {
            temp_string <- paste(c(temp_string, string[i]), collapse = "\n")
            res[ind] <- temp_string
          }
        }
      } else {
        if (logical[i]) {
          res[i] <- string[i]
        } else {
          temp_string <- paste(c(temp_string, string[i]), collapse = "\n")
          res[ind] <- temp_string
        }
      }
    }
    res <- na.omit(res)
    return(res)
  }
}
