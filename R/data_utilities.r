getDataPath <- function(input, isFile) {
  if (!(is.character(input) && length(input) == 1)) {
    stop("data path needs to be a string")
  }

  # Strip leading stuff
  path <- gsub("^(data://|/)", "", input)

  if (nchar(path) == 0) {
    stop("data path cannot be empty")
  }

  if (isFile && endsWith(path, "/")) {
    stop(paste0("Invalid file path ending: ", path))
  }

  path
}

getDataUrl <- function(input, isFile) {
  path <- getDataPath(input, isFile)

  paste0("/v1/data/", path)
}

checkFor200StatusCode <- function(response) {
  httr::status_code(response) == 200
}

checkResponse <- function(object, response) {
  data <- httr::content(response)
  if ("error" %in% names(data)) {
    stop(paste0("Data API Error: ", data$error))
  }

  if (checkFor200StatusCode(response)) {
    object
  } else {
    stop(paste0("Server error code: ", httr::status_code(response)))
  }
}
