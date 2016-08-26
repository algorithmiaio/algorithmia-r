#' @include Client.r

getAlgorithmUrl <- function(input) {
  if (!(is.character(input) && length(input) == 1)) {
    stop("algorithm path needs to be a string")
  }

  # Strip leading stuff
  path <- gsub("^(?:algo://|/|)", "", input)
  matching <- grep("(\\w+/.+)", path)

  if (length(matching) == 0 || !as.logical(matching)) {
    stop("algorithm path needs to follow the pattern author/algorithm_name[/version]")
  }

  paste0("/v1/algo/", path)
}

getResponse <- function(response) {
  data <- httr::content(response)
  if ("error" %in% names(data)) {
    stop(paste0("Algorithm error: ", data$error))
  }

  response <- NULL
  if (data$metadata$content_type == "binary") {
    response <- list(metadata=data$metadata, result=base64enc::base64decode(data$result))
  } else {
    response <- list(metadata=data$metadata, result=data$result)
  }

  response
}

#' Algorithm object which enables you to call Algorithmia algorithms.
#' To create one, call: `client$algo("algoUrl")`
#'
#' @field client Reference to the AlgorithmiaClient object that has the credentials
#'        necessary to make API calls.
#' @field algoUrl The unique identifier for an algorithm. Follows the pattern:
#'        [Algorithm Author]/[Algorithm Name]/[Optional Version] like: 'demo/Hello/0.1.1'.
#' @field queryParameters Mutable list of parameters to use while making
#'        algorithm calls. These can be changed by calling setOptions.
AlgorithmiaAlgorithm <- methods::setRefClass("AlgorithmiaAlgorithm",
  field = list(client = "AlgorithmiaClient", algoUrl = "character", queryParameters = "list"),
  methods = list(
    pipe = function(input) {
      "Calls an algorithm with the input provided."
      if (queryParameters$output == "default") {
        getResponse(client$postJsonHelper(algoUrl, input, queryParameters))
      } else if (queryParameters$output == "void") {
        httr::content(client$postJsonHelper(algoUrl, input, queryParameters))
      } else if (queryParameters$output == "raw") {
        httr::content(client$postJsonHelper(algoUrl, input, queryParameters), as="raw")
      } else {
        stop(paste0("This is an unsupported output type: ", queryParameters$output))
      }
    },
    setOptions = function(timeout=300, stdout=FALSE, output="default", parameters=list()) {
      "Allows you to set the timeout duration (in seconds), whether you want the stdout
       that was produced while running the algorithm (this only works when the algorithm
       author call it), and whether this should run as async (output = 'void') or in
       raw mode (output = 'raw')."
      queryParameters <<- list(timeout=timeout, stdout=stdout)
      queryParameters <<- modifyList(queryParameters, parameters)
      queryParameters["output"] <<- output
    }
  )
)

getAlgorithm <- function(client, algoRef) {
  defaultParameters <- list(timeout=300, stdout=FALSE, output="default")
  AlgorithmiaAlgorithm$new(client=client, algoUrl=getAlgorithmUrl(algoRef), queryParameters=defaultParameters)
}
