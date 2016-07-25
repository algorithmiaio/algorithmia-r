# Algorithmia Client
library("base64enc")
library("httr")
library("rjson")

source("Algorithm.r")

DEFAULT_ALGORITHMIA_API_ADDRESS <- "https://api.algorithmia.com"

getAlgorithmiaApiAddress <- function(apiAddress=NA_character_) {
  if (!is.na(apiAddress)) {
    apiAddress
  } else if (!is.na(Sys.getenv("ALGORITHMIA_API", unset=NA))) {
    Sys.getenv("ALGORITHMIA_API", unset=NA)
  } else {
    DEFAULT_ALGORITHMIA_API_ADDRESS
  }
}

getAlgorithmiaClient <- function(apiKey=NA_character_, apiAddress=NA_character_) {
  AlgorithmiaClient <- setRefClass("AlgorithmiaClient",
    fields = list(apiKey = "character", apiAddress = "character"),
    methods = list(
      algo = function(algoRef) {
        getAlgorithm(.self, algoRef)
      },
      postJsonHelper = function(algoUrl, input, queryParameters) {
        inputJson <- NULL
        headers <- c()

        if (!is.na(apiKey)) {
          headers["Authorization"] <- apiKey
        }

        if (is.null(input) || is.na(input)) {
          inputJson <- rjson::toJSON(NULL)
          headers["Content-Type"] <- 'application/json'
        } else if (is.raw(input)) {
          inputJson <- input
          headers["Content-Type"] <- 'application/octet-stream'
        } else {
          inputJson <- rjson::toJSON(input)
          headers["Content-Type"] <- 'application/json'
        }

        httr::POST(url=paste0(apiAddress, algoUrl), query=queryParameters, config=add_headers(headers), body=inputJson)
      }
    )
  )
  AlgorithmiaClient$new(apiKey=apiKey, apiAddress=getAlgorithmiaApiAddress(apiAddress))
}
