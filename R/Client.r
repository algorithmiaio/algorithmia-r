# Algorithmia Client
library("base64enc")
library("httr")
library("rjson")
library("methods")

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

AlgorithmiaClient <- setRefClass("AlgorithmiaClient",
  fields = list(apiKey = "character", apiAddress = "character"),
  methods = list(
    algo = function(algoRef) {
      getAlgorithm(.self, algoRef)
    },
    file = function(dataUrl) {
      getDataFile(.self, dataUrl)
    },
    dir = function(dataUrl) {
      getDataDirectory(.self, dataUrl)
    },
    getBasicHeaders = function() {
      headers <- c()

      if (!is.na(apiKey)) {
        headers["Authorization"] <- apiKey
      }

      headers
    },
    getHelper = function(url, queryParameters=c(), targetFile=NULL) {
      headers <- getBasicHeaders()

      if (is.null(targetFile)) {
        httr::GET(url=paste0(apiAddress, url),
                  query=queryParameters,
                  config=httr::add_headers(headers))
      } else {
        httr::GET(url=paste0(apiAddress, url),
                  query=queryParameters,
                  config=httr::add_headers(headers),
                  httr::write_disk(targetFile))
      }
    },
    postJsonHelper = function(algoUrl, input, queryParameters=c()) {
      inputJson <- NULL
      headers <- getBasicHeaders()

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

      httr::POST(url=paste0(apiAddress, algoUrl), query=queryParameters, config=httr::add_headers(headers), body=inputJson)
    },
    headHelper = function(url) {
      headers <- getBasicHeaders()

      httr::HEAD(url=paste0(apiAddress, url), config=httr::add_headers(headers))
    },
    putHelper = function(url, data) {
      headers <- getBasicHeaders()

      httr::PUT(url=paste0(apiAddress, url), config=httr::add_headers(headers), body=data)
    },
    deleteHelper = function(url) {
      headers <- getBasicHeaders()

      httr::DELETE(url=paste0(apiAddress, url), config=httr::add_headers(headers))
    },
    patchJsonHelper = function(url, input) {
      headers <- getBasicHeaders()
      headers["Content-Type"] <- 'application/json'

      httr::PATCH(url=paste0(apiAddress, url), config=httr::add_headers(headers), body=rjson::toJSON(input))
    }
  )
)

getAlgorithmiaClient <- function(apiKey=NA_character_, apiAddress=NA_character_) {
  AlgorithmiaClient$new(apiKey=apiKey, apiAddress=getAlgorithmiaApiAddress(apiAddress))
}
