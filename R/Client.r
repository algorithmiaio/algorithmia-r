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

#' Client object which makes it easy to interact with the Algorithmia REST API.
#' To create one, call `getAlgorithmiaClient("YOUR_ALGORITHMIA_API_KEY")`
#'
#' @field apiKey The API key used when making REST calls to Algorithmia. This
#'        should NOT be set inside algorithms.
#' @field apiAddress The Algorithmia API address. In most cases you don't need to
#'        set this explicitly since the default will talk to the correct
#'        Algorithmia API server.
AlgorithmiaClient <- methods::setRefClass("AlgorithmiaClient",
  fields = list(apiKey = "character", apiAddress = "character"),
  methods = list(
    algo = function(algoRef) {
      "Takes an algorithm reference  and returns an AlgorithmiaAlgorithm object.
       An algorithm reference is a string of the form
       [Algorithm Author]/[Algorithm Name]/[Optional Version] like: 'demo/Hello/0.1.1'.
       AlgorithmiaAlgorithm objects are used to call algorithms with data."
      getAlgorithm(.self, algoRef)
    },
    file = function(dataUrl) {
      "Takes a path to a file and returns a AlgorithmiaDataFile object.
       Data paths are described in detail at: http://docs.algorithmia.com/?java#data-api-specification.
       AlgorithmiaDataFile objects are used to read and write files."
      getDataFile(.self, dataUrl)
    },
    dir = function(dataUrl) {
      "Takes a path to a directory and returns a AlgorithmiaDataDirectory object.
       Data paths are described in detail at: http://docs.algorithmia.com/?java#data-api-specification.
       AlgorithmiaDataDirectory objects are used to interact with directories."
      getDataDirectory(.self, dataUrl)
    },
    reportInsights = function(insights) {
      "Takes a list of Algorithmia Insights and reports them for this algorithm execution."
      insights_names <- names(insights)
      payload <- lapply(seq_along(insights), function(nameindex) {
        list(insight_key = insights_names[[nameindex]], insight_value = insights[[insights_names[[nameindex]]]])
      })
      response <- client$postJsonHelper("/v1/insights", payload)
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
        httr::GET(url=URLencode(paste0(apiAddress, url)),
                  query=queryParameters,
                  config=httr::add_headers(headers))
      } else {
        httr::GET(url=URLencode(paste0(apiAddress, url)),
                  query=queryParameters,
                  config=httr::add_headers(headers),
                  httr::write_disk(targetFile))
      }
    },
    postJsonHelper = function(algoUrl, input, queryParameters=c()) {
      inputJson <- NULL
      headers <- getBasicHeaders()

      if (is.list(input) && length(input) == 0) {
        inputJson <- "[]"
        headers["Content-Type"] <- 'application/json'
      } else if (is.null(input) || is.na(input)) {
        inputJson <- rjson::toJSON(NULL)
        headers["Content-Type"] <- 'application/json'
      } else if (is.raw(input)) {
        inputJson <- input
        headers["Content-Type"] <- 'application/octet-stream'
      } else {
        inputJson <- rjson::toJSON(input)
        headers["Content-Type"] <- 'application/json'
      }

      httr::POST(url=URLencode(paste0(apiAddress, algoUrl)), query=queryParameters, config=httr::add_headers(headers), body=inputJson)
    },
    headHelper = function(url) {
      headers <- getBasicHeaders()

      httr::HEAD(url=URLencode(paste0(apiAddress, url)), config=httr::add_headers(headers))
    },
    putHelper = function(url, data) {
      headers <- getBasicHeaders()

      httr::PUT(url=URLencode(paste0(apiAddress, url)), config=httr::add_headers(headers), body=data)
    },
    deleteHelper = function(url) {
      headers <- getBasicHeaders()

      httr::DELETE(url=URLencode(paste0(apiAddress, url)), config=httr::add_headers(headers))
    },
    patchJsonHelper = function(url, input) {
      headers <- getBasicHeaders()
      headers["Content-Type"] <- 'application/json'

      httr::PATCH(url=URLencode(paste0(apiAddress, url)), config=httr::add_headers(headers), body=rjson::toJSON(input))
    }
  )
)

#' Creates a new Algorithmia Client which you can use to call algorithms and
#' interact with directories and files in the Algorithmia data API.
#'
#' @param apiKey The Algorithmia API key. You need to set this when you are
#' interacting with Algorithmia outside of an algorithm. To find your
#' Algorithmia API key visit: https://algorithmia.com/users/[YOUR USER NAME]
#'
#' @param apiAddress The Algorithmia API address. Normal users should not set
#' this. This defaults to "https://api.algorithmia.com" when it is not
#' explicitly set.
#'
#' @return A new AlgorithmiaClient object
#'
#' @examples
#' client <- algorithmia::getAlgorithmiaClient() # Inside an Algorithmia algorithm
#' client <- algorithmia::getAlgorithmiaClient("YOUR_ALGORITHMIA_API_KEY") # Everywhere else
getAlgorithmiaClient <- function(apiKey=NA_character_, apiAddress=NA_character_) {
  AlgorithmiaClient$new(apiKey=apiKey, apiAddress=getAlgorithmiaApiAddress(apiAddress))
}
