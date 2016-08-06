#' @include Client.r

library("methods")
library("httr")
library("rjson")

AlgorithmiaDataFile <- setRefClass("AlgorithmiaDataFile",
  field = list(client = "AlgorithmiaClient", dataFileUrl = "character", last_modified = "ANY", size = "numeric"),
  methods = list(
    exists = function() {
      checkFor200StatusCode(client$headHelper(dataFileUrl))
    },
    getName = function() {
      gsub("^.*/", "", dataFileUrl)
    },
    setAttributes = function(attributes) {
      last_modified <<- strptime(attributes$last_modified, "%Y-%m-%dT%H:%M:%S.000Z")
      size <<- attributes$size
    },
    getJson = function() {
      if (!exists()) {
        stop(paste0("file does not exist ", dataFileUrl))
      }
      response <- client$getHelper(dataFileUrl)
      checkResponse(.self, response)
      rjson::fromJSON(httr::content(response, as="text"))
    },
    getString = function() {
      if (!exists()) {
        stop(paste0("file does not exist ", dataFileUrl))
      }
      response <- client$getHelper(dataFileUrl)
      checkResponse(.self, response)
      httr::content(response, as="text")
    },
    getRaw = function() {
      if (!exists()) {
        stop(paste0("file does not exist ", dataFileUrl))
      }
      response <- client$getHelper(dataFileUrl)
      checkResponse(.self, response)
      httr::content(response, as="raw")
    },
    getFile = function() {
      if (!exists()) {
        stop(paste0("file does not exist ", dataFileUrl))
      }
      tempLocation <- tempfile()
      response <- client$getHelper(dataFileUrl, targetFile=tempLocation)
      checkResponse(.self, response)
      tempLocation
    },
    put = function(data) {
      response <- client$putHelper(dataFileUrl, data)
      checkResponse(.self, response)
    },
    putJson = function(data) {
      response <- client$putHelper(dataFileUrl, rjson::toJSON(data))
      checkResponse(.self, response)
    },
    putFile = function(fileName) {
      response <- client$putHelper(dataFileUrl, httr::upload_file(fileName))
      checkResponse(.self, response)
    },
    delete = function() {
      response <- client$deleteHelper(dataFileUrl)
      checkResponse(.self, response)
    }
  )
)

getDataFile <- function(client, dataRef) {
  AlgorithmiaDataFile$new(client=client, dataFileUrl=getDataUrl(dataRef, TRUE), last_modified=NA, size=NA_integer_)
}
