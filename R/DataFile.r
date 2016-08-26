#' @include Client.r

getDataFile <- function(client, dataRef) {
  AlgorithmiaDataFile$new(client=client, dataFileUrl=getDataUrl(dataRef, TRUE), last_modified=NA, size=NA_integer_)
}

#' DataFile object to interact with files. Supports Algorithmia data
#' files, S3, Dropbox and more coming soon!
#' To create one, call: `client$file("file_path")`
#'
#' @field client Reference to the AlgorithmiaClient object that has the credentials
#'        necessary to make API calls.
#' @field dataFileUrl Url to access the file.
#' @field last_modified A timestamp for the last modified time. Only gets set when setAttributes is called.
#' @field size Size (in bytes) of the file. Only gets set when setAttributes is called.
AlgorithmiaDataFile <- methods::setRefClass("AlgorithmiaDataFile",
  field = list(client = "AlgorithmiaClient", dataFileUrl = "character", last_modified = "ANY", size = "numeric"),
  methods = list(
    exists = function() {
      "Returns TRUE if this file exists. FALSE, if it does not."
      checkFor200StatusCode(client$headHelper(dataFileUrl))
    },
    getName = function() {
      "Returns the name of the file without the parent directory path."
      gsub("^.*/", "", dataFileUrl)
    },
    setAttributes = function(attributes) {
      "Sets the last_modified time and size (in bytes) of the file."
      last_modified <<- strptime(attributes$last_modified, "%Y-%m-%dT%H:%M:%S.000Z")
      size <<- attributes$size
    },
    getJson = function() {
      "Returns the contents of the file after it has been JSON decoded."
      if (!exists()) {
        stop(paste0("file does not exist ", dataFileUrl))
      }
      response <- client$getHelper(dataFileUrl)
      checkResponse(.self, response)
      rjson::fromJSON(httr::content(response, as="text"))
    },
    getString = function() {
      "Returns the contents of the file as a string."
      if (!exists()) {
        stop(paste0("file does not exist ", dataFileUrl))
      }
      response <- client$getHelper(dataFileUrl)
      checkResponse(.self, response)
      httr::content(response, as="text")
    },
    getRaw = function() {
      "Returns the raw contents of the file."
      if (!exists()) {
        stop(paste0("file does not exist ", dataFileUrl))
      }
      response <- client$getHelper(dataFileUrl)
      checkResponse(.self, response)
      httr::content(response, as="raw")
    },
    getFile = function() {
      "Copies the file to a tempfile and returns the path to that file."
      if (!exists()) {
        stop(paste0("file does not exist ", dataFileUrl))
      }
      tempLocation <- tempfile()
      response <- client$getHelper(dataFileUrl, targetFile=tempLocation)
      checkResponse(.self, response)
      tempLocation
    },
    put = function(data) {
      "Writes the file with the current data passed in."
      response <- client$putHelper(dataFileUrl, data)
      checkResponse(.self, response)
    },
    putJson = function(data) {
      "Encodes the data to a JSON object and writes that to the file."
      response <- client$putHelper(dataFileUrl, rjson::toJSON(data))
      checkResponse(.self, response)
    },
    putFile = function(fileName) {
      "Takes a local file path and writes its contents to this file."
      response <- client$putHelper(dataFileUrl, httr::upload_file(fileName))
      checkResponse(.self, response)
    },
    delete = function() {
      "Deletes the file."
      response <- client$deleteHelper(dataFileUrl)
      checkResponse(.self, response)
    }
  )
)
