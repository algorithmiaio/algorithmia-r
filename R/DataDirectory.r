#' @include Client.r

DATA_OBJECT_TYPE_FILE <- "FILE"
DATA_OBJECT_TYPE_DIRECTORY <- "DIRECTORY"

getChildPath <- function(parentPath, childName) {
  childPath <- NULL
  if (endsWith(parentPath, "/")) {
    childPath <- paste0(parentPath, childName)
  } else {
    childPath <- paste0(parentPath, "/", childName)
  }
  childPath
}

#' DataDirectory object to interact with directories. Supports Algorithmia data
#' directories, S3, Dropbox and more coming soon!
#' To create one, call: `client$dir("directory_path")`
#'
#' @field client Reference to the AlgorithmiaClient object that has the credentials
#'        necessary to make API calls.
#' @field dataDirectoryUrl Convenience field that holds "/v1/data/" + dataDirectoryPath
#' @field dataDirectoryPath The path of to the directory
AlgorithmiaDataDirectory <- methods::setRefClass("AlgorithmiaDataDirectory",
  field = list(client = "AlgorithmiaClient", dataDirectoryUrl = "character", dataDirectoryPath = "character"),
  methods = list(
    getName = function() {
      "Returns the name of the innermost directory."
      trimmed <- gsub("/+$", "", dataDirectoryUrl)
      gsub("^.*/", "", trimmed)
    },
    getParent = function () {
      "Returns the url for the parent directory (everything but the innermost directory)."
      trimmed <- gsub("/+$", "", dataDirectoryUrl)
      gsub("/[^/]+$", "", trimmed)
    },
    exists = function() {
      "Returns TRUE if this directory exists. FALSE, if it does not."
      # Heading a directory apparently isn't a valid operation, use get instead
      checkFor200StatusCode(client$getHelper(dataDirectoryUrl))
    },
    create = function(acl=NULL) {
      "Creates the directory with the ACL provided."
      data <- if (!is.null(acl)) {
        list(name=getName(), acl=acl$getApiQueryList())
      } else {
        list(name=getName())
      }

      parent <- getParent()
      response <- client$postJsonHelper(parent, data)
      checkResponse(.self, response)
    },
    delete = function(force=FALSE) {
      "Deletes the directory. If the directory is not empty, it will fail unless `force`
       is set to TRUE."
      url <- dataDirectoryUrl
      if (force) {
        url <- paste0(url, "?force=true")
      }
      response <- client$deleteHelper(url)
      checkResponse(.self, response)
    },
    file = function(name) {
      "Returns an AlgorithmiaDataFile object for the child file."
      getDataFile(client, getChildPath(dataDirectoryPath, name))
    },
    dir = function(name) {
      "Returns an AlgorithmiaDataDirectory object for the child directory."
      getDataDirectory(client, getChildPath(dataDirectoryPath, name))
    },
    getDirectoryEntries = function(marker) {
      if (!is.na(marker)) {
        queryParameters <- list(marker=marker)
        httr::content(client$getHelper(dataDirectoryUrl, queryParameters=queryParameters))
      } else {
        httr::content(client$getHelper(dataDirectoryUrl))
      }
    },
    files = function() {
      "Returns an AlgorithmiaDirectoryIterator of all the child files."
      getIterator(.self, DATA_OBJECT_TYPE_FILE)
    },
    dirs = function() {
      "Returns an AlgorithmiaDirectoryIterator of all the child directories."
      getIterator(.self, DATA_OBJECT_TYPE_DIRECTORY)
    },
    getPermissions = function() {
      "Returns the AlgorithmiaAcl object representing the permissions of the directory."
      response <- httr::content(client$getHelper(dataDirectoryUrl, queryParameters=list(acl='true')))
      if ("acl" %in% names(response)) {
        getAcl(response$acl)
      } else {
        NULL
      }
    },
    updatePermissions = function(acl) {
      "Updates the permissions of the directory. Currently supported ACLs are:
       `ReadAcl.PUBLIC`, `ReadAcl.PRIVATE`, and `ReadAcl.MY_ALGORITHMS`."
      input <- list(acl=acl$getApiQueryList())
      response <- client$patchJsonHelper(dataDirectoryUrl, input)
      checkResponse(.self, response)
    }
  )
)

#' DirectoryIterator object to iterate over child files or directories of a parent directory.
#' To create one, call: `dataDirectory$dirs()` or `dataDirectory$files()`
#'
#' @field dataDirectory Parent directory whose children we are iterating over.
#' @field typeFilter Either DATA_OBJECT_TYPE_FILE or DATA_OBJECT_TYPE_DIRECTORY.
#' @field marker Page marker while iterating over a directory with lots of entries.
#' @field first Whether we have asked the server for anything for the first time.
#' @field index The current position of the directoryEntries list.
#' @field directoryEntries The list of children entries we are iterating over.
AlgorithmiaDirectoryIterator <- methods::setRefClass("AlgorithmiaDirectoryIterator",
  fields = list(dataDirectory = "AlgorithmiaDataDirectory", typeFilter="character",
                marker="character", first="logical", index="numeric",
                directoryEntries="list"),
  methods = list(
    hasNext = function() {
      "Returns TRUE if there are any more elements. FALSE otherwise."
      if (first || (index > length(directoryEntries) && !is.na(marker))) {
        # do get and set marker
        data <- dataDirectory$getDirectoryEntries(marker)
        if ("marker" %in% names(data)) {
          marker <<- data$marker
        } else {
          marker <<- NA_character_
        }

        numElements <- 0
        if (is.na(typeFilter) || typeFilter == DATA_OBJECT_TYPE_FILE) {
          numElements <- length(data$files)
        }
        if (is.na(typeFilter) || typeFilter == DATA_OBJECT_TYPE_DIRECTORY) {
          numElements <- length(data$folders)
        }

        directoryEntries <<- vector(mode="list", length=numElements)

        curIndex <- 1
        if (is.na(typeFilter) || typeFilter == DATA_OBJECT_TYPE_FILE) {
          fileCount <- 0
          while (fileCount < length(data$files)) {
            directoryEntries[[curIndex]] <<- list(
              name=data$files[[fileCount + 1]]$filename,
              isFile=TRUE,
              last_modified=data$files[[fileCount + 1]]$last_modified,
              size=data$files[[fileCount + 1]]$size)

            curIndex <- curIndex + 1
            fileCount <- fileCount + 1
          }
        }

        if (is.na(typeFilter) || typeFilter == DATA_OBJECT_TYPE_DIRECTORY) {
          folderCount <- 0
          while (folderCount < length(data$folders)) {
            directoryEntries[[curIndex]] <<- list(
              name=data$folders[[folderCount + 1]]$name,
              isFile=FALSE)

            curIndex <- curIndex + 1
            folderCount <- folderCount + 1
          }
        }

        index <<- 1
        first <<- FALSE
      }
      index <= length(directoryEntries)
    },
    getNext = function() {
      "Returns the next element or stops if you have asked for a next element that does not exist."
      if (hasNext()) {
        result <- NULL
        if (directoryEntries[[index]]$isFile) {
          result <- dataDirectory$file(directoryEntries[[index]]$name)
          result$setAttributes(directoryEntries[[index]])
        } else {
          result <- dataDirectory$dir(directoryEntries[[index]]$name)
        }
        index <<- index + 1
        result
      } else {
        stop("There are no elements left")
      }
    }
  )
)

getIterator <- function(dataDirectory, typeFilter) {
  AlgorithmiaDirectoryIterator$new(dataDirectory=dataDirectory, typeFilter=typeFilter,
      marker=NA_character_, first=TRUE, index=1, directoryEntries=vector(mode="list"))
}

getDataDirectory <- function(client, dataRef) {
  AlgorithmiaDataDirectory$new(client=client,
    dataDirectoryUrl=getDataUrl(dataRef, FALSE),
    dataDirectoryPath=getDataPath(dataRef, FALSE))
}
