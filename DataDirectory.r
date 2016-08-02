source("data_utilities.r")

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

getIterator <- function(dataDirectory, typeFilter) {
  AlgorithmiaDirectoryIterator <- setRefClass("AlgorithmiaDirectoryIterator",
    fields = list(dataDirectory = "AlgorithmiaDataDirectory", typeFilter="character",
                  marker="character", first="logical", index="numeric",
                  directoryEntries="list"),
    methods = list(
      hasNext = function() {
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
  AlgorithmiaDirectoryIterator$new(dataDirectory=dataDirectory, typeFilter=typeFilter,
      marker=NA_character_, first=TRUE, index=1, directoryEntries=vector(mode="list"))
}

getDataDirectory <- function(client, dataRef) {
  AlgorithmiaDataDirectory <- setRefClass("AlgorithmiaDataDirectory",
    field = list(client = "AlgorithmiaClient", dataDirectoryUrl = "character", dataDirectoryPath = "character"),
    methods = list(
      getName = function() {
        trimmed <- gsub("/+$", "", dataDirectoryUrl)
        gsub("^.*/", "", trimmed)
      },
      getParent = function () {
        trimmed <- gsub("/+$", "", dataDirectoryUrl)
        gsub("/[^/]+$", "", trimmed)
      },
      exists = function() {
        # Heading a directory apparently isn't a valid operation, use get instead
        checkFor200StatusCode(client$getHelper(dataDirectoryUrl))
      },
      # TODO james - add ACL support
      create = function() {
        data <- list(name=getName())
        parent <- getParent()
        response <- client$postJsonHelper(parent, data)
        checkResponse(.self, response)
      },
      delete = function(force=FALSE) {
        url <- dataDirectoryUrl
        if (force) {
          url <- paste0(url, "?force=true")
        }
        response <- client$deleteHelper(url)
        checkResponse(.self, response)
      },
      file = function(name) {
        getDataFile(client, getChildPath(dataDirectoryPath, name))
      },
      dir = function(name) {
        getDataDirectory(client, getChildPath(dataDirectoryPath, name))
      },
      getDirectoryEntries = function(marker) {
        if (!is.na(marker)) {
          queryParameters <- list(marker=marker)
          content(client$getHelper(dataDirectoryUrl, queryParameters=queryParameters))
        } else {
          content(client$getHelper(dataDirectoryUrl))
        }
      },
      files = function() {
        getIterator(.self, DATA_OBJECT_TYPE_FILE)
      },
      dirs = function() {
        getIterator(.self, DATA_OBJECT_TYPE_DIRECTORY)
      }
    )
  )
  AlgorithmiaDataDirectory$new(client=client,
    dataDirectoryUrl=getDataUrl(dataRef, FALSE),
    dataDirectoryPath=getDataPath(dataRef, FALSE))
}
