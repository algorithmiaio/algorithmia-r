library("RUnit")
library("tools")

test.invalidPath <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  checkException(client$file(""))
  checkException(client$file("/"))
  checkException(client$file("data://"))
  checkException(client$file("data://blah/file.txt/"))
}

test.getName <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  dataFile <- client$file("a")
  checkEquals(dataFile$getName(), "a")

  dataFile <- client$file("really/long/path")
  checkEquals(dataFile$getName(), "path")

  dataFile <- client$file("data://a/b/c/d.txt")
  checkEquals(dataFile$getName(), "d.txt")
}

test.set_attributes <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  dataFile <- client$file("a")

  # When it is first created the attributes should be NA
  checkTrue(is.na(dataFile$last_modified))
  checkTrue(is.na(dataFile$size))

  last_modified <- "2016-07-31T05:29:44.000Z"
  size <- 5
  attributes <- list(last_modified=last_modified, size=size)
  dataFile$setAttributes(attributes)
  checkEquals(strptime(last_modified, "%Y-%m-%dT%H:%M:%S.000Z"), dataFile$last_modified)
  checkEquals(size, dataFile$size)
}

test.deleteNonExistantFile <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  dataFile <- client$file("data://.my/doesNotExist/doesNotExist.txt")
  checkTrue(!dataFile$exists())
  checkException(dataFile$delete())
}

test.fileCreationAndDeletion <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  dataDir <- client$dir("data://.my/largeFiles")
  if(!dataDir$exists()) {
    dataDir$create()
  }
  dataFile <- client$file("data://.my/largeFiles/R.file")

  checkTrue(!dataFile$exists())

  # make the file and check that it exists
  checkEquals(dataFile$putJson(1), dataFile)
  checkTrue(dataFile$exists())

  # delete the file and make sure it's gone for good
  checkEquals(dataFile$delete(), dataFile)
  checkTrue(!dataFile$exists())
}

test.filePutJsonAndGetJson <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  dataDir <- client$dir("data://.my/largeFiles")
  if(!dataDir$exists()) {
    dataDir$create()
  }
  dataFile <- client$file("data://.my/largeFiles/RjsonFile")
  input <- list(a=1, b=2)
  checkEquals(dataFile$putJson(input), dataFile)
  checkEquals(dataFile$getJson(), input)

  checkEquals(dataFile$putJson(2), dataFile)
  checkEquals(dataFile$getJson(), 2)

  checkEquals(dataFile$putJson("hello"), dataFile)
  checkEquals(dataFile$getJson(), "hello")
}

test.getString <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  dataDir <- client$dir("data://.my/largeFiles")
  if(!dataDir$exists()) {
    dataDir$create()
  }
  dataFile <- client$file("data://.my/largeFiles/RstringFile")

  input <- "what"
  checkEquals(dataFile$put(input), dataFile)
  checkEquals(dataFile$getString(), input)
}

test.getRaw <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  dataDir <- client$dir("data://.my/largeFiles")
  if(!dataDir$exists()) {
    dataDir$create()
  }
  dataFile <- client$file("data://.my/largeFiles/RstringFile")

  input <- raw(10)
  checkEquals(dataFile$put(input), dataFile)
  checkEquals(dataFile$getRaw(), input)
}

test.makeAndGetFile <- function() {
  LIMIT <- 1000000
  options(scipen = 999)

  filePath <- tempfile()
  f <- file(filePath, open="w")
  count <- 0
  while (count < LIMIT) {
    writeLines(as.character(count), f)
    count <- count + 1
  }
  close(f)

  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  directory <- client$dir("data://.my/largeFiles")
  if(!directory$exists()) {
    directory$create()
  }
  dataFile <- client$file("data://.my/largeFiles/R1000000Numbers")
  checkEquals(dataFile$putFile(filePath), dataFile)

  downloadedFilePath <- dataFile$getFile()
  lines <- readLines(downloadedFilePath)
  checkEquals(length(lines), LIMIT)
  count <- 1
  while (count <= LIMIT) {
    checkEquals(count-1, as.integer(lines[count]))
    count <- count + 1
  }
}
