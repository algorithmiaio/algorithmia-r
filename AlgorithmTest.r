library("RUnit")
library("tools")

source("Algorithm.r")

test.getAlgorithmPathErrors <- function() {
  checkException(getAlgorithmUrl(1))
  checkException(getAlgorithmUrl("BLAH"))
}

test.getAlgorithmPath <- function() {
  checkEquals(getAlgorithmUrl("a/b"), "/v1/algo/a/b")
  checkEquals(getAlgorithmUrl("/c/d"), "/v1/algo/c/d")
  checkEquals(getAlgorithmUrl("algo://e/f"), "/v1/algo/e/f")
}

test.hasResultAndMetadata <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://demo/hello")
  response <- algorithm$pipe(1)
  checkEquals(names(response), c("metadata", "result"))
  checkEquals(names(response$metadata), c("content_type", "duration"))
  checkEquals(response$metadata$content_type, "text")
}

test.runHelloWorld <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://demo/hello")
  checkEquals(algorithm$pipe(NULL)$result, "Hello null")
  checkEquals(algorithm$pipe(1)$result, "Hello 1")
}

test.runHelloWorldUnauthenticated <-function() {
  client <- getAlgorithmiaClient()
  algorithm <- client$algo("algo://demo/hello")
  checkException(algorithm$pipe(NULL))
}

test.runNonexistantAlgo <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://demo/thisshouldneverexist")
  checkException(algorithm$pipe(NULL))
}

test.checkTimeout <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://testing/Sleep")
  algorithm$setOptions(timeout=2)
  checkException(algorithm$pipe(10))
}

test.runWithNULLInputOutput <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://util/Echo")
  is.null(algorithm$pipe(NULL)$result)
}

test.runWithStringInputOutput <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://util/Echo")
  checkEquals(algorithm$pipe("hello")$result, "hello")
}

test.runWithIntegerInputOutput <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://util/Echo")
  checkEquals(algorithm$pipe(10)$result, 10)
}

test.runWithRawInputOutput <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://quality/Python2xEcho")
  checkEquals(algorithm$pipe(raw(10))$result, raw(10))
}

test.runAsync <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://testing/Sleep")
  algorithm$setOptions(output="void")
  result <- algorithm$pipe(10)
  checkEquals(c("async", "request_id"), names(result))
}

test.runRaw <-function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  algorithm <- client$algo("algo://quality/Python2xEcho")
  algorithm$setOptions(output="raw")
  checkEquals(rawToChar(algorithm$pipe("hello")), "hello")
}
