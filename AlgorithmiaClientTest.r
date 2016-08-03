library("RUnit")
library("tools")

source("AlgorithmiaClient.r")

test.getAlgorithmiaApiAddress <- function() {
  checkEquals(getAlgorithmiaApiAddress("BLAH"), "BLAH")
  checkEquals(getAlgorithmiaApiAddress(), DEFAULT_ALGORITHMIA_API_ADDRESS)
}

test.getAlgorithmiaApiAddressWithEnvironmentVariable <- function() {
  testEnvironmentVariable <- "from_env_variable"
  Sys.setenv(ALGORITHMIA_API = testEnvironmentVariable)
  checkEquals(getAlgorithmiaApiAddress(), testEnvironmentVariable)
  Sys.unsetenv("ALGORITHMIA_API")
}

test.getAlgorithmiaClientWithDefaults <- function() {
  client <- getAlgorithmiaClient()
  checkTrue(is.na(client$apiKey))
  checkEquals(client$apiAddress, DEFAULT_ALGORITHMIA_API_ADDRESS)
}

test.getAlgorithmiaClient <- function() {
  testKey <- "testKey"
  testApiAddress <- "testApi"
  client <- getAlgorithmiaClient(testKey, testApiAddress)
  checkEquals(client$apiKey, testKey)
  checkEquals(client$apiAddress, testApiAddress)
}

test.getAlgorithmiaAlgorithm <- function() {
  client <- getAlgorithmiaClient()
  algorithm <- client$algo("algo://test/algo")
  checkTrue(inherits(algorithm, "AlgorithmiaAlgorithm"))
  checkEquals(algorithm$client, client)
}

test.getAlgorithmiaDataFile <- function() {
  client <- getAlgorithmiaClient()
  dataFile <- client$file("test/path")
  checkTrue(inherits(dataFile, "AlgorithmiaDataFile"))
  checkEquals(dataFile$client, client)
}

test.getAlgorithmiaDataDirectory <- function() {
  client <- getAlgorithmiaClient()
  dataDirectory <- client$dir("test/path")
  checkTrue(inherits(dataDirectory, "AlgorithmiaDataDirectory"))
  checkEquals(dataDirectory$client, client)
}