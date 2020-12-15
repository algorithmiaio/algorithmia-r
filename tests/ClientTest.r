library("RUnit")
library("tools")

DEFAULT_ALGORITHMIA_API_ADDRESS = "https://api.algorithmia.com"

test.getAlgorithmiaApiAddressFromClient <- function() {
  client <- getAlgorithmiaClient("doesn't matter", "BLAH")
  checkEquals(client$apiAddress, "BLAH")
  client <- getAlgorithmiaClient()
  checkEquals(client$apiAddress, DEFAULT_ALGORITHMIA_API_ADDRESS)
}

test.getAlgorithmiaApiAddressWithEnvironmentVariable <- function() {
  testEnvironmentVariable <- "from_env_variable"
  Sys.setenv(ALGORITHMIA_API = testEnvironmentVariable)
  client <- getAlgorithmiaClient()
  checkEquals(client$apiAddress, testEnvironmentVariable)
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

test.useCustomCACert <- function(){
  testfile <- "testpem.pem"
  file.create(testfile)
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA),customCert=testfile)
  algorithm <- client$algo("algo://demo/hello")
  checkEquals(algorithm$pipe(NULL)$result, "Hello null")
  checkEquals(algorithm$pipe(1)$result, "Hello 1")
  if (file.exists(testfile)) {
  file.remove(testfile)
}
}
