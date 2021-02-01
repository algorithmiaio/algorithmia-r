library("RUnit")
library("tools")

test.getAlgorithm <- function(){
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkEquals(client$getAlgorithmObject("J_bragg/Echo")$resource_type,"algorithm")
}

test.createAlgorithm <- function(){
  payload <- '{
    "details": 
    {
      "label": "My First Algorithm1"
    },
    "name": "my_first_algorithm1",
    "settings": {
      "environment": "cpu",
      "language": "python3-1",
      "license": "apl",
      "network_access": "full",
      "pipeline_enabled": true,
      "source_visibility": "closed" 
      }
    }'
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  response <- client$createAlgorithm("J_bragg",payload)
  if(!is.null(response$error)){
    print(response$error)
  }
  checkEquals(response$resource_type,"algorithm")
}

test.deleteAlgorithm <- function(){
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  response <- client$deleteAlgorithm("J_bragg/my_first_algorithm1")
  if(!is.null(response$error)){
    print(response$error)
  }
  checkEquals(httr::status_code(response),204)
}

test.publishAlgorithm <- function() {
  payload <- '{
    "settings": {
      "algorithm_callability": "private"
    },
    "version_info": {
      "version_type": "minor",
      "release_notes": "A few bug fixes.",
      "sample_input": "42"
    }
  }' 

  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  response <- client$publishAlgorithm("J_bragg/Echo",payload)
  if(!is.null(response$error)){
    print(response$error)
  }
  checkEquals(response$resource_type,"algorithm")
}

test.updateAlgorithm <- function(){

  payload <- '{
    "details": {
      "label": "My Updated Algorithm"
    },
    "settings": {
      "environment": "gpu",
      "license": "apl",
      "network_access": "full",
      "pipeline_enabled": true,
      "source_visibility": "closed"
    }
  }'
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  response <- client$updateAlgorithm("J_bragg/my_first_algorithm",payload)
  if(!is.null(response$error)){
    print(response$error)
  }
  checkEquals(response$resource_type,"algorithm")
}

test.compileAlgorithm <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  response <- client$compileAlgorithm("J_bragg/Echo")
  if(!is.null(response$error)){
    print(response$error)
  }
  checkTrue(!is.null(response$id),TRUE)
}

## ORG
test.getOrgTypes <- function(){
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_A_KEY",unset=NA),Sys.getenv("ALGORITHMIA_TEST_API",unset=NA))
  response <- client$getOrgTypes()
  checkTrue(length(client$getOrgTypes())>0,TRUE)
}

test.createOrg <- function(){
  newOrg = paste0("a_myOrg",round(runif(1)*100))
  payload <- sprintf('{
    "org_name":"%s",
    "org_label": "some other label",
    "org_contact_name": "Some owner",
    "org_email": "a_myOrg@algo.com",
    "type_id":"basic"
  }',newOrg)

  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_A_KEY",unset=NA),Sys.getenv("ALGORITHMIA_TEST_API",unset=NA))
  response <- client$createOrg(payload)
  checkEquals(response$org_name,newOrg)
}

test.getOrg <- function(){
  orgName = "a_myOrg84"
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_A_KEY",unset=NA),Sys.getenv("ALGORITHMIA_TEST_API",unset=NA))
  response <- client$getOrg(orgName)
  checkEquals(response$org_name,orgName)

}

test.edit_Org <- function(){
  orgName = "a_myOrg84"
  payload <- '{
    "id": "b85d8c4e-7f3c-40b9-9659-6adc2cb0e16f",
    "org_label": "some other label",
    "org_contact_name": "Some owner",
    "org_email": "a_myOrg84@algo.com",
    "type_id": "3d40e3b0-d82a-11ea-9a3c-0ee5e2d35097",
    "resource_type": "organization"
  }'
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_A_KEY",unset=NA),Sys.getenv("ALGORITHMIA_TEST_API",unset=NA))
  response <- client$editOrg(orgName,payload)
  checkEquals(httr::status_code(response),204)
}

## SCM
test.getSCMStatus <- function(){
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkTrue(!is.null(client$getAlgoSCMStatus("J_bragg/Echo")$scm_connection_status),TRUE)
}

test.listSCMs <- function(){
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkTrue(length(client$listSCMs()$results)>0,TRUE)
}

test.getSCM <- function(){
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkTrue(!is.null(client$getSCM("internal")$id),TRUE)
}

test.getSCMAuthStatus <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkTrue(!is.null(client$getSCMAuthStatus("internal")$authorization_status),TRUE)
}

# test.revokeSCMAuth <- function(){
#   client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_TAPI_KEY",unset=NA),(Sys.getenv("ALGORITHMIA_TEST_API",unset=NA)))
#   response <-client$revokeSCMAuth("fa359f8a-5a37-4726-9174-1475b41939ef")
#   print(response)
#   checkEquals(httr::status_code(response),204)
# }

test.listAlgoVersions = function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkTrue(length(client$listAlgoBuilds("J_bragg/Echo")$results)>0,TRUE)
}

test.listAlgoBuilds <- function(){
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkTrue(length(client$listAlgoBuilds("J_bragg/Echo")$results)>0,TRUE)
}

test.getAlgoBuild <- function(){
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkTrue(!is.null(client$getAlgoBuild("J_bragg/Echo","1a392e2c-b09f-4bae-a616-56c0830ac8e5")$build_id),TRUE)
}

test.getAlgoBuildLogs <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY",unset=NA))
  checkTrue(length(client$getAlgoBuildLogs("J_bragg/Echo","1a392e2c-b09f-4bae-a616-56c0830ac8e5")$logs)>0,TRUE)
}

test.getAlgorithmPathErrors <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  checkException(client$algo(1))
  checkException(client$algo("BLAH"))
}

test.getAlgorithmPath <- function() {
  client <- getAlgorithmiaClient(Sys.getenv("ALGORITHMIA_API_KEY", unset=NA))
  checkEquals(client$algo("a/b")$algoUrl, "/v1/algo/a/b")
  checkEquals(client$algo("/c/d")$algoUrl, "/v1/algo/c/d")
  checkEquals(client$algo("algo://e/f")$algoUrl, "/v1/algo/e/f")
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
  checkException(algorithm$pipe(100))
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
  result <- algorithm$pipe(raw(10))$result
  checkEquals(result, raw(10))
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
  result <- algorithm$pipe("hello")
  checkEquals(rawToChar(result), "hello")
}
