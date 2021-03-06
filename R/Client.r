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

getCert <- function(customCert=NA_character_){
  

  certfile <- here::here("R/crt/cacert.pem")
  rcert <- file.info(certfile, extra_cols = FALSE)
  #check to see if root cert has been modified in the last 24 weeks or ~5.5 months 
  if(!is.na(rcert[difftime(Sys.time(), rcert[,"mtime"], units = "weeks") > 24,1:4][1,1])){
    GET("https://raw.githubusercontent.com/bagder/ca-bundle/e9175fec5d0c4d42de24ed6d84a06d504d5e5a09/ca-bundle.crt",write_disk(certfile, overwrite = TRUE))
  } 
  
  newCustomCert <- ""

  if(!is.na(customCert)){
    newCustomCert <- catCert(customCert,certfile)
    newCustomCert <- customCert
  } else if (!is.na(Sys.getenv("REQUESTS_CA_BUNDLE", unset=NA))) {
    cert = Sys.getenv("REQUESTS_CA_BUNDLE", unset=NA)
    newCustomCert <- catCert(cert,certfile)
  } else if (!is.na(Sys.getenv("CURL_CA_BUNDLE", unset=NA))) {
    cert = Sys.getenv("CURL_CA_BUNDLE", unset=NA)
    newCustomCert <- catCert(cert,certfile)
  }  else {
    newCustomCert <- certfile
  }
  httr::set_config(httr::config(cainfo=newCustomCert))
  newCustomCert
}
catCert <- function(customCert,rootCert){
  tempfile = tempfile("cert",fileext = ".pem")
  data = readr::read_lines(customCert)
  write(data,tempfile,append=TRUE)
  data2 = readr::read_lines(rootCert)
  write(data2,tempfile,append=TRUE) 
  tempfile
}



#' Client object which makes it easy to interact with the Algorithmia REST API.
#' To create one, call `getAlgorithmiaClient("YOUR_ALGORITHMIA_API_KEY")`
#'
#' @field apiKey The API key used when making REST calls to Algorithmia. This
#'        should NOT be set inside algorithms.
#' @field apiAddress The Algorithmia API address. In most cases you don't need to
#'        set this explicitly since the default will talk to the correct
#'        Algorithmia API server.
#' @field customCert is the custom CA certficate. This can be set from environment variables or explicitly declared
AlgorithmiaClient <- methods::setRefClass("AlgorithmiaClient",
  fields = list(apiKey = "character", apiAddress = "character", customCert = "character"),
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
    getAlgorithmObject = function(algoUrl) {
      "Returns an algorithm object"
      url = paste0("/v1/algorithms/",algoUrl)
      response <- httr::content(getHelper(url),"parsed")
    },
    createAlgorithm = function(username,data) {
      "Creates and returns an algorithm object"
      url = paste0("/v1/algorithms/",username)
      response <- httr::content(postHelper(url,data))
    },
    compileAlgorithm = function(algoUrl){
      "returns algorithm object"
      url = paste0("/v1/algorithms/",algoUrl,"/compile")
      response <- httr::content(postJsonHelper(url,{}),"parsed")
    },
    updateAlgorithm = function(algoUrl, data){
      "returns updated algorithm object. @data must be a list of members"
      url = paste0("/v1/algorithms/",algoUrl)
      response <- httr::content(putHelper(url,data),"parsed")
    },
    publishAlgorithm = function(algoUrl, data){
      "publish most recent version of algorithm returns algorithm object"
      url = url = paste0("/v1/algorithms/",algoUrl,"/versions")
      response <- httr::content(postHelper(url,data))
    },
    deleteAlgorithm = function(algoUrl){
      url = paste0("/v1/algorithms/",algoUrl)
      response <- deleteHelper(url)
    },
    listAlgoVersions = function(algoUrl){
      url = paste0("/v1/algorithms/",algoUrl,"/versions")
      response <- httr::content(getHelper(url),"parsed")
    },
    listAlgoBuilds = function(algoUrl) {
      url = paste0("/v1/algorithms/",algoUrl,"/builds")
      response <- httr::content(getHelper(url),"parsed")
    },
    getAlgoBuild = function(algoUrl,buildId){
      url = paste0("/v1/algorithms/",algoUrl,"/builds/",buildId)
      response <- httr::content(getHelper(url),"parsed")
    },
    getAlgoBuildLogs = function(algoUrl, buildId){
      url = paste0("/v1/algorithms/",algoUrl,"/builds/",buildId,"/logs")
      response <- httr::content(getHelper(url),"parsed")
    },
    getAlgoSCMStatus = function(algoUrl){
      url = paste0("/v1/algorithms/",algoUrl,"/scm/status")
      response <- httr::content(getHelper(url),"parsed")
    },
    ## ORG 
    getOrgTypes = function(){
      url = "/v1/organization/types"
      response <- httr::content(getHelper(url),"parsed")
    },
    createOrg = function(inputObject){
      #takes in json object
      data = jsonlite::fromJSON(inputObject)
      converted = convertTypeid(data$type_id)
      data$type_id <- converted$id

      data = jsonlite::toJSON(data, auto_unbox = TRUE)
      url = "/v1/organizations"
      response <- httr::content(postHelper(url,data))

      if(!is.na(response$error$message) && converted$error!=""){
        response$error$message <- converted$error
        return(response)
      }else{
        return(response)
      }
    },
    getOrg = function(orgName){
      url = paste0("/v1/organizations/",orgName)
      response <- httr::content(getHelper(url),"parsed")
    },
    editOrg = function(orgName, inputObject){
      #takes in json object
      data = jsonlite::fromJSON(inputObject)
      converted = convertTypeid(data$type_id)
      data$type_id <- converted$id
      
      data = jsonlite::toJSON(data, auto_unbox = TRUE)
      url = paste0("/v1/organizations/",orgName)
      response <- putHelper(url,data)
      if(("error" %in% names(httr::content(response))) && (converted$error != "")){
        r = httr::content(response)
        r$error$message <- converted$error
        return(r)
      }else{
        return(response)
      }
    },
    ## SCMs
    listSCMs = function(){
      response <- httr::content(getHelper("/v1/scms"),"parsed")
    },
    getSCM = function(scmId){
      url = paste0("/v1/scms/",scmId)
      response <- httr::content(getHelper(url),"parsed")
    },
    getSCMAuthStatus = function(scmId){
      url = paste0("/v1/scms/",scmId,"/oauth/status")
      response <- httr::content(getHelper(url),"parsed")
    },
    revokeSCMAuth = function(scmId){
      url = paste0("/v1/scms/",scmId,"/oauth/revoke")
      response <- postJsonHelper(url,{})
    },
    ## Helper functions
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
    postHelper = function(algoUrl, input, queryParameters=c()){
      "post helper that doesnt change formatting"
      headers <- getBasicHeaders()
      headers["Content-Type"] <- 'application/json'
      response <- httr::POST(url=URLencode(paste0(apiAddress, algoUrl)), query={}, config=httr::add_headers(headers), body=input)
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
      httr::PUT(url=URLencode(paste0(apiAddress, url)), config=httr::add_headers(headers), body=data, httr::content_type_json())
    },
    deleteHelper = function(url) {
      headers <- getBasicHeaders()

      httr::DELETE(url=URLencode(paste0(apiAddress, url)), config=httr::add_headers(headers))
    },
    patchJsonHelper = function(url, input) {
      headers <- getBasicHeaders()
      headers["Content-Type"] <- 'application/json'

      httr::PATCH(url=URLencode(paste0(apiAddress, url)), config=httr::add_headers(headers), body=rjson::toJSON(input))
    },
    convertTypeid = function(typeid){
      types = httr::content(getHelper("/v1/organization/types"),"parsed")
      error=""
      id=""
      for (type in types){
        if(type$name == typeid){
          error=""
          id = type$id
          break
        }else{
          error="invalid type_id"
        }
      }
      rList <- list()
      rList["error"] <- error
      rList["id"] <- id
      return(rList)
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
#' @param customCert (optional) Custom CA Certificate
#'
#' @return A new AlgorithmiaClient object
#'
#' @examples
#' client <- algorithmia::getAlgorithmiaClient() # Inside an Algorithmia algorithm
#' client <- algorithmia::getAlgorithmiaClient("YOUR_ALGORITHMIA_API_KEY") # Everywhere else
getAlgorithmiaClient <- function(apiKey=NA_character_, apiAddress=NA_character_,customCert=NA_character_) {
  AlgorithmiaClient$new(apiKey=apiKey, apiAddress=getAlgorithmiaApiAddress(apiAddress),customCert=getCert(customCert))
}
