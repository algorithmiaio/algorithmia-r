library("base64enc")
library("rjson")
AlgorithmHandler <- methods::setRefClass("AlgorithmHandler",
                                         fields = list(applyMethod = "function",
                                                       onLoadMethod = "function",
                                                       pipe_name = "character"),
                                         methods = list(
                                           serve = function(){
                                             getInputData_ <- function(input) {
                                               if (input$content_type == "binary") {
                                                 base64enc::base64decode(input$data)
                                               } else {
                                                 input$data
                                               }
                                             }
                                             getResponseObject_ <- function(output) {
                                               if (typeof(output) == "raw") {
                                                 list(result=base64enc::base64encode(output), metadata=list(content_type="binary"))
                                               } else if (is.character(output) & length(output) == 1) {
                                                 list(result=output, metadata=list(content_type="text"))
                                               } else {
                                                 list(result=output, metadata=list(content_type="json"))
                                               }
                                             }
                                             getResponseAsJsonString_ <- function(output) {
                                               tryCatch({
                                                 rjson::toJSON(output)
                                               },
                                               error = function(e) {
                                                 print(paste0("Error in getResponse: ", e))
                                                 rjson::toJSON(list(error=list(message=toString(e),
                                                                               stacktrace="pipe.r:getResponseAsJsonString", error_type="AlgorithmError")))
                                               })
                                             }
                                            runLoad_ <- function(){
                                              state <- onLoadMethod()
                                              print("PIPE_INIT_COMPLETE")
                                              flush.console()
                                              state
                                            }
                                            outputFile <- file("/tmp/algoout")
                                            inputFile <- file(pipe_name)
                                            open(inputFile)
                                            result <- tryCatch({
                                            stage <- "loading"
                                            state <- runLoad_()
                                            list(state=state)
                                            },
                                            error = function(e){
                                              message <- toString(e)
                                              formatted <- gsub("\n", "", message)
                                              list(error=list(message=formatted, stacktrace=stage, error_type="AlgorithmError"))
                                            })
                                            if(is.null(result$error)){
                                            state <- result$state
                                            while (length(line <- readLines(inputFile, n=1)) > 0) {
                                              stage <- "parsing"
                                              output <- tryCatch({
                                                input <- rjson::fromJSON(line)
                                                inputData <- getInputData_(input)
                                                stage <- "algorithm"
                                                if(is.null(state)){
                                                  output <- applyMethod(inputData)
                                                } else {
                                                  output <- applyMethod(inputData, state)
                                                }
                                                getResponseObject_(output)
                                              },
                                              error = function(e) {
                                                message <- toString(e)
                                                formatted <- gsub("\n", "", message)
                                                list(error=list(message=formatted, stacktrace=stage, error_type="AlgorithmError"))
                                              })
                                              
                                              # Flush stdout before writing back response
                                              flush.console()
                                              
                                              response = getResponseAsJsonString_(output)
                                              writeLines(response, con=outputFile)
                                            }
                                            } else{
                                              # Flush stdout before writing back response
                                              flush.console()
                                              response = getResponseAsJsonString_(result)
                                              writeLines(response, con=outputFile)
                                            }
                                            close(outputFile)
                                            close(inputFile)
                                           }
                                         ))


getAlgorithmHandler <- function(applyfunc, onLoadMethod=function(){NULL}, pipe='stdin'){
  AlgorithmHandler$new(applyMethod=applyfunc, onLoadMethod=onLoadMethod, pipe_name=pipe)
}
