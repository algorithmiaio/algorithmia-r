library("base64enc")
library("rjson")
AlgorithmHandler <- methods::setRefClass("AlgorithmHandler",
                                         fields = list(applyMethod = "function",
                                                       onLoadMethod = "function",
                                                       context = "list"),
                                         methods = list(
                                           run = function(){
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
                                                 rjson::toJSON(list(error=list(message=toString(e), stacktrace="pipe.r:getResponseAsJsonString", error_type="AlgorithmError")))
                                               })
                                             }
                                             
                                            self.context <- self.onLoadMethod()
                                            print("PIPE_INIT_COMPLETE")
                                            flush.console()
                                            
                                            outputFile <- fifo("/tmp/algoout", blocking=TRUE)
                                            inputFile <- file("stdin")
                                            open(inputFile)
                                            
                                            while (length(line <- readLines(inputFile, n=1)) > 0) {
                                              stage <- "parsing"
                                              output <- tryCatch({
                                                input <- rjson::fromJSON(line)
                                                inputData <- getInputData_(input)
                                                stage <- "algorithm"
                                                output <- self.applyMethod(inputData)
                                                getResponseObject_(output)
                                              },
                                              error = function(e) {
                                                list(error=list(message=toString(e), stacktrace=stage, error_type="AlgorithmError"))
                                              })
                                              
                                              # Flush stdout before writing back response
                                              flush.console()
                                              
                                              response = getResponseAsJsonString_(output)
                                              writeLines(response, con=outputFile)
                                            }
                                           }
                                         ))


getAlgorithmHandler <- function(applyfunc, onLoadMethod=function(){}){
  AlgorithmHandler$new(applyMethod=applyfunc, onLoadMethod=onLoadMethod, context=list())
}
