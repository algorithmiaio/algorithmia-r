library("base64enc")
library("rjson")

getResponseObject_ <- function(output) {
    if (typeof(output) == "raw") {
      list(
        result = base64enc::base64encode(output),
        metadata = list(content_type = "binary")
      )
    } else if (is.character(output) & length(output) == 1) {
      list(result = output,
           metadata = list(content_type = "text"))
    } else {
      list(result = output,
           metadata = list(content_type = "json"))
    }
  }

getResponseAsJsonString_ <- function(output) {
    tryCatch({
      rjson::toJSON(output)
    },
    error = function(e) {
      print(paste0("Error in getResponse: ", e))
      rjson::toJSON(list(
        error = list(
          message = toString(e),
          stacktrace = "pipe.r:getResponseAsJsonString",
          error_type = "AlgorithmError"
        )
      ))
    })
  }

runLoad_ <- function(onLoadMethod) {
  state <- onLoadMethod()
  print("PIPE_INIT_COMPLETE")
  flush.console()
  state
}

AlgorithmHandler <- methods::setRefClass(
  "AlgorithmHandler",
  fields = list(
    applyMethod = "function",
    onLoadMethod = "function",
    pipeName = "character"
  ),
  methods = list(
    serve=function() {
      getInputData_ <- function(input) {
        if (input$content_type == "binary") {
          base64enc::base64decode(input$data)
        } else {
          input$data
        }
      }
      # Begin startup
      outputFile <- fifo("/tmp/algoout", blocking=TRUE)
      inputFile <- file(pipeName)
      open(inputFile)
      
      loadResult <- tryCatch({
        stage <- "loading"
        state <- runLoad_(onLoadMethod)
        list(state = state)
      },
      error = function(e) {
        message <- toString(e)
        formatted <-
          gsub("\n", "", message)
        list(error = list(
          message = formatted,
          stacktrace = stage,
          error_type = "AlgorithmError"
        ))
      })
      
      #Finished loading, check if we failed and if not - start main algorithm loop
      if (is.null(loadResult$error)) {
        state <- loadResult$state
        while (length(line <- readLines(inputFile, n = 1)) > 0) {
          stage <- "parsing"
          output <- tryCatch({
            input <- rjson::fromJSON(line)
            inputData <-
              getInputData_(input)
            stage <- "algorithm"
            if (is.null(state)) {
              output <- applyMethod(inputData)
            } else {
              output <- applyMethod(inputData, state)
            }
            getResponseObject_(output)
          },
          error = function(e) {
            message <- toString(e)
            formatted <-
              gsub("\n", "", message)
            list(error = list(
              message = formatted,
              stacktrace = stage,
              error_type = "AlgorithmError"
            ))
          })
          
          # Flush stdout before writing back response
          flush.console()
          response = getResponseAsJsonString_(output)
          writeLines(response, con = outputFile)
          # Finished writing response to algoout, checking stdin for more input.
        }
      } else{
        # Flush stdout before writing back response
        flush.console()
        response = getResponseAsJsonString_(result)
        writeLines(response, con = outputFile)
      }
      # Either the stdin pipe was closed, or we recieved in error. In any case we should shut down now
      # and close the pipes.
      close(inputFile)
      close(outputFile)
    }
  )
)

#' Creates a new Algorithm Handler which registers the onLoad method (which loads the model or other
#' dependencies for the algorithm) and the apply method which takes in the result of the onLoad method
#' and the input from the user.
#'
#' @param applyfunc The method that we will call synchronously for each algorithm call. The first argument
#' it will be called with is the user input, followed with the output of the onLoad function.
#'
#' @param onLoadMethod This optional method that is run once when the process first starts. It loads the
#' data required and other shared state for each algorithm call.
#'
#' @param pipe The file which we will read line-by-line to get user input
#'
#' @return A new AlgorithmHandler object
#'
#' @examples
#' loadPrefix <- function() { "Hello" }
#' algorithm <- function(input, prefix) { paste(prefix, input) }
#'
#' #To create an algorithm that returns "Hello" + input:
#' algo <- getAlgorithmHandler(algorithm, loadPrefix)
getAlgorithmHandler <-function(applyfunc, onLoadMethod = function() {NULL}, pipe = 'stdin') {
    AlgorithmHandler$new(
      applyMethod = applyfunc,
      onLoadMethod = onLoadMethod,
      pipeName = pipe
    )
  }
