library("RUnit")
library("tools")
library("rjson")

beforeTest <- function() {
  system('touch /tmp/algoout')
}

afterTest <- function() {
  system('rm /tmp/algoout')
}

readPipe <- function() {
  p_out <- fifo('/tmp/algoout', 'r')
  result <- readLines(p_out)
  close(p_out)
  print(result)
  rjson::fromJSON(result)
}

test.runHelloWorld <- function() {
  expected <-
    list(result = "hello james",
         metadata = list(content_type = "text"))
  con <- "input/hello_world.json"
  algorithm <- function(input) {
    paste("hello", input)
  }
  beforeTest()
  handler <- getAlgorithmHandler(algorithm, pipe = con)
  handler$serve()
  result <- readPipe()
  afterTest()
  checkEquals(result, expected)
}

test.runHelloWithJson <- function() {
  expected <-
    list(result = "hello james",
         metadata = list(content_type = "text"))
  con = "input/hello_json.json"
  algorithm <- function(input) {
    paste("hello", input$name)
  }
  beforeTest()
  handler <- getAlgorithmHandler(algorithm, pipe = con)
  handler$serve()
  result <- readPipe()
  afterTest()
  checkEquals(result, expected)
}


test.runWithContext <- function() {
  expected <-
    list(result = "hello james here is your file /tmp/example",
         metadata = list(content_type = "text"))
  con = "input/hello_world.json"
  algorithm <- function(input, context) {
    if (is.null(context)) {
      stop("Context was not defined")
    }
    else {
      paste("hello", input, "here is your file", context$example)
    }
  }
  loader <- function() {
    context <- list()
    context$example = "/tmp/example"
    context
  }
  beforeTest()
  handler <- getAlgorithmHandler(algorithm, loader, pipe = con)
  handler$serve()
  result <- readPipe()
  afterTest()
  checkEquals(result, expected)
}

#TODO: figure out if we want to improve this error message (do we need the applyMethod(inputData) wrapper?)
test.algorithmThrowsException <- function() {
  expected <-
    list(
      error = list(
        message = "Error in applyMethod(inputData): a runtime exception was thrown",
        stacktrace = "algorithm",
        error_type = "AlgorithmError"
      )
    )
  con = "input/hello_world.json"
  algorithm <- function(input) {
    stop("a runtime exception was thrown")
  }
  beforeTest()
  handler <- getAlgorithmHandler(algorithm, pipe = con)
  handler$serve()
  result <- readPipe()
  afterTest()
  checkEquals(result, expected)
}

#TODO: figure out if we want to improve this error message
test.arityProblemWithContextThrowsException <- function() {
  expected <-
    list(
      error = list(
        message = "Error in applyMethod(inputData, state): unused argument (state)",
        stacktrace = "algorithm",
        error_type = "AlgorithmError"
      )
    )
  con = "input/hello_world.json"
  algorithm <- function(input) {
    paste("hello", input)
  }
  loader <- function() {
    state <- list(foo = "bar")
  }
  beforeTest()
  handler <- getAlgorithmHandler(algorithm, loader, pipe = con)
  handler$serve()
  result <- readPipe()
  afterTest()
  checkEquals(result, expected)
}

test.loaderThrowsException <- function() {
  expected <-
    list(
      error = list(
        message = "Error in onLoadMethod(): a load time exception was thrown",
        stacktrace = "loading",
        error_type = "AlgorithmError"
      )
    )
  
  con = "input/hello_world.json"
  algorithm <- function(input) {
    paste("hello", input)
  }
  loader <- function() {
    stop("a load time exception was thrown")
  }
  beforeTest()
  handler <- getAlgorithmHandler(algorithm, loader, pipe = con)
  handler$serve()
  result <- readPipe()
  afterTest()
  checkEquals(result, expected)
}
