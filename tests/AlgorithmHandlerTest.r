library("RUnit")
library("tools")

beforeTest <- function(){
    system('touch /tmp/algoout')
}

afterTest <- function(){
    system('rm /tmp/algoout')
}


test.runHelloWorld <- function(){
  algorithm <- function(input) {
    paste("hello", input)
  }
  beforeTest()
  con <- "input/hello_world.json"
  handler <- getAlgorithmHandler(algorithm, pipe=con)
  handler$run()
  p_out <- fifo('/tmp/algoout', 'r')
  result <- readLines(p_out)
  expected <- "{\"result\":\"hello james\",\"metadata\":{\"content_type\":\"text\"}}"
  afterTest()
  checkEquals(result, expected)
}
