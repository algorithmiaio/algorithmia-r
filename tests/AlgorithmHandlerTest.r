library("RUnit")
library("tools")
library(algorithmia)

beforeTest <- function(){
  system('touch /tmp/algoout')
  p_out <- fifo('/tmp/algoout', 'r')
}

afterTest <- function(){
  system('rm /tmp/algoout')
}

test.runHelloWorld <- function(){
  algorithm <- function(input) {
    paste("hello", input)
  }
  handler <- algorithmia$getAlgorithmHandler(algorithm)
  handler$run()
}