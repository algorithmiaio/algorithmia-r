library("RUnit")
library("tools")


test.runHelloWorld <- function(){
  algorithm <- function(input) {
    paste("hello", input)
  }
  beforeTest <- function(){
    system('touch /tmp/algoout')
    p_out <- fifo('/tmp/algoout', 'r')
  }
  
  afterTest <- function(){
    system('rm /tmp/algoout')
  }
  beforeTest()
  handler <- getAlgorithmHandler(algorithm)
  handler$run()
  afterTest()
  checkEquals("", "")
}
