# Uncomment these lines to test local (unpublished) changes to the client
#install.packages("RUnit")
#install.packages("devtools")
#library(devtools)
#install("..")
library("algorithmia")

library("RUnit")

runTestFile("ClientTest.r")
runTestFile("AlgorithmTest.r")
runTestFile("DataFileTest.r")
runTestFile("DataDirectoryTest.r")
runTestFile("AlgorithmHandlerTest.r")


