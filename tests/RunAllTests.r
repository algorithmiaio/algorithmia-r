install.packages("RUnit")
install.packages("devtools")
library(devtools)
install("..")
library("algorithmia")

library("RUnit")

# commenting out other test files until we're happy with AlgorithmHandlerTest.r

#runTestFile("ClientTest.r")
#runTestFile("AlgorithmTest.r")
#runTestFile("DataFileTest.r")
#runTestFile("DataDirectoryTest.r")
runTestFile("AlgorithmHandlerTest.r")


