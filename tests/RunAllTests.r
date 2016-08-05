#install.packages("RUnit")
library(devtools)
install("..")
library("algorithmia")

library("RUnit")

runTestFile("ClientTest.r")
runTestFile("AlgorithmTest.r")
runTestFile("DataFileTest.r")
runTestFile("DataDirectoryTest.r")
