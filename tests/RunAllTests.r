#install.packages("RUnit")
library("RUnit")

runTestFile("ClientTest.r")
runTestFile("AlgorithmTest.r")
runTestFile("DataFileTest.r")
runTestFile("DataDirectoryTest.r")
