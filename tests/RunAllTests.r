library(RUnit)

pkgname <- "algorithmia"
require(pkgname, quietly=TRUE, character.only=TRUE) || stop("package '", pkgname, "' not found")

message(file.path(path.package(package=pkgname), "tests"))

# Define RUnit test suite
suite <- defineTestSuite(name=paste(pkgname, "RUnit Tests"),
                         dirs = file.path(path.package(package=pkgname), "tests"),
                         testFileRegexp=".*Test\\.r$",
                         testFuncRegexp = "^test.+",
                         rngKind="default",
                         rngNormalKind="default")

# Run tests
result <- runTestSuite(suite)

# Fail build if any errors or failures were encountered
if (result[[1]]$nErr > 0 || result[[1]]$nFail > 0) {
  testFileNames <- names(result[[1]]$sourceFileResults)
  for (testFileName in testFileNames) {
    testFileResult <- result[[1]]$sourceFileResults[[testFileName]]
    testNames <- names(testFileResult)
    for (testName in testNames) {
      result <- testFileResult[[testName]]
	  if (!is.null(result$msg)) {
	    message(paste(testFileName, testName, result$msg))
	    if (!is.null(result$traceBack)) {
	      message(result$traceBack)
	    }
	  }
	}
  }
  stop("Errors were found")
}
