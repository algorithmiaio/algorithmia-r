library(RUnit)

pkgname <- "algorithmia"
require(pkgname, quietly=TRUE, character.only=TRUE) || stop("package '", pkgname, "' not found")

message(file.path(path.package(package=pkgname), "../tests"))

# Define RUnit test suite
suite <- defineTestSuite(name=paste(pkgname, "RUnit Tests"),
                         dirs = file.path(path.package(package=pkgname), "../tests"),
                         testFileRegexp=".*Test\\.r$",
                         testFuncRegexp = "^test.+",
                         rngKind="default",
                         rngNormalKind="default")

# Run tests
result <- runTestSuite(suite)

# Display result tests on the console
printTextProtocol(result)
