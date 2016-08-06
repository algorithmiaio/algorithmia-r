library("methods")

# TODO(james): you should really unit test this since, you know...
# best practices and all that jazz. But maybe Allison won't find out
# There are some indirect tests in DataDirectoryTest
AlgorithmiaAcl <- setRefClass("AlgorithmiaAcl",
    fields = list(read_acl = "character"),
    methods = list(
        getApiQueryList = function() {
            if (read_acl == "PRIVATE") {
                list(read=list())
            } else {
                list(read=list(read_acl))
            }
        }
    )
)

getAcl <- function(response) {
    if ("read" %in% names(response)) {
        if (length(response$read) == 0) {
            AlgorithmiaAcl$new(read_acl="PRIVATE")
        } else if (length(response$read) == 1) {
            if (response$read[1] == "user://*") {
                # PUBLIC
                AlgorithmiaAcl$new(read_acl="user://*")
            } else if (response$read[1] == "algo://.my/*") {
                # MY ALGOS
                AlgorithmiaAcl$new(read_acl="algo://.my/*")
            } else {
                stop(paste0("Invalid read ACL string: ", response$read))
            }
        } else {
            stop(paste0("Invalid read ACL: ", response$read))
        }
    } else {
        stop("Response does not contain read ACL")
    }
}

ReadAcl.PUBLIC <- getAcl(list(read=list("user://*")))
ReadAcl.PRIVATE <- getAcl(list(read=list()))
ReadAcl.MY_ALGORITHMS <- getAcl(list(read=list("algo://.my/*")))
