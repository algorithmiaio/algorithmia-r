#' Internal class used to describe ACLs.
AlgorithmiaAcl <- methods::setRefClass("AlgorithmiaAcl",
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

#' The ACL that allows anyone to read an item.
#' Sample usage:
#' dataDirectory$create(ReadAcl.PUBLIC)
#' dataDirectory$updatePermissions(ReadAcl.PUBLIC)
ReadAcl.PUBLIC <- getAcl(list(read=list("user://*")))

#' The ACL that allows only you to read an item.
#' Sample usage:
#' dataDirectory$create(ReadAcl.PRIVATE)
#' dataDirectory$updatePermissions(ReadAcl.PRIVATE)
ReadAcl.PRIVATE <- getAcl(list(read=list()))

#' The ACL that allows your algorithms to read an item.
#' Sample usage:
#' dataDirectory$create(ReadAcl.MY_ALGORITHMS)
#' dataDirectory$updatePermissions(ReadAcl.MY_ALGORITHMS)
ReadAcl.MY_ALGORITHMS <- getAcl(list(read=list("algo://.my/*")))
