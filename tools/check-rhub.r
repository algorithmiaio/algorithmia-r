chooseCRANmirror(graphics=FALSE, ind=1)

message("Running rhubcheck...")

cr <- rhub::check_for_cran(
  show_status = TRUE
)

statuses <- cr[[".__enclos_env__"]][["private"]][["status_"]]

# capture errors, warnings, and notes in data frame
res <- do.call(rbind, lapply(statuses, function(thisStatus) {
  data.frame(
    plaform  = thisStatus[["platform"]][["name"]],
    errors   = length(thisStatus[["result"]][["errors"]]),
    warnings = length(thisStatus[["result"]][["warnings"]]),
    notes    = length(thisStatus[["result"]][["notes"]]),
    stringsAsFactors = FALSE
  )
}))

message("\n\nResults")
print(res)

message("\n\nGrabbing Rcheck logs")
logCandidates <- do.call(rbind, lapply(
  cr$urls()$artifacts,
  function(thisUrl) {
    logFile <- tools::file_path_sans_ext(dir("./logs/"))
    data.frame(
      url = file.path(
        thisUrl,
        paste0(unique(sapply(statuses, `[[`, "package"))[[1L]], ".Rcheck"),
        c(paste0(logFile, "00check.log"), paste0(logFile, "00check.log"))
      ),
      fileName = paste(
        basename(thisUrl),
        c(paste0(logFile, "00check.log"), paste0(logFile, "00check.log")),
        sep = "-"
      ),
      stringsAsFactors = FALSE
    )
  }
))

message("Downloading Rcheck logs from rhub: \n")
logLogs <- apply(
  logCandidates,
  1L,
  function(x) {
    try(
      utils::download.file(x["url"], x["fileName"], quiet = TRUE),
      silent = TRUE
    )
  }
)
names(logLogs) <- logCandidates[["fileName"]]

message("Attempting do cat log logs from rhub: \n\n")
invisible(lapply(
  names(logLogs[logLogs == 0L]),
  function(x) {
    message("\n", x)
    cat(readLines(x), sep = "\n")
  }
))

# report error status if errors or warnings exist
if (any(colSums(res[2L:3L]) > 0)) {
  stop("Some checks with errors or warnings.")
}
