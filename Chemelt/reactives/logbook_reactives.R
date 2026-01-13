write_logbook <- function(record_str,include_time=FALSE) {

    if (include_time) {
        record_str <- paste0(as.character(format(Sys.time(),usetz = TRUE)),' ',record_str)
    }
    # Append '' to print in the output a new empty line after the record
    record_str <- c(record_str,'')

    reactives$logbook <- append(reactives$logbook, record_str)

    return(NULL)
}