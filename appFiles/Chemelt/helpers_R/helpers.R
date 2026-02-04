popUpWarning <- function(string) shinyalert(text = string,type = "warning",closeOnEsc = T,closeOnClickOutside = T,html=T)
popUpInfo    <- function(string) shinyalert(text = string,type = "info",   closeOnEsc = T,closeOnClickOutside = T,html=T)
popUpSuccess <- function(string) shinyalert(text = string,type = "success",closeOnEsc = T,closeOnClickOutside = T,html=T)


# Function to delete elements equal to their previous element
delete_duplicates <- function(vector) {
    keep <- c(TRUE, vector[-length(vector)] != vector[-1])

    return(vector[keep])
}

# For each ith line, delete them if the ith+2 line contains the same pattern
delete_duplicate_pattern <- function(lines,pattern) {

    id2keep <- c()

    for (i in 1:(length(lines)-2)) {

        c <- grepl(pattern,lines[c(i,i+2)])

        id2keep <- c(id2keep,!all(c))

    }

    lines <- lines[id2keep]

  return(lines)
}

find_all_blocks <- function(lines, pattern_start, pattern_end) {
    starts <- which(grepl(pattern_start, lines))
    ends   <- which(grepl(pattern_end, lines))

    blocks <- lapply(starts, function(s) {
        e <- ends[ends > s][1]
        if (!is.na(e)) (s + 1):(e - 1) else NULL
    })

    blocks_list <- Filter(Negate(is.null), blocks)

    return(blocks_list)
}

keep_only_last_match <- function(lines, pattern) {
    hits <- which(grepl(pattern, lines))

    if (length(hits) == 0) {
        return(lines)  # nothing to do
    }

    # Set all matches to empty except the last one
    lines[hits[-length(hits)]] <- ""

    return(lines)
}
# Delete unwanted lines
purge_logbook_lines <- function(lines) {

    # Remove non-informative entries
    # For example, if we have consecutive lines with the same text
    # we only leave the last line
    lines <- delete_duplicate_pattern(lines,'Number of residues set to:')

    blocks <- find_all_blocks(lines,"Files imported","Fitting process started")

    for (block in blocks) {

        lines[block] <- keep_only_last_match(lines[block],"Denaturant concentrations set to")
        lines[block] <- keep_only_last_match(lines[block],"Rescaling set to:")
        lines[block] <- keep_only_last_match(lines[block],"Selecting conditions:")
        lines[block] <- keep_only_last_match(lines[block],"Signal set to:")
        lines[block] <- keep_only_last_match(lines[block],"Temperature range set to:")

    }

    # remove duplicates entries, useful to remove extra white lines
    lines <- delete_duplicates(lines)

    return(lines)
}
