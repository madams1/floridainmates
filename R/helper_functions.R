
# helper functions for getting inmate data ----------------------------------------------------

# for formatting column names
snake_case <- function(x, sanitize = TRUE) {

    out_str <- tolower(x) %>%
        stringr::str_trim("both") %>%
        stringr::str_replace_all(" {1,}|-", "_")

    if (sanitize) {
        out_str %<>% stringr::str_replace_all("(?!_)[:punct:]", "")
    }

    out_str
}

# for getting numbers for heights
calc_inches_height <- function(x) {

    map_dbl(x, function(w) {
        parsed_hgt <- stringr::str_split(w, pattern = "'", n = 2) %>%
            unlist %>%
            readr::parse_number()
        parsed_hgt[1] * 12 + parsed_hgt[2]
    })

}

# for discarding errors and keeping results in a chain
discard_errors <- function(x) {
    discard(x, ~ !is_null(.x[["error"]])) %>%
        map("result")
}

# for bumping the DESCRIPTION's version
bump_version <- function(type = c("minor", "patch", "major")) {
    type <- match.arg(type)

    des_df <- as.data.frame(read.dcf("DESCRIPTION"), stringsAsFactors = FALSE)

    v_split <- as.integer(unlist(strsplit(des_df$Version, "\\.")))
    names(v_split) <- c("major", "minor", "patch")
    v_split[[type]] <- v_split[[type]] + 1
    if (type != "patch") {
        v_split[["patch"]] <- 0
    }
    if (type == "major") {
        v_split[["minor"]] <- 0
    }
    new_version <- paste(v_split, collapse = ".")

    des_df$Version <- new_version
    write.dcf(des_df, "DESCRIPTION")
}
