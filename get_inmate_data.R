require(rvest)
require(dplyr)
require(parallel)
require(purrr)
require(magrittr)
require(lubridate)

devtools::load_all(".")

release_new_data <- function() {

    # get all counties
    counties <- get_all_counties()

    # get all inmates and their counties
    all_inmates <-
        mclapply(
            counties,
            get_county_inmates,
            mc.cores = detectCores()
        ) %>%
        bind_rows

    # get all inmate data
    floridainmates <-
        mclapply(
            all_inmates[["dc_number"]],
            get_inmate_data %>% safely,
            mc.cores = detectCores()
        ) %>%
        discard_errors %>%
        bind_rows %>%
        mutate(
            weight = readr::parse_number(weight),
            height_inches = calc_inches_height(height)
        ) %>%
        select(
            -special_note,
            -height
        )

    # save data
    saveRDS(floridainmates, "floridainmates.rds", compress = "bzip2")

    # bump version
    read.dcf("DESCRIPTION") %>%
        as_data_frame %>%
        mutate(Version = format(as.numeric(Version) + 1, nsmall = 1)) %>%
        write.dcf("DESCRIPTION")

    # commit changes to DESCRIPTION
    system("git add DESCRIPTION && git commit -m 'bump version' && git push")

    # release new version of data on GH
    floridainmates::mydata_release("refresh data", filename = "floridainmates.rds", yes = TRUE)
}

system.time(release_new_data())
