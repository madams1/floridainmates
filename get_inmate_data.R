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
            height = calc_inches_height(height),
            birth_date = mdy(birth_date),
            initial_receipt_date = mdy(initial_receipt_date),
            current_release_notes = ifelse(
                stringr::str_detect(current_release_date, "^[:alpha:]"),
                current_release_date,
                NA
            ),
            current_release_date = mdy(current_release_date)
        ) %>%
        rename(
            weight_lbs = weight,
            height_inches = height
        ) %>%
        left_join(all_inmates) %>%
        select(
            dc_number,
            county_of_commitment,
            name:current_release_date,
            current_release_notes,
            everything(),
            -special_note
        )

    # save data
    saveRDS(floridainmates, "floridainmates.rds", compress = "bzip2")

    # bump version to new minor version
    bump_version() # defined in R/helper_functions.R

    # commit changes to DESCRIPTION
    system("git add DESCRIPTION && git commit -m 'bump version' && git push")

    # release new version of data on GH
    floridainmates::floridainmates_release("refresh data", filename = "floridainmates.rds", yes = TRUE)
}

system.time(release_new_data())
