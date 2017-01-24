require(rvest)
require(dplyr)
require(parallel)
require(purrr)
require(magrittr)
require(lubridate)

system.time({

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

    get_all_counties <- function() {
        inmate_search_url <- httr::modify_url(
            url = "http://www.dc.state.fl.us",
            path = "OffenderSearch/search.aspx",
            query = list(
                TypeSearch = "AI"
            )
        )

        inmate_search_url %>%
            read_html %>%
            html_nodes("#ctl00_ContentPlaceHolder1_lstCountyOfCommitment") %>%
            html_nodes("option") %>%
            html_text %>%
            discard(~ .x == "ALL")

    }

    # ! data
    counties <- get_all_counties()

    get_county_inmates <- function(county) {
        county_inmates_url <- httr::modify_url(
            url = "http://www.dc.state.fl.us",
            path = "OffenderSearch/list.aspx",
            query = list(
                TypeSearch = "AI",
                Page = "List",
                DataAction = "Filter",
                dcnumber = "",
                LastName = "",
                FirstName = "",
                SearchAliases = 1,
                OffenseCategory = "All",
                CurrentLocation = "",
                CountyOfCommitment = county,
                photosonly = 0,
                nophotos = 1,
                matches = 50000 # arbitrarily big to get all results
            )
        )

        inmates <- county_inmates_url %>%
            read_html %>%
            html_nodes("#ctl00_ContentPlaceHolder1_grdList") %>%
            html_table %>%
            extract2(1) %>%
            set_names(., snake_case(names(.))) %>%
            extract2("dc_number")

        data_frame(
            county_of_commitment = stringr::str_to_title(county),
            dc_number = inmates
        )
    }

    # ! data
    all_inmates <-
        mclapply(
            counties,
            get_county_inmates,
            mc.cores = detectCores()
        ) %>%
        bind_rows


    get_inmate_page <- function(id) {
        inmate_details_url <- httr::modify_url(
            url = "http://www.dc.state.fl.us",
            path = "offenderSearch/detail.aspx",
            query = list(
                Page = "Detail",
                DCNumber = id,
                TypeSearch = "AI"
            )
        )

        read_html(inmate_details_url)
    }

    get_inmate_table_data <- function(page, str_lookup, table_type_selector = "table.dcCSStableAlias") {

        table_xpath <- paste0(
            "//table//*[contains(., '",
            str_lookup,
            "')]//.."
        )

        ref <- page %>%
            html_nodes(table_type_selector) %>%
            html_nodes(xpath = table_xpath)

        output <- if (length(ref) > 0) {
            ref %>%
                extract(1) %>%
                html_table %>%
                extract2(1) %>%
                set_names(., snake_case(map(., 1))) %>%
                filter(row_number() > 1) %>%
                map_df(stringr::str_to_title) %>%
                mutate_at(vars(dplyr::contains("date")), mdy)
        } else {
            NULL
        }

        # return output as list
        list(output)

    }


    get_inmate_data <- function(dc_num) {
        inmate_page <- get_inmate_page(dc_num)

        # partial function specific to this inmate's page
        get_table <- partial(get_inmate_table_data, page = inmate_page)

        # collect/combine data for this inmate
        inmate_data <- inmate_page %>%
            html_nodes("#ctl00_ContentPlaceHolder1_tblProfile") %>%
            html_nodes("table") %>%
            extract(2) %>%
            html_nodes("tr") %>%
            html_text %>%
            strsplit(":") %>%
            map(stringr::str_trim, "both") %>%
            discard(~ length(.x) < 2) %>%
            set_names(., snake_case(map(., 1))) %>%
            map_df(~ stringr::str_to_title(.x[2])) %>%
            # list-cols for inmate-specific subdata
            mutate(
                incarceration_history = get_table("Incarceration"),
                sentence_history = get_table("Sentence History"),
                prison_history = get_table("Prior Prison"),
                scars_marks_tatoos = get_table("Scars, Marks")
            )

        inmate_data
    }

    # ! data

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

})

### save data and upload to GH as release

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

# process to get data:
# devtools::install_github("madams1/floridainmates")
# devtools::install_github("richfitz/datastorr")
# my_data <- floridainmates::mydata()
