### functions for getting inmate data

get_all_counties <- function() {
    inmate_search_url <- httr::modify_url(
        url = inmate_url,
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


get_county_inmates <- function(county) {
    county_inmates_url <- httr::modify_url(
        url = inmate_url,
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


get_inmate_page <- function(id) {
    inmate_details_url <- httr::modify_url(
        url = inmate_url,
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
            scars_marks_tattoos = get_table("Scars, Marks")
        )

    inmate_data
}
