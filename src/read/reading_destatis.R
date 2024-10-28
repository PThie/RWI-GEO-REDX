reading_destatis <- function(data_path_destatis = NA) {
    #' @title Read and clean Destatis data
    #' 
    #' @description This function reads and cleans the Destatis data for comparison
    #' with the RWI data.
    #' 
    #' @param data_path_destatis Character, path to the Destatis data
    #' 
    #' @return List, cleaned Destatis data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # get all files

    all_files <- list.files(
        data_path_destatis,
        pattern = "*.csv"
    )

    #--------------------------------------------------
    # cleaning function

    cleaning_destatis_data <- function(
        data = NA,
        period = c("year", "quarter"),
        file_name = NA
    ) {
        # define where the column names are stored
        if (period == "quarter") {
            slicing_rows <- 1:2
        } else {
            slicing_rows <- 1
        }

        # extract column names
        col_names <- data |>
            dplyr::slice(slicing_rows)

        # clean column names
        col_names <- paste0(col_names)
        if (!grepl("net", file_name)) {
            col_names[1] <- "data_type"
            col_names[2] <- "DELETE"
            col_names <- stringr::str_replace(col_names, 'c\\(\\"', '')
            col_names <- stringr::str_replace(col_names, '\", \"', '-0')
            col_names <- stringr::str_replace(col_names, '\\. Quartal\\"\\)', '')        
        } else {
            col_names[1] <- "year"
        }

        # assign clean column names
        colnames(data) <- col_names

        if (!grepl("net", file_name)) {
            # replace umlaute
            # remove DELETE column
            data_clean <- data |>
                dplyr::mutate(
                    data_type = stringi::stri_trans_general(
                        data_type,
                        "de-ASCII; Latin-ASCII"
                    )
                ) |>
                dplyr::select(-DELETE)
        } else {
            data_clean <- data
        }

        # remove unnessary information
        # former col name rows
        data_clean <- data_clean |>
            dplyr::filter(!dplyr::row_number() %in% slicing_rows)

        # replace comma with dot
        data_clean <- data_clean |>
            dplyr::mutate(
                dplyr::across(
                    -1,
                    ~ stringr::str_replace_all(.x, ",", ".")
                ),
                dplyr::across(
                    -1,
                    ~ as.numeric(.x)
                )
            )

        # redefine data_type column
        if (grepl("house", file_name)) {
            data_clean <- data_clean |>
                dplyr::mutate(
                    data_type = dplyr::case_when(
                        data_type == "Haeuserpreisindex" ~ "house_price_index",
                        data_type == "Neu erstellte Wohnimmobilien" ~ "price_index_new_buildings",
                        data_type == "Bestehende Wohnimmobilien" ~ "price_index_existing_buildings",
                        data_type == "Preisindex fuer Bauland" ~ "price_index_building_land"
                    )
                )
        } else if (grepl("apartment", file_name) & !grepl("net", file_name)) {
            data_clean <- data_clean |>
                dplyr::filter(
                    grepl("Preisindex", data_type)
                ) |>
                dplyr::mutate(
                    data_type = "price_index_owner_occupied_building"
                )
        }

        # transform to long format
        if (!grepl("net", file_name)) {
            data_clean_long <- data_clean |>
                tidyr::pivot_longer(
                    cols = -data_type,
                    names_to = period,
                    values_to = "timeeff"
                ) |>
                # drop NAs in value since these are not yet available quarters
                dplyr::filter(!is.na(value))
        } else {
            data_clean_long <- data_clean |>
                tidyr::pivot_longer(
                    cols = -year,
                    names_to = "state",
                    values_to = "timeeff"
                ) |>
                # aggregate to yearly (disregard states as our index is also
                # at the national level for time effects)
                dplyr::group_by(year) |>
                dplyr::summarise(
                    timeeff = mean(timeeff, na.rm = TRUE)
                ) |>
                dplyr::filter(!is.na(timeeff))
        }

        # return
        return(data_clean_long)
    }

    #--------------------------------------------------
    # read and clean files

    data_storage <- list()
    for (file in all_files) {
        # extract file name
        file_name <- stringr::str_replace(file, ".csv", "")

        if (
            file_name == "house_price_index_quarters" |
            file_name == "net_rents_index_states_years"
        ) {
            # read file
            dta <- data.table::fread(
                file.path(
                    data_path_destatis,
                    file
                ),
                skip = 4,
                fill = TRUE
            )
        } else {
            # read file
            dta <- data.table::fread(
                file.path(
                    data_path_destatis,
                    file
                ),
                skip = 5,
                fill = TRUE
            )
        }

        # cleaning
        if (grepl("quarters", file_name)) {
            dta_cleaned <- cleaning_destatis_data(
                data = dta,
                period = "quarter",
                file_name = file_name
            )
        } else {
            dta_cleaned <- cleaning_destatis_data(
                data = dta,
                period = "year",
                file_name = file_name
            )
        }

        # export
        data_storage[[file_name]] <- dta_cleaned
    }

    #--------------------------------------------------
    # return

    return(data_storage)
}