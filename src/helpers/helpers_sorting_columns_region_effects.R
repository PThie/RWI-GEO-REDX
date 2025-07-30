helpers_sorting_columns_region_effects <- function(
    region_effects_data = NA,
    housing_type = NA,
    regional_col = NA,
    time_col = c("year", "quarter"),
    grids = TRUE
) {
    #' @title Sort columns of region effects data frame
    #' 
    #' @description This function sorts the columns of the region effects data
    #' frame such that the year information is grouped together. It also adds
    #' the housing type to the data frame.
    #' 
    #' @param region_effects_data Data frame with region effects
    #' @param housing_type Housing type
    #' @param regional_col Column names for regional identifier
    #' 
    #' @return Data frame with sorted columns and housing type
    #' @author Patrick Thiel

    #--------------------------------------------------
    # extract column names and get rid of the grid column

    nam <- names(region_effects_data)
    nam <- nam[!grepl(regional_col, nam)]

    # extract time periods
    year_sequence <- seq(
        config_globals()[["first_year"]],
        config_globals()[["max_year"]]
    )

    if (time_col == "year") {
        time_sequence <- year_sequence
    } else {
        time_sequence <- c()
        for (year in year_sequence) {
            for (quarter in seq(1, config_globals()[["max_quarter"]])) {
                time_sequence <- c(
                    time_sequence,
                    paste0(year, "-0", quarter)
                )
            }
        }
    }

    # sort time periods
    sorted_vec <- c(regional_col)
    for (time_period in time_sequence) {
        sorted_vec <- c(
            sorted_vec,
            nam[grepl(time_period, nam)]
        )
    }

    # sort column names of data frame
    region_effects_data <- region_effects_data |>
        dplyr::select(dplyr::all_of(sorted_vec))

    #--------------------------------------------------
    # add housing type
    # only for grids data since this is one combined export
    
    if (grids == TRUE) { 
        region_effects_data$housing_type <- housing_type

        # move a second position
        region_effects_data <- region_effects_data |>
            dplyr::relocate(housing_type, .after = regional_col)
    }
    #--------------------------------------------------
    # return

    return(region_effects_data)
}