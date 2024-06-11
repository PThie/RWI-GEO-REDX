helpers_sorting_columns_region_effects <- function(
    region_effects_data = NA,
    housing_type = NA
) {
    #' @title Sort columns of region effects data frame
    #' 
    #' @description This function sorts the columns of the region effects data
    #' frame such that the year information is grouped together. It also adds
    #' the housing type to the data frame.
    #' 
    #' @param region_effects_data Data frame with region effects
    #' @param housing_type Housing type
    #' 
    #' @return Data frame with sorted columns and housing type
    #' @author Patrick Thiel

    #--------------------------------------------------
    # extract column names and get rid of the grid column
    
    nam <- names(region_effects_data)
    nam <- nam[!grepl("grid", nam)]

    # extract years and sort them
    sorted_vec <- c("grid")
    for (year in seq(
        config_globals()[["first_year"]],
        config_globals()[["max_year"]])
    ) {
        sorted_vec <- c(
            sorted_vec,
            nam[grepl(year, nam)]
        )
    }

    # sort column names of data frame
    region_effects_data <- region_effects_data |>
        dplyr::select(dplyr::all_of(sorted_vec))

    #--------------------------------------------------
    # add housing type

    region_effects_data$housing_type <- housing_type

    # move a second position
    region_effects_data <- region_effects_data |>
        dplyr::relocate(housing_type, .after = "grid")

    #--------------------------------------------------
    # return

    return(region_effects_data)
}