calculating_deviations_regions <- function(
    grid_effects_abs = NA
) {
    #' @title Calculating deviations within region
    #' 
    #' @description This function calculates the deviations within a region from a
    #' base time period. The deviations are calculated for the absolute
    #' estimated region effects and the percentage deviations.
    #' 
    #' @param grid_effects_abs List of data frames with absolute estimated region.
    #' 
    #' @return List of data frames with deviations within region for each time
    #' period.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # calculate deviations within region from base time

    results_list <- list()
    for (time_label in names(grid_effects_abs)) {
        #--------------------------------------------------
        # get data

        period_data <- grid_effects_abs[[time_label]]

        #--------------------------------------------------
        # set up for different time periods

        reference_period <- helpers_regional_effects_change_settings(time_period = time_label)[["reference_period"]]
    
        #--------------------------------------------------
        # add reference value to data

        period_data_ref <- period_data |>
            merge(
                period_data |>
                    dplyr::filter(!!rlang::sym(time_label) == reference_period) |>
                    dplyr::rename(pindex_ref = pindex) |>
                    dplyr::select(grid, pindex_ref),
                by = "grid",
                all.x = TRUE
            )

        #--------------------------------------------------
        # calculate deviations within region

        period_data_ref <- period_data_ref |>
            dplyr::mutate(
                pindex_dev = pindex - pindex_ref,
                pindex_dev_perc = ((pindex - pindex_ref) / pindex_ref) * 100
            )

        #--------------------------------------------------
        # store results

        results_list[[time_label]] <- period_data_ref
    }

    #--------------------------------------------------
    # return

    return(results_list)
}