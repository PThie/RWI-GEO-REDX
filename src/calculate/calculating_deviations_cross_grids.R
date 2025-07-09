calculating_deviations_cross_grids <- function(
    grid_effects_abs = NA
) {
    #' @title Calculating deviations between grids (cross-section)
    #' 
    #' @description This function calculates deviations between grids
    #' (cross-section) for the absolute and percent price index values. The
    #' deviations are calculated relative to the overall mean of the price index
    #' values for each time period.
    #' 
    #' @param grid_effects_abs List of data frames with absolute estimated region.
    #' 
    #' @return List of data frames with deviations across grids for each time
    #' period.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # calculate deviations between grids (relative to overall mean)

    results_list <- list()
    for (time_label in names(grid_effects_abs)) {
        #--------------------------------------------------
        # get data

        period_data <- grid_effects_abs[[time_label]]
        
        #--------------------------------------------------
        # calculate mean across all grids in the period

        period_means <- period_data |>
            dplyr::group_by(!!rlang::sym(time_label)) |>
            dplyr::summarise(
                mean_pindex = mean(pindex, na.rm = TRUE)
            )

        #--------------------------------------------------
        # add reference mean to the data

        period_data_ref <- period_data |>
            merge(
                period_means,
                by = time_label
            )

        #--------------------------------------------------
        # calculate deviations across region

        period_data_ref <- period_data_ref |>
            dplyr::mutate(
                pindex_dev = pindex - mean_pindex,
                pindex_dev_perc = (pindex_dev / mean_pindex) * 100
            )
        
        #--------------------------------------------------
        # store results

        results_list[[time_label]] <- period_data_ref
    }

    #--------------------------------------------------
    # return

    return(results_list)
}