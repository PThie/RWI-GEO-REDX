calculating_deviations_cross <- function(
    aggregated_effects = NA
) {
    #' @title Calculating deviations between regions (cross-section)
    #' 
    #' @description This function calculates deviations between regions
    #' (cross-section) for the absolute and percent price index values. The
    #' deviations are calculated relative to the overall mean of the price index
    #' values for each time period.
    #' 
    #' @param grid_effects_abs List of data frames with absolute estimated region.
    #' 
    #' @return List of data frames with deviations across region for each time
    #' period.
    #' @author Patrick Thiel

    #--------------------------------------------------
    results_list <- list()
    for (region_time in names(aggregated_effects)) {
        #--------------------------------------------------
        # subset for the data at hand

        region_time_effects <- aggregated_effects[[region_time]]

        #--------------------------------------------------
        # set up for specific regional level and time definitions
        
        region_label <- unlist(stringr::str_split(region_time, "_"))[1]
        time_label <- unlist(stringr::str_split(region_time, "_"))[2]
        nobs_var <- helpers_regional_effects_settings(agg_level = region_label)[["nobs_var"]]
        region_id <- helpers_regional_effects_settings(agg_level = region_label)[["region_id"]]

        #--------------------------------------------------
        # calculate mean across all regions in the period

        period_means <- region_time_effects |>
            # NOTE: weighted mean cannot be calculated if there are NAs in the
            # NOBS variable
            dplyr::filter(
                !is.na(.data[[nobs_var]])
            ) |>
            dplyr::group_by(!!rlang::sym(time_label)) |>
            dplyr::summarise(
                mean_pindex = stats::weighted.mean(
                    weighted_pindex,
                    w = .data[[nobs_var]],
                    na.rm = TRUE
                )
            )
        
        #--------------------------------------------------
        # add reference mean to the data

        region_time_effects_ref <- region_time_effects |>
            merge(
                period_means,
                by = time_label
            )

        #--------------------------------------------------
        # calculate deviations across region

        region_time_effects_ref <- region_time_effects_ref |>
            dplyr::mutate(
                pindex_dev = weighted_pindex - mean_pindex,
                pindex_dev_perc = (pindex_dev / mean_pindex) * 100
            )

        #--------------------------------------------------
        # store results

        results_list[[region_time]] <- region_time_effects_ref
    }

    #--------------------------------------------------
    # return

    return(results_list)
}