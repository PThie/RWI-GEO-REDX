calculating_deviations_regions <- function(
    aggregated_effects = NA,
    destatis = FALSE,
    housing_type = NA
) {
    #' @title Calculate deviations for regions
    #' 
    #' @description This function calculates the deviations of the regional effects
    #' from a base period. The base period is defined by the settings in the
    #' `helpers_regional_effects_change_settings` function.
    #' 
    #' @param aggregated_effects List of data frames with the regional effects
    #' @param destatis Logical indicating whether to use Destatis references.
    #' @param housing_type Character string specifying the housing type.
    #' 
    #' @return List of data frames with the deviations for each region and time period
    #' @author Patrick Thiel

    #--------------------------------------------------

    results_list <- list()
    for (result in names(aggregated_effects)) {
        #--------------------------------------------------
        # subset for data at hand

        region_time_effects <- aggregated_effects[[result]]

        #--------------------------------------------------
        # set up for specific regional level and time definitions
        
        region_label <- unlist(stringr::str_split(result, "_"))[1]
        time_label <- unlist(stringr::str_split(result, "_"))[2]

        region_id <- helpers_regional_effects_settings(agg_level = region_label)[["region_id"]]

        reference_period <- helpers_regional_effects_change_settings(
            time_period = time_label,
            destatis = destatis,
            housing_type = housing_type
        )[["reference_period"]]

        #--------------------------------------------------
        # merge base period to overall data

        base_period <- region_time_effects |>
            dplyr::filter(!!rlang::sym(time_label) == reference_period) |>
            dplyr::rename(
                pindex_ref = weighted_pindex
            ) |>
            dplyr::select(dplyr::all_of(c(region_id, "pindex_ref")))

        region_time_effects_prep <- merge(
            region_time_effects,
            base_period,
            by = region_id,
            all.x = TRUE
        )

        #--------------------------------------------------
        # calculate deviations within region from base time

        region_time_effects_prep <- region_time_effects_prep |>
            dplyr::mutate(
                pindex_dev = weighted_pindex - pindex_ref,
                pindex_dev_perc = ((weighted_pindex - pindex_ref) / pindex_ref) * 100
            )

        #--------------------------------------------------
        # store results

        results_list[[result]] <- region_time_effects_prep
    }

    #--------------------------------------------------
    # export (only for destatis)

    if (destatis == TRUE) {
        for (result in names(results_list)) {
            data.table::fwrite(
                results_list[[result]],
                file.path(
                    config_paths()[["output_path"]],
                    "destatis",
                    paste0("deviations_", result, ".csv")
                )
            )
        }
    }
    
    #--------------------------------------------------
    # return

    return(results_list)
}