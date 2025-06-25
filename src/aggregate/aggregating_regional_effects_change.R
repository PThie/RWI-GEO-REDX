aggregating_regional_effects_change <- function(
    estimated_effects_list = NA,
    housing_type = NA
) {
    #' @title Aggregating regional effects
    #' 
    #' @description This function aggregates regional effects for a given housing
    #' type to a larger regional level (municipality, districts, LMR).
    #' 
    #' @param estimated_effects_list List with estimated regional effects
    #' @param housing_type Housing type
    #' 
    #' @note Based on regression 3 in the former Stata coding.
    #' 
    #' @return List with aggregated regional effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # loop through time levels on grid level (previously estimated effects)

    results_list <- list()
    for (result in names(estimated_effects_list)) {
        for (agg_level in c("munic", "district", "lmr")) {
            #--------------------------------------------------
            # set up for specific regional level
            # and time definitions
            
            nobs_var <- helpers_regional_effects_settings(agg_level = agg_level)[["nobs_var"]]
            region_id <- helpers_regional_effects_settings(agg_level = agg_level)[["region_id"]]
            region_label <- helpers_regional_effects_settings(agg_level = agg_level)[["region_label"]]
            time_label <- helpers_regional_effects_change_settings(time_period = result)[["time_label"]]

            #--------------------------------------------------
            # calculate weights
            # NOTE: weight follows the definition of:
            # NOBS_ejahr_time_grid / SUM(NOBS_ejahr_time_region)
            # for each type separately
            # and region depends on the aggregation level (municipality or district)
            # and time depends on the time level (year or quarter) (this is given
            # by the result variable, i.e. the data set itself)

            estimated_effects <- estimated_effects_list[[result]] |>
                dplyr::mutate(
                    weight = nobs_grid / .data[[nobs_var]],
                    weighted_pindex = pindex * weight
                )

            #--------------------------------------------------
            # aggregate to higher level

            estimated_effects_agg <- estimated_effects |>
                dplyr::group_by(
                    !!rlang::sym(time_label),
                    !!rlang::sym(region_id)
                ) |>
                dplyr::summarise(
                    weighted_pindex = sum(weighted_pindex, na.rm = TRUE),
                    !!nobs_var := dplyr::first(.data[[nobs_var]])
                ) |>
                dplyr::ungroup()
            
            #--------------------------------------------------
            # export

            openxlsx::write.xlsx(
                estimated_effects_agg,
                file.path(
                    config_paths()[["output_path"]],
                    housing_type,
                    "estimates",
                    paste0(
                        "regional_effects_",
                        region_label,
                        "_",
                        time_label,
                        "_absolute.xlsx"
                    )
                )
            )

            #--------------------------------------------------
            # store results

            results_list[[paste0(region_label, "_", time_label)]] <- estimated_effects_agg
        }
    }

    #--------------------------------------------------
    # return

    return(results_list)
}