aggregating_regional_effects_change <- function(
    estimated_effects_list = NA,
    housing_type = NA
) {
    #' @title Aggregating regional effects (change)
    #' 
    #' @description This function aggregates regional effects for a given housing
    #' type to a larger regional level (municipality and districts). It also
    #' calculates the change between the reference period and the current period.
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
            reference_period <- helpers_regional_effects_change_settings(time_period = result)[["reference_period"]]

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
            # calculate the change between District_Year - District_2008

            estimated_effects_agg <- estimated_effects_agg |>
                merge(
                    estimated_effects_agg |>
                        dplyr::filter(!!rlang::sym(time_label) == reference_period) |>
                        dplyr::rename(weighted_pindex_ref = weighted_pindex) |>
                        dplyr::select(-c(time_label, nobs_var)),
                    by = region_id,
                    all.x = TRUE
                ) |>
                dplyr::mutate(
                    weighted_pindex_change = dplyr::case_when(
                        !is.na(weighted_pindex) ~ (
                            (weighted_pindex - weighted_pindex_ref) / weighted_pindex_ref
                        ) * 100,
                        TRUE ~ NA_real_
                    )
                ) |>
                dplyr::select(-c(weighted_pindex_ref))
            
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
                        "_change.xlsx"
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