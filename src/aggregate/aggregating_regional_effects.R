aggregating_regional_effects <- function(
    estimated_region_effects = NA,
    housing_type = NA
) {
    #' @title Aggregating regional effects
    #' 
    #' @description This function aggregates regional effects for a given housing
    #' type to a larger regional level (municipality and districts).
    #' 
    #' @param estimated_effects List with estimated regional effects
    #' @param housing_type Housing type
    #' 
    #' @note Based on regression 2 in the former Stata coding.
    #' 
    #' @return List with aggregated regional effects
    #' @author Patrick Thiel

    results_list <- list()
    for (agg_level in c("munic", "district")) {
        #--------------------------------------------------
        # set up for specific regional level
        # and time definitions
        
        if (agg_level == "munic") {
            nobs_var <- "nobs_munic"
            region_id <- "gid2019"
            region_label <- "municipality"
        } else {
            nobs_var <- "nobs_district"
            region_id <- "kid2019"
            region_label <- "district"
        }

        time_label <- "year"

        #--------------------------------------------------
        # calculate weights
        # NOTE: weight follows the definition of:
        # NOBS_ejahr_time_grid / SUM(NOBS_ejahr_time_region)
        # for each type separately
        # NOTE: no grouping by time because there is only one time definition
        # (year) and because of the cross-sectional approach each grid already
        # has time specific NOBS
        # same holds for NOBS on higher aggregation level (is already time
        # specific)

        estimated_effects <- estimated_region_effects |>
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
                paste0(housing_type, "_rebuild"),
                paste0(
                    "regional_effects_",
                    region_label,
                    "_",
                    time_label,
                    ".xlsx"
                )
            )
        )

        #--------------------------------------------------
        # store results
        
        results_list[[region_label]] <- estimated_effects_agg
    }

    #--------------------------------------------------
    # return

    return(results_list)
}