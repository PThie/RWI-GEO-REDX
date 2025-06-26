combining_regional_effects_change <- function(
    HK_estimated_region_effects_abs = NA,
    WK_estimated_region_effects_abs = NA,
    WM_estimated_region_effects_abs = NA
) {
    #' @title Combining regional effects
    #' 
    #' @description This function combines regional effects for all housing types
    #' by weighting the individual housing type results by the number of
    #' observations.
    #' 
    #' @param HK_estimated_region_effects_abs Estimated regional effects for
    #' HK
    #' @param WK_estimated_region_effects_abs Estimated regional effects for
    #' WK
    #' @param WM_estimated_region_effects_abs Estimated regional effects for
    #' WM
    #' 
    #' @return List with combined regional effects
    #' @author Patrick Thiel    

    #--------------------------------------------------
    # combine all indices

    results_list <- list()
    for (result in c("year", "quarter")) {
        #--------------------------------------------------
        # set up for specific time levels

        time_label <- helpers_regional_effects_change_settings(time_period = result)[["time_label"]]
        reference_period <- helpers_regional_effects_change_settings(time_period = result)[["reference_period"]]

        #--------------------------------------------------
        # extract all individual effects

        HK_effects <- HK_estimated_region_effects_abs[[result]] |>
            dplyr::mutate(
                housing_type = "HK"
            )

        WK_effects <- WK_estimated_region_effects_abs[[result]] |>
            dplyr::mutate(
                housing_type = "WK"
            )
        
        WM_effects <- WM_estimated_region_effects_abs[[result]] |>
            dplyr::mutate(
                housing_type = "WM"
            )
        
        # combine all effects
        regional_effects <- rbind(
            HK_effects,
            WK_effects,
            WM_effects
        )

        #--------------------------------------------------
        # calculate the weighted effects for each grid-time combination

        weighted_effects <- regional_effects |>
            dplyr::group_by(
                !!rlang::sym(time_label),
                grid
            ) |>
            dplyr::mutate(
                total_nobs = sum(nobs_grid, na.rm = TRUE),
                weight = nobs_grid / total_nobs,
                weighted_pindex = pindex * weight
            ) |>
            dplyr::ungroup() |>
            dplyr::group_by(
                !!rlang::sym(time_label),
                grid
            ) |>
            dplyr::summarise(
                weighted_pindex = sum(weighted_pindex, na.rm = TRUE)
            ) |>
            dplyr::ungroup()

        #--------------------------------------------------
        # number of observations

        nobs <- regional_effects |>
            dplyr::select(
                !!rlang::sym(time_label),
                grid,
                nobs_grid,
                housing_type
            ) |>
            tidyr::pivot_wider(
                id_cols = c(time_label, "grid"),
                names_from = housing_type,
                values_from = nobs_grid,
                names_glue = "{housing_type}_nobs"
            ) |>
            dplyr::mutate(
                dplyr::across(
                    .cols = dplyr::contains("_nobs"),
                    ~ tidyr::replace_na(.x, 0)
                ),
                total_nobs = HK_nobs + WK_nobs + WM_nobs
            )
        
        #--------------------------------------------------
        # merge weighted effects and number of observations

        weighted_effects <- weighted_effects |>
            merge(
                nobs,
                by = c(time_label, "grid")
            )

        #--------------------------------------------------
        # export

        openxlsx::write.xlsx(
            weighted_effects,
            file.path(
                config_paths()[["output_path"]],
                "CI",
                "estimates",
                paste0(
                    "combined_regional_effects_grids_",
                    time_label,
                    "_absolute.xlsx"
                )
            )
        )

        #--------------------------------------------------
        # store results
        results_list[[time_label]] <- weighted_effects
    }

    #--------------------------------------------------
    # return

    return(results_list)
}