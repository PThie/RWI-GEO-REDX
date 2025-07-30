combining_regional_effects <- function(
    HK_estimated_region_effects = NA,
    WK_estimated_region_effects = NA,
    WM_estimated_region_effects = NA,
    export_name_addendum = c("cross", "region")
) {
    #' @title Combining regional effects at regions (deviations in percent)
    #' 
    #' @description This function combines regional effects for all housing types
    #' at the regional level by weighting the individual housing type results
    #' by the number of observations.
    #' 
    #' @param HK_estimated_region_effects Estimated regional effects for HK
    #' @param WK_estimated_region_effects Estimated regional effects for WK
    #' @param WM_estimated_region_effects Estimated regional effects for WM
    #' @param export_name_addendum Addendum for the exported file name
    #' 
    #' @return List with combined regional effects
    #' @author Patrick Thiel    
    
    #--------------------------------------------------
    # combine all indices

    results_list <- list()
    for (region_time in names(HK_estimated_region_effects)) {
        #--------------------------------------------------
        # define settings

        region_label <- unlist(stringr::str_split(region_time, "_"))[[1]]
        time_label <- unlist(stringr::str_split(region_time, "_"))[[2]]
        region_id <- helpers_regional_effects_settings(agg_level = region_label)[["region_id"]]
        nobs_var <- helpers_regional_effects_settings(agg_level = region_label)[["nobs_var"]]

        #--------------------------------------------------
        # extract all individual effects

        # adding housing type
        HK_effects <- HK_estimated_region_effects[[region_time]] |>
            dplyr::mutate(housing_type = "HK")
        
        WK_effects <- WK_estimated_region_effects[[region_time]] |>
            dplyr::mutate(housing_type = "WK")
        
        WM_effects <- WM_estimated_region_effects[[region_time]] |>
            dplyr::mutate(housing_type = "WM")
        
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
                !!rlang::sym(region_id)
            ) |>
            dplyr::mutate(
                total_nobs = sum(.data[[nobs_var]], na.rm = TRUE),
                weight = .data[[nobs_var]] / total_nobs,
                weighted_pindex = pindex_dev_perc * weight
            ) |>
            dplyr::ungroup() |>
            dplyr::group_by(
                !!rlang::sym(time_label),
                !!rlang::sym(region_id)
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
                !!rlang::sym(region_id),
                !!rlang::sym(nobs_var),
                housing_type
            ) |>
            tidyr::pivot_wider(
                id_cols = c(time_label, region_id),
                names_from = housing_type,
                values_from = !!rlang::sym(nobs_var),
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
                by = c(time_label, region_id)
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
                    "combined_regional_effects_",
                    time_label,
                    "_",
                    region_label,
                    "_dev_perc_",
                    export_name_addendum,
                    ".xlsx"
                )
            )
        )

        #--------------------------------------------------
        # store results

        results_list[[region_time]] <- weighted_effects
    }

    #--------------------------------------------------
    # return

    return(results_list)
}