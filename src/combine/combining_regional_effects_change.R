combining_regional_effects_change <- function(
    HK_estimated_region_effects_change = NA,
    WK_estimated_region_effects_change = NA,
    WM_estimated_region_effects_change = NA
) {
    #' @title Combining regional effects (with change values)
    #' 
    #' @description This function combines regional effects for all housing types
    #' by weighting the individual housing type results by the number of
    #' observations. It also calculates the change to the reference period.
    #' 
    #' @param HK_estimated_region_effects_change Estimated regional effects for
    #' HK (with change values)
    #' @param WK_estimated_region_effects_change Estimated regional effects for
    #' WK (with change values)
    #' @param WM_estimated_region_effects_change Estimated regional effects for
    #' WM (with change values)
    #' 
    #' @return List with combined regional effects
    #' @author Patrick Thiel    

    #--------------------------------------------------
    # combine all indices

    results_list <- list()
    for (result in c("ejahr", "e_year_quarter")) {
        #--------------------------------------------------
        # set up for specific time levels

        if (result == "ejahr") {
            time_label <- "year"
            reference_period <- "2008"
        } else {
            time_label <- "quarter"
            reference_period <- "2008-01"
        }

        #--------------------------------------------------
        # extract all individual effects

        HK_effects <- HK_estimated_region_effects[[result]] |>
            dplyr::mutate(
                housing_type = "HK"
            )

        WK_effects <- WK_estimated_region_effects[[result]] |>
            dplyr::mutate(
                housing_type = "WK"
            )
        
        WM_effects <- WM_estimated_region_effects[[result]] |>
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
            # calculate the change between District_Year - District_2008

            weighted_effects <- weighted_effects |>
                merge(
                    weighted_effects |>
                        dplyr::filter(!!rlang::sym(time_label) == reference_period) |>
                        dplyr::rename(weighted_pindex_ref = weighted_pindex) |>
                        dplyr::select(-c(time_label)),
                    by = "grid",
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
                "Combined_rebuild",
                paste0("combined_regional_effects_grids_", time_label, "_change.xlsx")
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