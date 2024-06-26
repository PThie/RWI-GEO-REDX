combining_regional_effects <- function(
    HK_estimated_region_effects = NA,
    WK_estimated_region_effects = NA,
    WM_estimated_region_effects = NA
) {
    #' @title Combining regional effects
    #' 
    #' @description This function combines regional effects for all housing types
    #' by weighting the individual housing type results by the number of
    #' observations.
    #' 
    #' @param HK_estimated_region_effects Estimated regional effects for HK
    #' @param WK_estimated_region_effects Estimated regional effects for WK
    #' @param WM_estimated_region_effects Estimated regional effects for WM
    #' 
    #' @return Combined regional effects
    #' @author Patrick Thiel    
    
    #--------------------------------------------------
    # only one time period
    # Therefore no loop

    time_label <- "year"

    #--------------------------------------------------
    # preparation

    # adding housing type
    HK_effects <- HK_estimated_region_effects |>
        dplyr::mutate(housing_type = "HK")
    WK_effects <- WK_estimated_region_effects |>
        dplyr::mutate(housing_type = "WK")
    WM_effects <- WM_estimated_region_effects |>
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
            "Combined_rebuild",
            paste0("combined_regional_effects_grids_", time_label, ".xlsx")
        )
    )

    #--------------------------------------------------
    # return

    return(weighted_effects)
}