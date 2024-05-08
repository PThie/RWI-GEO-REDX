combining_time_effects <- function(
    WM_estimated_time_effects = NA,
    HK_estimated_time_effects = NA,
    WK_estimated_time_effects = NA
) {
    #' @title Combining time effects
    #' 
    #' @description This function combines time effects for all housing types
    #' by weighting the individual housing type results by the number of
    #' observations.
    #' 
    #' @param WM_estimated_time_effects Estimated time effects for WM
    #' @param HK_estimated_time_effects Estimated time effects for HK
    #' @param WK_estimated_time_effects Estimated time effects for WK
    #' 
    #' @return List with combined time effects
    #' @author Patrick Thiel

    #--------------------------------------------------
    # function to adapt output for each housing type

    adapting_output <- function(
        estimated_effects = NA,
        housing_type = NA
    ) {
        # add housing type
        estimated_effects <- estimated_effects |>
            dplyr::mutate(
                housing_type = housing_type
            ) |>
            dplyr::rename(
                mean_ln_price_sqm = ncol(estimated_effects)
            )

        return(estimated_effects)
    }

    #--------------------------------------------------
    # combining all data

    combined_results_list <- list()
    for(time in names(WM_estimated_time_effects)) {
        # extract respective time effects for all housing types
        HK_effects <- adapting_output(
            HK_estimated_time_effects[[time]],
            housing_type = "HK"
        )
        WK_effects <- adapting_output(
            WK_estimated_time_effects[[time]],
            housing_type = "WK"
        )
        WM_effects <- adapting_output(
            WM_estimated_time_effects[[time]],
            housing_type = "WM"
        )

        # bind time effects
        time_effects <- rbind(
            HK_effects,
            WK_effects,
            WM_effects
        )

        # store output
        combined_results_list[[time]] <- time_effects 
    }

    #--------------------------------------------------
    # add weights
    # weight defined as NOBS_ejahr_2008 for type X / SUM(NOBS_ejahr_2008)
    # across all types for each year in the sample
    # Same approach for quarters

    results_list <- list()
    for (result in names(combined_results_list)) {
        dta <- combined_results_list[[result]]

        # define time variable
        if (result == "ejahr") {
            time <- "year"
        } else {
            time <- "quarter"
        }
        
        # calculate the number of observations for each year and type
        # and then the total number of observations for each year
        # This gives the weight for each type in each year.
        # Same approach for quarters
        nobs <- dta |>
            dplyr::group_by(!!rlang::sym(time), housing_type) |>
            dplyr::summarise(
                nobs_type = sum(nobs)
            ) |>
            dplyr::ungroup() |>
            dplyr::group_by(!!rlang::sym(time)) |>
            dplyr::mutate(
                total_nobs = sum(nobs_type),
                weight = nobs_type / total_nobs
            )

        # calculate weighted coefficients
        dta_weighted <- dta |>
            dplyr::select(-nobs) |>
            merge(
                nobs,
                by = c(!!rlang::sym(time), "housing_type")
            ) |>
            dplyr::mutate(
                timeeff = timeeff * weight,
                timeeff_p025 = timeeff_p025 * weight,
                timeeff_p975 = timeeff_p975 * weight
            ) |>
            dplyr::group_by(year) |>
            dplyr::summarise(
                timeeff = sum(timeeff),
                timeeff_p025 = sum(timeeff_p025),
                timeeff_p975 = sum(timeeff_p975)
            )

        # reshape the NOBS data to wide format
        nobs_wide <- nobs |>
            dplyr::select(year, housing_type, nobs_type) |>
            tidyr::pivot_wider(
                names_from = housing_type,
                values_from = nobs_type
            ) |>
            dplyr::rename(
                HK_nobs = 2,
                WK_nobs = 3,
                WM_nobs = 4
            ) |>
            dplyr::mutate(
                total_nobs = HK_nobs + WK_nobs + WM_nobs
            )

        # merge number of observations and combined indices
        dta_weighted <- dta_weighted |>
            merge(
                nobs_wide,
                by = "year"
            )

        # export
        openxlsx::write.xlsx(
            dta_weighted,
            file.path(
                config_paths()[["output_path"]],
                "Combined_rebuild",
                paste0("combined_time_effects_grids_", result, ".xlsx")
            )
        )

        # store output
        results_list[[result]] <- dta_weighted
    }

    #--------------------------------------------------
    # return

    return(results_list)
}
