helpers_calculating_regional_means <- function(
    region_data = NA,
    index_col = NA,
    region_id_col = NA,
    nobs_col = NA,
    time_col = NA
) {
    #' @title Calculate weighted means across regions
    #' 
    #' @description This function calculates the weighted means of the
    #' pindex across regions.
    #' 
    #' @param region_data Data frame with regional data
    #' @param index_col Name of the column with the pindex values
    #' @param region_id_col Name of the column with the region id
    #' @param nobs_col Name of the column with the number of observations
    #' @param time_col Name of the column with the time information
    #' 
    #' @return Data frame with the weighted means across regions
    #' @author Patrick Thiel

    #--------------------------------------------------
    # calculate weighted means

    weighted_means <- region_data |>
        # NOTE: weighted mean cannot be calculated if there are NAs in the
        # NOBS variable
        dplyr::filter(
            !is.na(.data[[nobs_col]])
        ) |>
        dplyr::group_by(
            !!rlang::sym(time_col)
        ) |>
        dplyr::summarise(
            overall_weighted_mean = stats::weighted.mean(
                !!rlang::sym(index_col),
                w = .data[[nobs_col]],
                na.rm = TRUE
            )
        ) |>
        dplyr::mutate(
            # add name column (needed for row binding to overall data)
            var_name = paste0(
                "pindex",
                .data[[time_col]]
            ),
            # add empty NOBS column (so you have same structure as the 
            # overall/ unmeaned data)
            nobs_var = paste0(
                "NOBS",
                .data[[time_col]]
            ),
            nobs_value = NA_real_
        )

    #--------------------------------------------------
    # make wide table (to resemble sorted data)

    weighted_means_wide <- cbind(
        weighted_means |>
            dplyr::select(var_name, overall_weighted_mean) |>
            tidyr::pivot_wider(
                names_from = var_name,
                values_from = overall_weighted_mean
            ),
        weighted_means |>
            dplyr::select(nobs_var, nobs_value) |>
            tidyr::pivot_wider(
                names_from = nobs_var,
                values_from = nobs_value
            )
    )

    #--------------------------------------------------
    # add region id (for row binding with overall data)
    
    weighted_means_wide[[region_id_col]] <- "Weighted Mean"

    #--------------------------------------------------
    # sort columns

    weighted_means_wide <- helpers_sorting_columns_region_effects(
        region_effects_data = weighted_means_wide,
        housing_type = housing_type,
        regional_col = region_id_col,
        time_col = time_col,
        grids = FALSE
    )

    #--------------------------------------------------
    # return

    return(weighted_means_wide)            
}
