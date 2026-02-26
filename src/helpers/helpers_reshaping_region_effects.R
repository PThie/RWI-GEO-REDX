helpers_reshaping_region_effects <- function(
    region_effects_data = NA,
    regional_col = c("grid", "kid2019", "gid2019", "lmrid", "zipcodeid"),
    pindex_col = c("pindex", "weighted_pindex"),
    nobs_col = c("nobs_grid", "total_nobs", "nobs_district", "nobs_munic", "nobs_lmr", "nobs_zipcode"),
    time_col = c("year", "quarter")
) {
    #' @title Reshaping region effects
    #' 
    #' @description This function reshapes the region effects data frame to
    #' a wide format (both for price indices and NOBS).
    #' 
    #' @param region_effects_data Data frame with region effects
    #' @param regional_col Column names for regional identifier
    #' @param pindex_col Column names for price indices
    #' @param nobs_col Column names for NOBS
    #' @param time_col Column names for time variable
    #' 
    #' @return Data frame with reshaped region effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # reshape data

    reshaped_region_effects <- merge(
        region_effects_data |>
            dplyr::select(
                dplyr::all_of(c(time_col, regional_col, pindex_col))
            ) |>
            tidyr::pivot_wider(
                names_from = time_col,
                values_from = pindex_col,
                names_glue = glue::glue("pindex{{{time_col}}}")
            ),
        region_effects_data |>
            dplyr::select(
                dplyr::all_of(c(time_col, regional_col, nobs_col))
            ) |>
            tidyr::pivot_wider(
                names_from = time_col,
                values_from = nobs_col,
                names_glue = glue::glue("NOBS{{{time_col}}}")
            ),
        by = regional_col
    )

    #--------------------------------------------------
    # return

    return(reshaped_region_effects)
}
