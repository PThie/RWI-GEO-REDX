helpers_reshaping_region_effects <- function(
    region_effects_data = NA,
    regional_col = c("grid", "kid2019", "gid2019", "lmrid"),
    pindex_col = c("pindex", "weighted_pindex"),
    nobs_col = c("nobs_grid", "total_nobs", "nobs_district", "nobs_munic", "nobs_lmr")
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
    #' 
    #' @return Data frame with reshaped region effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # reshape data

    reshaped_region_effects <- merge(
        region_effects_data |>
            dplyr::select(
                dplyr::all_of(c("year", regional_col, pindex_col))
            ) |>
            tidyr::pivot_wider(
                names_from = "year",
                values_from = pindex_col,
                names_glue = "pindex{year}"
            ),
        region_effects_data |>
            dplyr::select(
                dplyr::all_of(c("year", regional_col, nobs_col))
            ) |>
            tidyr::pivot_wider(
                names_from = "year",
                values_from = nobs_col,
                names_glue = "NOBS{year}"
            ),
        by = regional_col
    )

    #--------------------------------------------------
    # return

    return(reshaped_region_effects)
}
