aggregating_combined_regional_effects <- function(
    combined_region_effects = NA,
    grids_municipalities = NA
) {
    #' @title Aggregating combined regional effects
    #' 
    #' @description This function aggregates combined regional effects to a larger
    #' regional level (municipality and districts).
    #' 
    #' @param combined_region_effects List with combined regional effects
    #' @param grids_municipalities Data frame with connection between grids and
    #' municipalities
    #' 
    #' @return List with aggregated combined regional effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # loop through results and aggregation levels

    results_list <- list()
    for (result in names(combined_region_effects)) {
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

            if (result == "year") {
                time_label <- "year"
                reference_period <- "2008"
            } else {
                time_label <- "quarter"
                reference_period <- "2008-01"
            }

            #--------------------------------------------------
            # prepare combined regional effects

            combined_effects_prep <- combined_region_effects[[result]] |>
                dplyr::select(-c("HK_nobs", "WK_nobs", "WM_nobs")) |>
                dplyr::rename(
                    nobs_grid = total_nobs,
                    pindex = weighted_pindex
                ) |>
                merge(
                    grids_municipalities,
                    by.x = "grid",
                    by.y = "ergg_1km",
                    all.x = TRUE
                ) |>
                dplyr::mutate(
                    kid2019 = substring(gid2019, 1, 5)
                )

            #--------------------------------------------------
            # add number of observations at higher level of aggregation

            combined_effects_prep <- combined_effects_prep |>
                dplyr::group_by(
                    !!rlang::sym(time_label),
                    !!rlang::sym(region_id)
                ) |>
                dplyr::mutate(
                    !!nobs_var := sum(nobs_grid, na.rm = TRUE)
                ) |>
                dplyr::ungroup()

            #--------------------------------------------------
            # calculate weight
            # NOTE: weight follows the definition of:
            # NOBS_ejahr_time_grid / SUM(NOBS_ejahr_time_region)

            combined_effects_prep <- combined_effects_prep |>
                dplyr::mutate(
                    weight = nobs_grid / .data[[nobs_var]],
                    weighted_pindex = pindex * weight
                )

            #--------------------------------------------------
            # aggregate to higher level

            combined_effects_agg <- combined_effects_prep |>
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

            combined_effects_agg <- combined_effects_agg |>
                merge(
                    combined_effects_agg |>
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
                combined_effects_agg,
                file.path(
                    config_paths()[["output_path"]],
                    "Combined_rebuild",
                    paste0(
                        "combined_regional_effects_",
                        region_label,
                        "_",
                        time_label,
                        ".xlsx"
                    )
                )
            )

            #--------------------------------------------------
            # store results

            results_list[[paste0(region_label, "_", time_label)]] <- combined_effects_agg
        }
    }
    
    #--------------------------------------------------
    # return

    return(results_list)
}