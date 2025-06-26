exporting_aggregated_regional_effects_abs <- function(
    aggregated_region_effects_list = NA,
    housing_type = NA,
    housing_type_label = NA
) {
    #' @title Exporting aggregated regional effects (change values)
    #' 
    #' @description This function exports the aggregated regional effects
    #' (change values) for the different regional delineations. The data is also
    #' anonymized (individual housing types only, NOT combined index).
    #' 
    #' @param aggregated_region_effects List with estimated aggregated regional
    #' effects (change values)
    #' @param housing_type Housing type
    #' @param housing_type_label Housing type labels
    #' 
    #' @return List with exported aggregated regional effects (change values)
    #' @author Patrick Thiel

    #--------------------------------------------------
    # loop through different regional delineations

    results_list <- list()
    for (region_time in names(aggregated_region_effects_list)) {
        #--------------------------------------------------
        # general setup

        region <- unlist(stringr::str_split(region_time, "_"))[1]
        time_label <- unlist(stringr::str_split(region_time, "_"))[2]
        nobs_var <- helpers_regional_effects_settings(agg_level = region)[["nobs_var"]]
        region_id <- helpers_regional_effects_settings(agg_level = region)[["region_id"]]
        sheet_name <- helpers_regional_effects_settings(agg_level = region)[["sheet_name"]]

        #--------------------------------------------------
        # subset data

        region_data <- aggregated_region_effects_list[[region_time]]

        # remove NAs in region identifier
        region_data <- region_data[!is.na(region_data[[region_id]]), ]

        #--------------------------------------------------
        # reshape columns

        reshaped_data <- helpers_reshaping_region_effects(
            region_effects_data = region_data,
            regional_col = region_id,
            pindex_col = "weighted_pindex",
            nobs_col = nobs_var,
            time_col = time_label
        )

        #--------------------------------------------------
        # sort columns

        sorted_data <- helpers_sorting_columns_region_effects(
            region_effects_data = reshaped_data,
            housing_type = housing_type,
            regional_col = region_id,
            time_col = time_label,
            grids = FALSE
        )

        #--------------------------------------------------
        # add mean

        weighted_means <- region_data |>
            dplyr::group_by(
                !!rlang::sym(time_label)
            ) |>
            dplyr::summarise(
                overall_weighted_mean = weighted.mean(
                    weighted_pindex,
                    w = .data[[nobs_var]],
                    na.rm = TRUE
                )
            ) |>
            dplyr::mutate(
                # add name column (needed for row binding to overall data)
                var_name = paste0(
                    "pindex",
                    .data[[time_label]]
                ),
                # add empty NOBS column (so you have same structure as the 
                # overall/ unmeaned data)
                nobs_var = paste0(
                    "NOBS",
                    .data[[time_label]]
                ),
                nobs_value = NA_real_
            )
            
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

        # add region id (for row binding with overall data)
        weighted_means_wide[[region_id]] <- "Weighted Mean"

        # sort columns
        weighted_means_wide <- helpers_sorting_columns_region_effects(
            region_effects_data = weighted_means_wide,
            housing_type = housing_type,
            regional_col = region_id,
            time_col = time_label,
            grids = FALSE
        )

        #--------------------------------------------------
        # anonymize the data
        # Only applied to individual housing types (not to the combined index)

        anonymized_data_list <- list()
        # apply anonymization function
        dta_suf <- helpers_anonymizing_region_effects(
            region_effects_data = sorted_data,
            SUF = TRUE
        )

        dta_puf <- helpers_anonymizing_region_effects(
            region_effects_data = sorted_data,
            SUF = FALSE
        )

        # add mean
        dta_suf <- dplyr::bind_rows(
            dta_suf,
            weighted_means_wide
        )

        dta_puf <- dplyr::bind_rows(
            dta_puf,
            weighted_means_wide
        )

        # store anonymized data
        anonymized_data_list[[paste0(housing_type, "_SUF")]] <- dta_suf
        anonymized_data_list[[paste0(housing_type, "_PUF")]] <- dta_puf

        #--------------------------------------------------
        # export

        for (dta_name in names(anonymized_data_list)) {
            dta_name_clean <- stringr::str_remove(
                dta_name,
                paste0(housing_type, "_")
            )
            dta <- anonymized_data_list[[dta_name]]

            # load workbook
            complete_wb <- openxlsx::loadWorkbook(
                file.path(
                    config_paths()[["output_path"]],
                    "export",
                    paste0(
                        "RWIGEOREDX_",
                        housing_type_label,
                        "_",
                        config_globals()[["next_version"]],
                        "_",
                        dta_name_clean,
                        ".xlsx"
                    )
                )
            )

            # add sheet if not existent
            if (time_label == "year") {
                if (
                    !paste0(sheet_name, "_RegionEff_abs_yearly") %in%
                    names(complete_wb)
                ) {
                    openxlsx::addWorksheet(
                        complete_wb,
                        paste0(sheet_name, "_RegionEff_abs_yearly")
                    )
                }

                # write data
                openxlsx::writeData(
                    wb = complete_wb,
                    sheet = paste0(sheet_name, "_RegionEff_abs_yearly"),
                    x = dta
                )
            } else {
                if (
                    !paste0(sheet_name, "_RegionEff_abs_quarterly") %in%
                    names(complete_wb)
                ) {
                    openxlsx::addWorksheet(
                        complete_wb,
                        paste0(sheet_name, "_RegionEff_abs_quarterly")
                    )
                }

                # write data
                openxlsx::writeData(
                    wb = complete_wb,
                    sheet = paste0(sheet_name, "_RegionEff_abs_quarterly"),
                    x = dta
                )
            }

            # export workbook
            openxlsx::saveWorkbook(
                complete_wb,
                file = file.path(
                    config_paths()[["output_path"]],
                    "export",
                    paste0(
                        "RWIGEOREDX_",
                        housing_type_label,
                        "_",
                        config_globals()[["next_version"]],
                        "_",
                        dta_name_clean,
                        ".xlsx"
                    )
                ),
                overwrite = TRUE
            )
        }
        
        #--------------------------------------------------
        # store results

        results_list[[region_time]] <- anonymized_data_list
    }

    #--------------------------------------------------
    # return

    return(results_list)
}