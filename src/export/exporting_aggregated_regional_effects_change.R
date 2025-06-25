exporting_aggregated_regional_effects_change <- function(
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

    # TODO: Add export for quarterly effects
    results_list <- list()
    for (region_time in names(aggregated_region_effects_list)) {
        # TODO: expand to quarterly effects later
        # NOTE: focus for now on yearly effects to generate consistent output
        # for quarterly data the output has to be different since a column per
        # quarter would create too many columns
        if (grepl("year", region_time) == TRUE) {
            #--------------------------------------------------
            # general setup

            region <- unlist(stringr::str_split(region_time, "_"))[1]

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
                nobs_col = nobs_var
            )

            #--------------------------------------------------
            # sort columns

            sorted_data <- helpers_sorting_columns_region_effects(
                region_effects_data = reshaped_data,
                housing_type = housing_type,
                regional_col = region_id,
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

            # store anonymized data
            anonymized_data_list[[paste0(housing_type, "_SUF")]] <- dta_suf
            anonymized_data_list[[paste0(housing_type, "_PUF")]] <- dta_puf

            #--------------------------------------------------
            # export

            for (dta_name in names(anonymized_data_list)) {
                dta_name_clean <- stringr::str_remove(dta_name, paste0(housing_type, "_"))
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
        }
        
        #--------------------------------------------------
        # store results

        results_list[[region_time]] <- anonymized_data_list
    }

    #--------------------------------------------------
    # return

    return(results_list)
}