exporting_aggregated_regional_effects <- function(
    aggregated_region_effects = NA,
    housing_type = NA,
    housing_type_label = NA
) {
    #' @title Exporting aggregated regional effects
    #' 
    #' @description This function exports the aggregated regional effects for
    #' the different regional delineations. The data is also anonymized
    #' (individual housing types only, NOT combined index).
    #' 
    #' @param aggregated_region_effects List with estimated aggregated regional
    #' effects
    #' @param housing_type Housing type
    #' @param housing_type_label Housing type labels
    #' 
    #' @return List with exported aggregated regional effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # loop through different regional delineations

    results_list <- list()
    for (region in names(aggregated_region_effects)) {
        #--------------------------------------------------
        # general setup

        if (region == "district") {
            nobs_col <- "nobs_district"
            regional_col <- "kid2019"
            sheet_name <- "District"
        } else {
            nobs_col <- "nobs_munic"
            regional_col <- "gid2019"
            sheet_name <- "Munic"
        }

        #--------------------------------------------------
        # subset data

        region_data <- aggregated_region_effects[[region]]

        # remove NAs in region identifier
        region_data <- region_data[!is.na(region_data[[regional_col]]), ]

        #--------------------------------------------------
        # reshape columns

        reshaped_data <- helpers_reshaping_region_effects(
            region_effects_data = region_data,
            regional_col = regional_col,
            pindex_col = "weighted_pindex",
            nobs_col = nobs_col
        )

        #--------------------------------------------------
        # sort columns

        sorted_data <- helpers_sorting_columns_region_effects(
            region_effects_data = reshaped_data,
            housing_type = housing_type,
            regional_col = regional_col,
            grids = FALSE
        )

        #--------------------------------------------------
        # anonymize the data
        # Only applied to individual housing types (not to the combined index)

        anonymized_data_list <- list()
        if (housing_type %in% c("HK", "WK", "WM")) {
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
        } else {
            anonymized_data_list[[paste0(housing_type, "_SUF")]] <- sorted_data
            anonymized_data_list[[paste0(housing_type, "_PUF")]] <- sorted_data
        }

        #--------------------------------------------------
        # export
        # TODO: change output folder (shouldn't be Temp_Export)

        for (dta_name in names(anonymized_data_list)) {
            dta_name_clean <- stringr::str_remove(dta_name, paste0(housing_type, "_"))
            dta <- anonymized_data_list[[dta_name]]

            # load workbook
            complete_wb <- openxlsx::loadWorkbook(
                file.path(
                    config_paths()[["output_path"]],
                    "Temp_Export",
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
                !paste0(sheet_name, "_RegionEff_yearly") %in%
                names(complete_wb)
            ) {
                openxlsx::addWorksheet(
                    complete_wb,
                    paste0(sheet_name, "_RegionEff_yearly")
                )
            }

            # write data
            openxlsx::writeData(
                wb = complete_wb,
                sheet = paste0(sheet_name, "_RegionEff_yearly"),
                x = dta
            )

            # export workbook
            openxlsx::saveWorkbook(
                complete_wb,
                file = file.path(
                    config_paths()[["output_path"]],
                    "Temp_Export",
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

        results_list[[region]] <- anonymized_data_list
    }

    #--------------------------------------------------
    # return

    return(results_list)
}