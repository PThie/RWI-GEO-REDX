exporting_aggregated_regional_effects <- function(
    aggregated_region_effects_list = NA,
    housing_type = NA,
    housing_type_label = NA,
    pindex_col_name = c("weighted_pindex", "pindex_dev", "pindex_dev_perc"),
    sheet_name_addendum = c("abs", "dev", "dev_perc"),
    export_name_addendum = c("abs", "dev_cross", "dev_region"),
    dependencies = NA
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
    #' @param pindex_col_name Name of the column with the price index
    #' @param sheet_name_addendum Addendum for the sheet name
    #' @param export_name_addendum Addendum for the exported file name
    #' @param dependencies Dependencies for the function
    #' 
    #' @return List with exported aggregated regional effects (change values)
    #' @author Patrick Thiel

    #--------------------------------------------------
    # check dependencies

    targets::tar_assert_nonempty(dependencies)

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
            pindex_col = pindex_col_name,
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
        # calculate weighted means across regions

        weighted_means <- helpers_calculating_regional_means(
            region_data = region_data,
            index_col = pindex_col_name,
            region_id_col = region_id,
            nobs_col = nobs_var,
            time_col = time_label
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
            weighted_means
        )

        dta_puf <- dplyr::bind_rows(
            dta_puf,
            weighted_means
        )

        # store anonymized data
        anonymized_data_list[[paste0(housing_type, "_SUF")]] <- dta_suf
        anonymized_data_list[[paste0(housing_type, "_PUF")]] <- dta_puf

        #--------------------------------------------------
        # export

        for (dta_name in names(anonymized_data_list)) {
            # extract anonymization type (SUF or PUF)
            anonym_type <- stringr::str_remove(
                dta_name,
                paste0(housing_type, "_")
            )

            # extract actual data for anonymization type
            dta <- anonymized_data_list[[dta_name]]

            # load workbook
            complete_wb <- openxlsx::loadWorkbook(
                file.path(
                    config_paths()[["output_path"]],
                    "export",
                    paste0(
                        "RWIGEOREDX_",
                        toupper(housing_type_label),
                        "_",
                        config_globals()[["next_version"]],
                        "_",
                        anonym_type,
                        "_",
                        toupper(time_label),
                        "_",
                        toupper(export_name_addendum),
                        ".xlsx"
                    )
                )
            )

            # add sheet if not existent
            if (time_label == "year") {
                if (
                    !paste0(sheet_name, "_RegionEff_", sheet_name_addendum, "_yearly") %in%
                    names(complete_wb)
                ) {
                    openxlsx::addWorksheet(
                        complete_wb,
                        paste0(sheet_name, "_RegionEff_", sheet_name_addendum, "_yearly")
                    )
                }

                # write data
                openxlsx::writeData(
                    wb = complete_wb,
                    sheet = paste0(sheet_name, "_RegionEff_", sheet_name_addendum, "_yearly"),
                    x = dta
                )
            } else {
                if (
                    !paste0(sheet_name, "_RegionEff_", sheet_name_addendum, "_quarterly") %in%
                    names(complete_wb)
                ) {
                    openxlsx::addWorksheet(
                        complete_wb,
                        paste0(sheet_name, "_RegionEff_", sheet_name_addendum, "_quarterly")
                    )
                }

                # write data
                openxlsx::writeData(
                    wb = complete_wb,
                    sheet = paste0(sheet_name, "_RegionEff_", sheet_name_addendum, "_quarterly"),
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
                        toupper(housing_type_label),
                        "_",
                        config_globals()[["next_version"]],
                        "_",
                        anonym_type,
                        "_",
                        toupper(time_label),
                        "_",
                        toupper(export_name_addendum),
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