exporting_region_effects_change_grids <- function(
    HK_estimated_region_effects_change = NA,
    WK_estimated_region_effects_change = NA,
    WM_estimated_region_effects_change = NA,
    combined_region_effects_change = NA
) {
    #' @title Exporting region effects (change effects)
    #' 
    #' @description This function exports the estimated region effects (at the
    #' grid level) for the different housing types and the combined index.
    #' The data is also anonymized (individual housing types only, NOT combined
    #' index).
    #' 
    #' @param HK_estimated_region_effects_change Data frame with estimated region effects
    #' for housing type HK (change values)
    #' @param WK_estimated_region_effects_change Data frame with estimated region effects
    #' for housing type WK (change values)
    #' @param WM_estimated_region_effects_change Data frame with estimated region effects
    #' for housing type WM (change values)
    #' @param combined_region_effects_change Data frame with estimated region effects
    #' for the combined index (change values)
    #' 
    #' @return List with exported region effects
    #' @author Patrick Thiel
    
    for (time in names(HK_estimated_region_effects_change)) {
        # TODO: expand to quarterly effects later
        # NOTE: focus for now on yearly effects to generate consistent output
        # for quarterly data the output has to be different since a column per
        # quarter would create too many columns
        if (time == "year") {
            #--------------------------------------------------
            # group all data frames into a list

            data_list <- list(
                HK = HK_estimated_region_effects_change[[time]],
                WK = WK_estimated_region_effects_change[[time]],
                WM = WM_estimated_region_effects_change[[time]],
                CI = combined_region_effects_change[[time]]
            )

            #--------------------------------------------------
            # reshape columns

            reshaped_data_list <- list()
            for (dta in names(data_list)) {
                if (dta == "CI") {
                    rehaped <- helpers_reshaping_region_effects(
                        region_effects_data = data_list[[dta]],
                        regional_col = "grid",
                        pindex_col = "weighted_pindex_change",
                        nobs_col = "total_nobs"
                    )
                } else {
                    reshaped <- helpers_reshaping_region_effects(
                        region_effects_data = data_list[[dta]],
                        regional_col = "grid",
                        pindex_col = "pindex_change",
                        nobs_col = "nobs_grid"
                    )
                }

                reshaped_data_list[[dta]] <- reshaped
            }

            #--------------------------------------------------
            # sort columns
            # NOTE: using mapply to loop over the list of data frames and the housing
            # types within the same iteration
            # NOTE: helpers_sorting_columns_region_effects is a helper function (see
            # helpers folder)

            sorted_data_list <- mapply(
                helpers_sorting_columns_region_effects,
                reshaped_data_list,
                c("HK", "WK", "WM", "CI"),
                "grid",
                SIMPLIFY = FALSE
            )

            #--------------------------------------------------
            # anonymize the data
            # Only applied to individual housing types (not to the combined index)

            anonymized_data_list <- list()
            for (housing_type in c("HK", "WK", "WM")) {
                dta <- sorted_data_list[[housing_type]]

                # apply anonymization function
                dta_suf <- helpers_anonymizing_region_effects(
                    region_effects_data = dta,
                    SUF = TRUE
                )

                dta_puf <- helpers_anonymizing_region_effects(
                    region_effects_data = dta,
                    SUF = FALSE
                )

                # store anonymized data
                anonymized_data_list[[paste0(housing_type, "_SUF")]] <- dta_suf
                anonymized_data_list[[paste0(housing_type, "_PUF")]] <- dta_puf
            }

            #--------------------------------------------------
            # row bind all data frames
            # separate for SUF and PUF

            all_data_SUF <- rbind(
                data.table::rbindlist(anonymized_data_list[
                    c("HK_SUF", "WK_SUF", "WM_SUF")
                ]),
                sorted_data_list[["CI"]]
            )
            
            all_data_PUF <- rbind(
                data.table::rbindlist(anonymized_data_list[
                    c("HK_PUF", "WK_PUF", "WM_PUF")
                ]),
                sorted_data_list[["CI"]]
            )

            # pool together for export
            all_data_list <- list(
                "SUF" = all_data_SUF,
                "PUF" = all_data_PUF
            )

            #--------------------------------------------------
            # export
            # TODO: change output folder (shouldn't be Temp_Export)

            for (dta_name in names(all_data_list)) {
                dta <- all_data_list[[dta_name]]

                # load workbook
                complete_wb <- openxlsx::loadWorkbook(
                    file.path(
                        config_paths()[["output_path"]],
                        "Temp_Export",
                        paste0(
                            "RWIGEOREDX_GRIDS_",
                            config_globals()[["next_version"]],
                            "_",
                            dta_name,
                            ".xlsx"
                        )
                    )
                )

                # add sheet
                openxlsx::addWorksheet(
                    complete_wb,
                    "Grids_RegionEff_Change_yearly"
                )

                # write data
                openxlsx::writeData(
                    wb = complete_wb,
                    sheet = "Grids_RegionEff_Change_yearly",
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
                            "GRIDS_",
                            config_globals()[["next_version"]],
                            "_",
                            dta_name,
                            ".xlsx"
                        )
                    ),
                    overwrite = TRUE
                )
            }
        }
    }
    
    #--------------------------------------------------
    # return

    return(all_data_list)
}