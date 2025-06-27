exporting_region_effects_abs_grids <- function(
    HK_estimated_region_effects_abs = NA,
    WK_estimated_region_effects_abs = NA,
    WM_estimated_region_effects_abs = NA
) {
    #' @title Exporting region effects
    #' 
    #' @description This function exports the estimated region effects (at the
    #' grid level) for the different housing types and the combined index.
    #' The data is also anonymized (individual housing types only, NOT combined
    #' index).
    #' 
    #' @param HK_estimated_region_effects_abs Data frame with estimated region
    #' effects for housing type HK
    #' @param WK_estimated_region_effects_abs Data frame with estimated region
    #' effects for housing type WK
    #' @param WM_estimated_region_effects_abs Data frame with estimated region
    #' effects for housing type WM
    #' 
    #' @return List with exported region effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    for (time_label in names(HK_estimated_region_effects_abs)) {
        #--------------------------------------------------
        # group all data frames into a list

        data_list <- list(
            HK = HK_estimated_region_effects_abs[[time_label]],
            WK = WK_estimated_region_effects_abs[[time_label]],
            WM = WM_estimated_region_effects_abs[[time_label]]
        )

        #--------------------------------------------------
        # reshape columns

        reshaped_data_list <- list()
        for (dta in names(data_list)) {
            reshaped <- helpers_reshaping_region_effects(
                region_effects_data = data_list[[dta]],
                regional_col = "grid",
                pindex_col = "pindex",
                nobs_col = "nobs_grid",
                time_col = time_label
            )
            # }

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
            c("HK", "WK", "WM"),
            "grid",
            time_label,
            SIMPLIFY = FALSE
        )

        #--------------------------------------------------
        # calculate weighted means

        weighted_means_list <- list()
        for (dta in names(data_list)) {
            idx <- "pindex"
            nvar <- "nobs_grid"

            weighted_means <- helpers_calculating_regional_means(
                region_data = data_list[[dta]],
                index_col = idx,
                region_id_col = "grid",
                nobs_col = nvar,
                time_col = time_label
            ) |>
            # add housing type
            dplyr::mutate(
                housing_type = dta
            ) |>
            dplyr::relocate(
                housing_type,
                .after = "grid"
            )

            # add to list
            weighted_means_list[[dta]] <- weighted_means
        }

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

            # add mean
            dta_suf <- dplyr::bind_rows(
                dta_suf,
                weighted_means_list[[housing_type]]
            )

            dta_puf <- dplyr::bind_rows(
                dta_puf,
                weighted_means_list[[housing_type]]
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
            anonymized_data_list[["CI_SUF"]]
        )
        
        all_data_PUF <- rbind(
            data.table::rbindlist(anonymized_data_list[
                c("HK_PUF", "WK_PUF", "WM_PUF")
            ]),
            anonymized_data_list[["CI_PUF"]]
        )

        # pool together for export
        all_data_list <- list(
            "SUF" = all_data_SUF,
            "PUF" = all_data_PUF
        )

        #--------------------------------------------------
        # export

        for (dta_name in names(all_data_list)) {
            dta <- all_data_list[[dta_name]]

            # load workbook
            complete_wb <- openxlsx::loadWorkbook(
                file.path(
                    config_paths()[["output_path"]],
                    "export",
                    paste0(
                        "RWIGEOREDX_GRIDS_",
                        config_globals()[["next_version"]],
                        "_",
                        dta_name,
                        ".xlsx"
                    )
                )
            )

            # add sheet if not existent
            if (time_label == "year") {
                if (
                    !"Grids_RegionEff_abs_yearly" %in%
                    names(complete_wb)
                ) {
                    openxlsx::addWorksheet(
                        complete_wb,
                        "Grids_RegionEff_abs_yearly"
                    )
                }

                # write data
                openxlsx::writeData(
                    wb = complete_wb,
                    sheet = "Grids_RegionEff_abs_yearly",
                    x = dta
                )
            } else {
                if (
                    !"Grids_RegionEff_abs_quarterly" %in%
                    names(complete_wb)
                ) {
                    openxlsx::addWorksheet(
                        complete_wb,
                        "Grids_RegionEff_abs_quarterly"
                    )
                }

                # write data
                openxlsx::writeData(
                    wb = complete_wb,
                    sheet = "Grids_RegionEff_abs_quarterly",
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
    
    #--------------------------------------------------
    # return

    return(all_data_list)
}