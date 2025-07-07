exporting_region_effects_grids <- function(
    HK_estimated_region_effects = NA,
    WK_estimated_region_effects = NA,
    WM_estimated_region_effects = NA,
    pindex_col_name = c("pindex", "pindex_dev", "pindex_dev_perc"),
    export_name_addendum = c("abs", "dev", "dev_perc")
) {
    #' @title Exporting region effects
    #' 
    #' @description This function exports the estimated region effects (at the
    #' grid level) for the different housing types and the combined index.
    #' The data is also anonymized (individual housing types only, NOT combined
    #' index).
    #' 
    #' @param HK_estimated_region_effects Data frame with estimated region
    #' effects for housing type HK
    #' @param WK_estimated_region_effects Data frame with estimated region
    #' effects for housing type WK
    #' @param WM_estimated_region_effects Data frame with estimated region
    #' effects for housing type WM
    #' @param pindex_col_name Name of the column with the price index
    #' @param export_name_addendum Addendum for the exported file name
    #' 
    #' @return List with exported region effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    all_results <- list()
    for (time_label in names(HK_estimated_region_effects)) {
        #--------------------------------------------------
        # group all data frames into a list

        data_list <- list(
            HK = HK_estimated_region_effects[[time_label]],
            WK = WK_estimated_region_effects[[time_label]],
            WM = WM_estimated_region_effects[[time_label]]
        )

        #--------------------------------------------------
        # reshape columns

        reshaped_data_list <- list()
        for (dta in names(data_list)) {
            reshaped <- helpers_reshaping_region_effects(
                region_effects_data = data_list[[dta]],
                regional_col = "grid",
                pindex_col = pindex_col_name,
                nobs_col = "nobs_grid",
                time_col = time_label
            )

            reshaped_data_list[[dta]] <- reshaped
        }

        #--------------------------------------------------
        # sort columns
        # NOTE: using mapply to loop over the list of data frames and the housing
        # types within the same iteration
        # NOTE: helpers_sorting_columns_region_effects is a helper function (see
        # helpers folder)

        sorted_data_list <- list()
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

        all_data_SUF <- data.table::rbindlist(anonymized_data_list[
                c("HK_SUF", "WK_SUF", "WM_SUF")
            ])
        
        all_data_PUF <- data.table::rbindlist(anonymized_data_list[
                c("HK_PUF", "WK_PUF", "WM_PUF")
            ])
        
        # pool together for export
        all_data_list <- list(
            "SUF" = all_data_SUF,
            "PUF" = all_data_PUF
        )

        #--------------------------------------------------
        # export

        for (anonym_type in names(all_data_list)) {
            dta <- all_data_list[[anonym_type]]

            # create export directory
            directory <- file.path(
                config_paths()[["output_path"]],
                "export",
                paste0(
                    "RWIGEOREDX_GRIDS_",
                    config_globals()[["next_version"]],
                    "_",
                    anonym_type,
                    "_",
                    toupper(time_label),
                    "_",
                    toupper(export_name_addendum)
                )
            )

            # export data in CSV and parquet format
            for (file_format in c("csv", "parquet")) {
                file_name <- paste0(
                    directory,
                    ".",
                    file_format
                )

                if (file_format == "csv") {
                    data.table::fwrite(
                        dta,
                        file_name,
                        row.names = FALSE
                    )
                } else {
                    arrow::write_parquet(
                        dta,
                        file_name
                    )
                }
            }
        }

        # store all results
        all_results[[time_label]] <- all_data_list
    }
    
    #--------------------------------------------------
    # return

    return(all_results)
}