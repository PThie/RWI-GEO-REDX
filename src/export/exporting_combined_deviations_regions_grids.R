exporting_combined_deviations_regions_grids <- function(
    combined_effects = NA,
    pindex_col_name = "weighted_pindex",
    nvar = "total_nobs",
    housing_type = "CI",
    export_name_addendum = c("dev_perc_cross", "dev_perc_region")
) {
    #' @title Exporting combined deviations for grids
    #' 
    #' @description This function exports the combined deviations for grids
    #' (deviations in percent).
    #' 
    #' @param combined_effects List with combined effects for grids
    #' @param pindex_col_name Name of the column with the price index
    #' @param nvar Name of the column with the number of observations
    #' @param housing_type Housing type (default: "CI" for combined index)
    #' @param export_name_addendum Addendum for the exported file name
    #' 
    #' @return List with exported combined deviations for grids
    #' @author Patrick Thiel

    #--------------------------------------------------
    all_results <- list()
    for (time_label in names(combined_effects)) {
        #--------------------------------------------------
        # extract data

        extracted_data <- combined_effects[[time_label]]
        
        #--------------------------------------------------
        # reshape columns

        reshaped_data <- helpers_reshaping_region_effects(
            region_effects_data = extracted_data,
            regional_col = "grid",
            pindex_col = pindex_col_name,
            nobs_col = nvar,
            time_col = time_label
        )

        #--------------------------------------------------
        # sort columns

        sorted_data <- helpers_sorting_columns_region_effects(
            region_effects_data = reshaped_data,
            housing_type = housing_type,
            regional_col = "grid",
            time_col = time_label,
            grids = TRUE
        )

        #--------------------------------------------------
        # calculate weighted means
    
        weighted_means <- helpers_calculating_regional_means(
            region_data = extracted_data,
            index_col = pindex_col_name,
            region_id_col = "grid",
            nobs_col = nvar,
            time_col = time_label
        ) |>
        # add housing type
        dplyr::mutate(
            housing_type = housing_type
        ) |>
        dplyr::relocate(
            housing_type,
            .after = "grid"
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

        for (anonym_type in names(anonymized_data_list)) {
            dta <- anonymized_data_list[[anonym_type]]

            # create export directory
            directory <- file.path(
                config_paths()[["output_path"]],
                "export",
                paste0(
                    "RWIGEOREDX_GRIDS_",
                    toupper(config_globals()[["next_version"]]),
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
        all_results[[time_label]] <- anonymized_data_list
    }

    #--------------------------------------------------
    # load files that have been modified

    files <- list.files(
        file.path(
            config_paths()[["output_path"]],
            "export"
        ),
        full.names = TRUE
    )

    # only keep csv and parquet files
    files <- files[
        stringr::str_detect(files, "\\.(csv|parquet)$")
    ]

    # keep only exported files depending on type
    files <- files[
        stringr::str_detect(files, toupper(export_name_addendum))
    ]

    # keep only files that are CI
    # because the other GRIDS files have their own export function
    files <- files[
        stringr::str_detect(files, "CI")
    ]

    # define what number of exported files to expect
    n_file_formats <- 2 # CSV, parquet
    n_anonym_types <- 2 # PUF and SUF
    n_time_periods <- 2 # year and quarter
    # NOTE: CombInd (CI) has different types because not all values can be
    # calculated for CI
    # NOTE: each type is exported separately such that the number is set to one
    n_types <- 1 # DEV PERC REGION, DEV PERC CROSS

    total_n <- (
        n_file_formats *
        n_anonym_types *
        n_time_periods *
        n_types
    )

    targets::tar_assert_true(
        length(files) == total_n,
        msg = glue::glue(
            "!!! WARNING: ",
            "The number of GRIDS files ({length(files)}) does not
            match the expected number ({total_n}).",
            " (Error code: ecdrg#1)"
        )
    )

    #--------------------------------------------------
    # return

    return(files)
}