creating_export_workbooks <- function() {
    #' @title Create empty export workbooks
    #' 
    #' @description This function creates empty workbooks for the export of the
    #' REDGEOREDX data which is the final data which goes to the user.
    #' 
    #' @return Key word "WORKED" to indicate that the function has run
    #' @author Patrick Thiel

    #--------------------------------------------------
    # helper function to create empty workbooks
    # needed for the export of the RED-GEO-REDX data

    helpers_defining_workbook_directory <- function(
        housing_type_labels = NA,
        workbook_suffix = NA,
        time_label = NA
    ) {
        for (anonym_type in c("PUF", "SUF")) {
            for (data_type in housing_type_labels) {
                # create empty workbook
                empty_workbook <- openxlsx::createWorkbook()

                # define directory
                directory <- file.path(
                    config_paths()[["output_path"]],
                    "export",
                    paste0(
                        "RWIGEOREDX_",
                        toupper(data_type),
                        "_",
                        toupper(config_globals()[["next_version"]]),
                        "_",
                        anonym_type,
                        "_",
                        toupper(time_label),
                        "_",
                        workbook_suffix,
                        ".xlsx"
                    )
                )

                if (!file.exists(directory)) {
                    openxlsx::saveWorkbook(
                        empty_workbook,
                        file = directory
                    )
                }
            }
        }
    }

    #--------------------------------------------------
    # create workbooks for aggregation levels and time periods

    for (time_period in config_globals()[["time_periods"]]) {
        # workbooks for absolute values
        # NOTE: CombInd is not included as it is only calculated for the deviations
        helpers_defining_workbook_directory(
            housing_type_labels = c("ApPurc", "ApRent", "HouPurc"),
            workbook_suffix = "ABS",
            time_label = time_period
        )

        # workbooks for the deviations within regions (relative to base period)
        helpers_defining_workbook_directory(
            housing_type_labels = c("ApPurc", "ApRent", "CombInd", "HouPurc"),
            workbook_suffix = "DEV_REGION",
            time_label = time_period
        )

        # workbooks for the deviations within regions (relative to base period)
        helpers_defining_workbook_directory(
            housing_type_labels = c("ApPurc", "ApRent", "CombInd", "HouPurc"),
            workbook_suffix = "DEV_CROSS",
            time_label = time_period
        )
    }

    #--------------------------------------------------
    # read all created file paths

    created_files <- list.files(
        file.path(
            config_paths()[["output_path"]],
            "export"
        ),
        full.names = TRUE
    )

    # keep only Excel workbooks
    created_files <- created_files[
        stringr::str_detect(created_files, "\\.xlsx$")
    ]

    #--------------------------------------------------
    # check that the number of created workbooks is correct

    n_anonym_types <- 2 # PUF and SUF
    n_housing_types <- length(config_globals()[["housing_types"]])
    n_time_periods <- 2 # year and quarter
    n_workbook_suffixes <- 3 # ABS, DEV_REGION, DEV_CROSS
    n_exception_CI <- 4 # CombInd (CI) does not come for ABS (absolute), i.e.
    # not for ABS PUF YEAR, ABS PUF QUARTER, ABS SUF YEAR, ABS SUF QUARTER (= 4)

    total_n <- (
        n_anonym_types *
        n_housing_types *
        n_time_periods *
        n_workbook_suffixes
    ) - n_exception_CI

    targets::tar_assert_true(
        length(created_files) == total_n,
        msg = glue::glue(
            "!!! WARNING: ",
            "The number of created workbooks ({length(created_files)}) does not
            match the expected number ({total_n}).",
            " (Error code: cew#1)"
        )
    )

    #--------------------------------------------------
    # return

    return(created_files)
}