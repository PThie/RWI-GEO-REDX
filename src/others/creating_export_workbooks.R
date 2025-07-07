creating_export_workbooks <- function() {
    #' @title Create empty export workbooks
    #' 
    #' @description This function creates empty workbooks for the export of the
    #' REDGEOREDX data which is the final data which goes to the user.
    #' 
    #' @return NULL
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
    # return

    return(NULL)
}