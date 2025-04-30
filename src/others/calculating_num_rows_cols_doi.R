calculating_num_rows_cols_doi <- function(
    housing_types_labels = NA,
    dependency = NA
) {
    #' @title Calculate number of rows and columns for DOI
    #' 
    #' @description This function calculates the number of rows and columns for
    #' each housing type. This information is needed for the DOI registration.
    #' 
    #' @param housing_types_labels Character with housing types
    #' @param dependency Placeholder for target object that needs to be created
    #' before executing this function
    #' 
    #' @return NULL, direct export
    #' @author Patrick Thiel

    #--------------------------------------------------
    # check depdenency

    targets::tar_assert_nonempty(
        dependency,
        msg = glue::glue(
            "!!! WARNING:",
            "The dependency object is empty.",
            " (Error code: rw#1)"
        )
    )
    
    #--------------------------------------------------
    # define export file (for storage)

    export_file <- file.path(
        config_paths()[["output_path"]],
        "info",
        "doi_info.txt"
    )

    #--------------------------------------------------
    # define options

    anonym_types <- c("SUF", "PUF")

    housing_types_labels_extended <- c(
        housing_types_labels,
        "CombInd",
        "GRIDS"
    )

    #--------------------------------------------------
    # loop through options

    for (anonym_type in anonym_types) {
        # define header for export file
        header <- paste0(
            "\n",
            "Output for REDX ",
            config_globals()[["next_version"]],
            "-",
            anonym_type,
            "\n"
        )

        write(
            header,
            file = export_file,
            append = TRUE
        )

        for (data_type in housing_types_labels_extended) {
            # define directory
            directory <- file.path(
                config_paths()[["output_path"]],
                "export",
                paste0(
                    "RWIGEOREDX_",
                    data_type,
                    "_",
                    config_globals()[["next_version"]],
                    "_",
                    anonym_type,
                    ".xlsx"
                )
            )

            # open workbook
            workbook <- openxlsx::loadWorkbook(directory)

            # get sheet names
            sheet_names <- names(workbook)

            # exclude information
            sheet_names <- sheet_names[
                !grepl("Information", sheet_names)
            ]

            # loop through all sheets and calculate the number of rows and columns
            nrows <- 0
            ncols <- 0
            for (sheet_name in sheet_names) {
                dta <- openxlsx::read.xlsx(
                    directory,
                    sheet = sheet_name
                )

                nrows <- nrows + nrow(dta)
                ncols <- ncols + ncol(dta)                
            }

            #--------------------------------------------------
            # export findings

            export_housing_type <- paste0(
                "#-----------------------\n",
                "Count for ",
                data_type,
                "\nTotal number of rows: ",
                nrows,
                "\nTotal number of columns: ",
                ncols,
                "\n#-----------------------"
            )

            write(
                export_housing_type,
                file = export_file,
                append = TRUE
            )
        }
    }

    #--------------------------------------------------
    # return

    return(NULL)
}

