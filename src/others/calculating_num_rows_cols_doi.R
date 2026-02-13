calculating_num_rows_cols_doi <- function(
    housing_types_labels = NA
) {
    #' @title Calculate number of rows and columns for DOI
    #' 
    #' @description This function calculates the number of rows and columns for
    #' each housing type. This information is needed for the DOI registration.
    #' 
    #' @param housing_types_labels Character with housing types
    #' 
    #' @return NULL, direct export
    #' @author Patrick Thiel

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
        "CombInd"
    )

    time_periods <- config_globals()[["time_periods"]]

    data_types <- c("ABS", "DEV_CROSS", "DEV_REGION")

    #--------------------------------------------------
    # loop through options

    total_rows <- 0
    total_cols <- 0
    for (anonym_type in anonym_types) {
        for (data_type in data_types) {
            for (time_period in time_periods) {
                # define header for export file
                header <- paste0(
                    "\n",
                    "Output for REDX ",
                    config_globals()[["next_version"]],
                    "-",
                    anonym_type,
                    "-",
                    data_type,
                    "\n"
                )

                write(
                    header,
                    file = export_file,
                    append = TRUE
                )

                for (housing_types_label in housing_types_labels_extended) {
                    # skip absolute values for combined index because the
                    # combined index is only available as deviations
                    if (housing_types_label == "CombInd" & data_type == "ABS") {
                        next
                    }
                    
                    # define directory
                    directory <- file.path(
                        config_paths()[["output_path"]],
                        "export",
                        paste0(
                            "RWIGEOREDX_",
                            toupper(housing_types_label),
                            "_",
                            toupper(config_globals()[["next_version"]]),
                            "_",
                            toupper(anonym_type),
                            "_",
                            toupper(time_period),
                            "_",
                            toupper(data_type),
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

                    sheet_names <- sheet_names[
                        !grepl("Sheet 1", sheet_names)
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

                    # add to overall count
                    total_rows <- total_rows + nrows
                    total_cols <- total_cols + ncols

                    #--------------------------------------------------
                    # export findings

                    export_housing_type <- paste0(
                        "#-----------------------\n",
                        "Count for ",
                        housing_types_label,
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
        }
    }

    #--------------------------------------------------
    # add overall count

    export_overall_count <- paste0(
        "#-----------------------\n",
        "Overall count",
        "\nTotal number of rows: ",
        total_rows,
        "\nTotal number of columns: ",
        total_cols,
        "\n#-----------------------"
    )

    write(
        export_overall_count,
        file = export_file,
        append = TRUE
    )

    #--------------------------------------------------
    # return

    return(export_file)
}

