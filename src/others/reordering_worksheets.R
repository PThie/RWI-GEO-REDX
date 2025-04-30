
reordering_worksheets <- function(
    housing_types_labels = NA,
    dependency = NA
) {
    #' @title Reordering worksheets in export workbooks
    #' 
    #' @description This function reorders the worksheets in the export workbooks.
    #' 
    #' @param housing_types_labels Character with housing types
    #' @param dependency Placeholder for target object that needs to be created
    #' before executing this function
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    #' 
    #' @note Technically, the function is returning the last handled workbook.
    #' However, this returned object is not used in the pipeline. It is only
    #' returned to establish dependencies in the next step.
    
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
    # open workbooks
    # reorder worksheets
    # save workbooks

    housing_types_labels_extended <- c(
        housing_types_labels,
        "CombInd",
        "GRIDS"
    )

    for (anonym_type in c("PUF", "SUF")) {
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

            # remove sheet 1
            # NOTE: gets created when generating an empty workbook
            if ("Sheet 1" %in% sheet_names) {
                openxlsx::removeWorksheet(
                    workbook,
                    sheet = "Sheet 1"
                )
            }

            # update sheet names (in case "Sheet 1" got removed)
            sheet_names <- names(workbook)

            # retrieve desired order
            # NOTE: GRIDS output is shorter and has fewer sheets
            if (data_type == "GRIDS") {
                desired_sheet_order <- config_globals()[["desired_sheet_order_grids"]]
            } else {
                desired_sheet_order <- config_globals()[["desired_sheet_order"]]
            }

            # match sheet names with desired order
            sheet_indices <- match(
                desired_sheet_order,
                sheet_names
            )

            # reorder worksheets
            openxlsx::worksheetOrder(workbook) <- sheet_indices

            # save workbook
            openxlsx::saveWorkbook(
                workbook,
                file = directory,
                overwrite = TRUE
            )
        }
    }

    #--------------------------------------------------
    # return
    
    return(workbook)
}