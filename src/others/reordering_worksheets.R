
reordering_worksheets <- function() {
    #' @title Reordering worksheets in export workbooks
    #' 
    #' @description This function reorders the worksheets in the export workbooks.
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # open workbooks
    # reorder worksheets
    # save workbooks

    for (anonym_type in c("PUF", "SUF")) {
        for (data_type in c("ApPurc", "ApRent", "CombInd", "GRIDS", "HouPurc")) {
            print(anonym_type)
            print(data_type)
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
    
    return(NULL)
}