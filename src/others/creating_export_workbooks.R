creating_export_workbooks <- function() {
    #' @title Create empty export workbooks
    #' 
    #' @description This function creates empty workbooks for the export of the
    #' REDGEOREDX data which is the final data which goes to the user.
    #' 
    #' @return NULL
    #' @author Patrick Thiel

    #--------------------------------------------------
    # create empty workbooks
    # needed for the export of the REDGEOREDX data

    for (anonym_type in c("PUF", "SUF")) {
        for (data_type in c("ApPurc", "ApRent", "CombInd", "GRIDS", "HouPurc")) {
            # create empty workbook
            empty_workbook <- openxlsx::createWorkbook()

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

            if (!file.exists(directory)) {
                openxlsx::saveWorkbook(
                    empty_workbook,
                    file = directory
                )
            }
        }
    }

    #--------------------------------------------------
    # return

    return(NULL)
}