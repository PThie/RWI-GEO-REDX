exporting_time_effects <- function(
    time_effects = NA,
    housing_type = NA,
    housing_type_label = NA
) {
    #' @title Exporting time effects

    #' @param time_effects List with estimated time effects
    #' @param housing_type Housing type
    #' @param housing_type_label Housing type labels
    #' 
    #' @return List with exported time effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # prepare data

    results_list <- helpers_preparing_time_effects_export(
        time_effects_data = time_effects
    )

    #--------------------------------------------------
    # export

    for (file_type in c("PUF", "SUF")) {
        # load workbook
        complete_wb <- openxlsx::loadWorkbook(
            file.path(
                config_paths()[["output_path"]],
                "export",
                paste0(
                    "RWIGEOREDX_",
                    housing_type_label,
                    "_",
                    config_globals()[["next_version"]],
                    "_",
                    file_type,
                    ".xlsx"
                )
            )
        )

        # sheet names
        year_name <- "Grids_TimeEff_yearly"
        quarter_name <- "Grids_TimeEff_quarterly"

        # add sheet if not existent
        if (!year_name %in% names(complete_wb)) {
            openxlsx::addWorksheet(
                complete_wb,
                year_name
            )
        }

        if (!quarter_name %in% names(complete_wb)) {
            openxlsx::addWorksheet(
                complete_wb,
                quarter_name
            )
        }

        # store data
        openxlsx::writeData(
            wb = complete_wb,
            sheet = year_name,
            x = results_list[["ejahr"]]
        )
        openxlsx::writeData(
            wb = complete_wb,
            sheet = quarter_name,
            x = results_list[["e_year_quarter"]]
        )

        # save workbook
        openxlsx::saveWorkbook(
            complete_wb,
            file = file.path(
                config_paths()[["output_path"]],
                "export",
                paste0(
                    "RWIGEOREDX_",
                    housing_type_label,
                    "_",
                    config_globals()[["next_version"]],
                    "_",
                    file_type,
                    ".xlsx"
                )
            ),
            overwrite = TRUE
        )
    }

    #--------------------------------------------------
    # return

    return(results_list)
}
