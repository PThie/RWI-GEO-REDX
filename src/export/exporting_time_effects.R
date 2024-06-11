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
        openxlsx::write.xlsx(
            list(
                "Grids_TimeEff_yearly" = results_list[["ejahr"]],
                "Grids_TimeEff_quarterly" = results_list[["e_year_quarter"]]
            ),
            file.path(
                config_paths()[["output_path"]],
                "Temp_Export",
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
            rowNames = FALSE
        )
    }

    #--------------------------------------------------
    # return

    return(results_list)
}
