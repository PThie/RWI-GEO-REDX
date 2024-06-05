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
    # keep only time and effects

    results_list <- list()
    for (result in names(time_effects)) {
        if (result == "ejahr") {
            time_label <- "year"
        } else {
            time_label <- "quarter"
        }

        time_effects_prep <- time_effects[[result]] |>
            dplyr::select(
                !!rlang::sym(time_label),
                dplyr::contains("timeeff")
            )

        results_list[[result]] <- time_effects_prep
    }

    #--------------------------------------------------
    # export

    openxlsx::write.xlsx(
        list(
            "Grids_TimeEff_yearly" = results_list[["ejahr"]],
            "Grids_TimeEff_quarterly" = results_list[["e_year_quarter"]]
        ),
        file.path(
            config_paths()[["output_path"]],
            paste0(housing_type, "_rebuild"),
            paste0(
                "RWIGEOREDX_",
                housing_type_label,
                "_",
                config_globals()[["next_version"]],
                ".xlsx"
            )
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # return

    return(results_list)
}
