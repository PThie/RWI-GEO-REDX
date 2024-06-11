exporting_time_effects_grids <- function(
    HK_estimated_time_effects = NA,
    WK_estimated_time_effects = NA,
    WM_estimated_time_effects = NA,
    combined_time_effects = NA
) {
    #' @title Exporting time effects for grids
    #' 
    #' @description This function exports the time fixed effects for the different
    #' housing types and the combined index at the grid level.
    #' 
    #' @param HK_estimated_time_effects Data frame with estimated time effects for
    #' housing type HK
    #' @param WK_estimated_time_effects Data frame with estimated time effects for
    #' housing type WK
    #' @param WM_estimated_time_effects Data frame with estimated time effects for
    #' housing type WM
    #' @param combined_time_effects Data frame with estimated time effects for the
    #' combined index
    #' 
    #' @return List with exported time effects
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # pool data together

    data_list <- list(
        HK = HK_estimated_time_effects,
        WK = WK_estimated_time_effects,
        WM = WM_estimated_time_effects,
        CI = combined_time_effects
    )

    #--------------------------------------------------
    # prepare data for export

    data_list <- lapply(data_list, helpers_preparing_time_effects_export)

    #--------------------------------------------------
    # merge data from different housing types in one data frame (per time period)

    results_list <- list()
    for (time in c("ejahr", "e_year_quarter")) {
        if (time == "ejahr") {
            time_label <- "year"
        } else {
            time_label <- "quarter"
        }

        # select all time period corresponding effects
        all_effects <- rbind(
            data_list[["HK"]][[time]] |>
                dplyr::mutate(
                    housing_type = "HK"
                ),
            data_list[["WK"]][[time]] |>
                dplyr::mutate(
                    housing_type = "WK"
                ),
            data_list[["WM"]][[time]] |>
                dplyr::mutate(
                    housing_type = "WM"
                ),
            data_list[["CI"]][[time]] |>
                dplyr::mutate(
                    housing_type = "CI"
                )
        )

        # reorder columns
        all_effects <- all_effects |>
            dplyr::relocate(housing_type, .after = time_label)

        results_list[[time_label]] <- all_effects
    }

    #--------------------------------------------------
    # export

    # create empty workbook
    complete_wb <- openxlsx::createWorkbook()

    # sheet names
    year_name <- "Grids_TimeEff_yearly"
    quarter_name <- "Grids_TimeEff_quarterly"

    # create sheets for each time period
    openxlsx::addWorksheet(
        complete_wb,
        year_name
    )
    openxlsx::addWorksheet(
        complete_wb,
        quarter_name
    )

    # store data
    openxlsx::writeData(
        wb = complete_wb,
        sheet = year_name,
        x = results_list[["year"]]
    )
    openxlsx::writeData(
        wb = complete_wb,
        sheet = quarter_name,
        x = results_list[["quarter"]]
    )

    # save workbook
    # TODO: change output folder (shouldn't be Temp_Export)

    for (file_type in c("PUF", "SUF")) {
        openxlsx::saveWorkbook(
            complete_wb,
            file = file.path(
                config_paths()[["output_path"]],
                "Temp_Export",
                paste0(
                    "RWIGEOREDX_",
                    "GRIDS_",
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
