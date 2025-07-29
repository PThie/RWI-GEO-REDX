
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
    # define options

    anonym_types <- c("SUF", "PUF")

    housing_types_labels_extended <- c(
        housing_types_labels,
        "CombInd"
    )

    time_periods <- config_globals()[["time_periods"]]

    data_types <- c("ABS", "DEV_CROSS", "DEV_REGION")

    #--------------------------------------------------
    # open workbooks
    # reorder worksheets
    # save workbooks

    housing_types_labels_extended <- c(
        housing_types_labels,
        "CombInd"
    )

    for (time_period in time_periods) {
        for (data_type in data_types) {
            for (anonym_type in c("PUF", "SUF")) {
                for (housing_types_label in housing_types_labels_extended) {
                    #--------------------------------------------------
                    # skip absolute values for combined index because the
                    # combined index is only available as deviations
                    if (housing_types_label == "CombInd" & data_type == "ABS") {
                        next
                    }

                    #--------------------------------------------------
                    # define sheet order

                    if (housing_types_label == "CombInd") {
                        sheet_order <- c(
                            "Information",
                            paste0("Munic_RegionEff_devpc_", paste0(time_period, "ly")),
                            paste0("Distr_RegionEff_devpc_", paste0(time_period, "ly")),
                            paste0("LMR_RegionEff_devpc_", paste0(time_period, "ly"))
                        )
                    } else {
                        if (data_type == "ABS") {
                            sheet_order <- c(
                                "Information",
                                paste0("Munic_RegionEff_abs_", paste0(time_period, "ly")),
                                paste0("Distr_RegionEff_abs_", paste0(time_period, "ly")),
                                paste0("LMR_RegionEff_abs_", paste0(time_period, "ly"))
                            )
                        } else {
                            sheet_order <- c(
                                "Information",
                                paste0("Munic_RegionEff_dev_", paste0(time_period, "ly")),
                                paste0("Distr_RegionEff_dev_", paste0(time_period, "ly")),
                                paste0("LMR_RegionEff_dev_", paste0(time_period, "ly")),
                                paste0("Munic_RegionEff_devpc_", paste0(time_period, "ly")),
                                paste0("Distr_RegionEff_devpc_", paste0(time_period, "ly")),
                                paste0("LMR_RegionEff_devpc_", paste0(time_period, "ly"))
                            )
                        }
                    }

                    #--------------------------------------------------
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

                    # match sheet names with desired order
                    sheet_indices <- match(
                        sheet_order,
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
        }
    }

    #--------------------------------------------------
    # return
    
    return(workbook)
}