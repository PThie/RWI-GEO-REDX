copying_information_worksheet <- function(
    housing_types_labels = NA,
    dependencies = NA
) {
    #' @title Copying information worksheet
    #' 
    #' @description This function copies the information worksheet template
    #' to the final data outputs and updates the corresponding places according
    #' to the new version.
    #' 
    #' @param housing_types_labels Character with housing types
    #' @param dependencies List with dependencies. This should ensure that this
    #' step gets updated when previous steps change.
    #' 
    #' @return NULL, direct export
    #' @author Patrick Thiel
    #' 
    #' @note Technically, the function is returning the last handled workbook.
    #' However, this returned object is not used in the pipeline. It is only
    #' returned to establish dependencies in the next step.
    
    #--------------------------------------------------
    # check for dependencies

    for (dta in dependencies) {
        targets::tar_assert_nonempty(
            dta,
            msg = glue::glue(
                "!!! WARNING: ",
                "One of the dependencies is empty.",
                " (Error code: ciw#1)"
            )
        )
    }

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

    for (time_period in time_periods) {
        for (data_type in data_types) {
            for (anonym_type in anonym_types) {
                for (housing_types_label in housing_types_labels_extended) {
                    #--------------------------------------------------
                    # skip absolute values for combined index because the
                    # combined index is only available as deviations
                    if (housing_types_label == "CombInd" & data_type == "ABS") {
                        next
                    }

                    #--------------------------------------------------
                    # define full names for housing types
                    # and names for DOI
                    if (housing_types_label == "ApPurc") {
                        housing_type_full_name <- "Apartment Purchases"
                        doi_name <- "Apartments for Sale"
                        housing_type_short <- "WK"
                    } else if (housing_types_label == "HouPurc") {
                        housing_type_full_name <- "House Purchases"
                        doi_name <- "Houses for Sale"
                        housing_type_short <- "HK"
                    } else if (housing_types_label == "ApRent") {
                        housing_type_full_name <- "Apartment Rents"
                        doi_name <- "Apartments for Rent"
                        housing_type_short <- "WM"
                    } else {
                        housing_type_full_name <- "Combined Index"
                    }

                    #--------------------------------------------------
                    # define names for different measurements

                    if (data_type == "ABS") {
                        data_type_label <- "Absolute Values"
                    } else if (data_type == "DEV_CROSS") {
                        data_type_label <- "Cross-Sectional Deviations"
                    } else {
                        data_type_label <- "Regional Deviations"
                    }

                    #--------------------------------------------------
                    # read upcoming version workbook

                    # read workbook
                    workbook <- openxlsx::loadWorkbook(
                        file.path(
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
                    )

                    # only add if information is not present
                    if (!"Information" %in% names(workbook)) {
                        # add empty information sheet
                        openxlsx::addWorksheet(workbook, "Information")
                    }

                    # increase column width of label column
                    openxlsx::setColWidths(
                        workbook,
                        sheet = "Information",
                        cols = 2,
                        widths = 30
                    )

                    #--------------------------------------------------
                    # read template workbook

                    if (housing_types_label == "CombInd") {
                        data_type_abbr <- housing_types_label
                    } else {
                        if (data_type == "ABS") {
                            data_type_abbr <- "ABS"
                        } else {
                            data_type_abbr <- "DEV"
                        }
                    }

                    # read information sheet
                    information_sheet <- openxlsx::readWorkbook(
                        file.path(
                            # NOTE: Cannot use output_path because it references to the specific
                            # version
                            config_paths()[["main_path"]],
                            "output",
                            "information_templates",
                            paste0(
                                "information_template_",
                                toupper(data_type_abbr),
                                ".xlsx"
                            )
                        ),
                        skipEmptyRows = FALSE,
                        skipEmptyCols = FALSE,
                        sheet = "Information"
                    )

                    #--------------------------------------------------
                    # fill empty information worksheet with information from previous version

                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = information_sheet,
                        startRow = 2,
                        colNames = FALSE
                    )

                    #--------------------------------------------------
                    # add header with title

                    title <- paste(
                        "Regional Real Estate Price Indices for Germany (RWI-GEO-REDX)",
                        housing_type_full_name,
                        data_type_label,
                        anonym_type,
                        stringr::str_replace(
                            config_globals()[["next_version"]],
                            "v",
                            "Version "
                        ),
                        paste0(
                            config_globals()[["first_year"]],
                            "-",
                            as.numeric(config_globals()[["max_month"]]) - 1,
                            "/",
                            config_globals()[["max_year"]]
                        ),
                        sep = ", "
                    )

                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = title,
                        startCol = 2,
                        startRow = 1,
                        colNames = FALSE
                    )

                    # styling for the title
                    openxlsx::addStyle(
                        workbook,
                        sheet = "Information",
                        rows = 1,
                        cols = 2,
                        style = openxlsx::createStyle(
                            fontSize = 16,
                            textDecoration = "bold"
                        )
                    )

                    #--------------------------------------------------
                    # add subtitle

                    if (anonym_type == "SUF") {
                        subtitle <- "SCIENTIFIC USE FILE - NO FREE DISTRIBUTION - PERMISSION MUST BE GIVEN BY Research Data Center at the RWI!"
                        font_col <- "red"
                    } else {
                        subtitle <- "Public Use File (PUF) Version"
                        font_col <- "black"
                    }

                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = subtitle,
                        startCol = 2,
                        startRow = 2,
                        colNames = FALSE
                    )

                    # styling for the subtitle
                    openxlsx::addStyle(
                        workbook,
                        sheet = "Information",
                        rows = 2,
                        cols = 2,
                        style = openxlsx::createStyle(
                            fontSize = 16,
                            fontColour = font_col
                        )
                    )

                    #--------------------------------------------------
                    # add styling to the headings

                    # NOTE: exception for DEV because they list sheets
                    # Hence the headings are shifted.
                    if (housing_types_label == "CombInd") {
                        headings_pos <- c(4, 10, 21, 28, 33, 36)
                    } else {
                        if (data_type == "ABS") {
                            headings_pos <- c(4, 10, 21, 28, 31, 34)
                        } else {
                            headings_pos <- c(4, 13, 24, 31, 34, 37)
                        }
                    }

                    openxlsx::addStyle(
                        workbook,
                        sheet = "Information",
                        rows = headings_pos,
                        cols = 2,
                        style = openxlsx::createStyle(
                            fontSize = 14,
                            textDecoration = "bold"
                        )
                    )

                    #--------------------------------------------------
                    # adopt "table of contents" according new data version
                    # NOTE: text is automatically written row-wise starting in startRow

                    if (housing_types_label == "CombInd") {
                        # Lines for "Worksheet Title"
                        title_munic_regioneff_devpc <- paste0(
                                "Munic_RegionEff_devpc_",
                                time_period,
                                "ly"
                            )

                        title_distr_regioneff_devpc <- paste0(
                            "Distr_RegionEff_devpc_",
                            time_period,
                            "ly"
                        )

                        title_lmr_regioneff_devpc <- paste0(
                            "LMR_RegionEff_devpc_",
                            time_period,
                            "ly"
                        )

                        # Lines for "Contents"
                        text_munic_regioneff_devpc <- paste0(
                            "Regional price index on municipality level, 2008Q1-",
                            config_globals()[["max_year"]],
                            "Q",
                            config_globals()[["max_quarter"]],
                            " (deviations in percent)"
                        )

                        text_distr_regioneff_devpc <- paste0(
                            "Regional price index on district level, 2008Q1-",
                            config_globals()[["max_year"]],
                            "Q",
                            config_globals()[["max_quarter"]],
                            " (deviations in percent)"
                        )

                        text_lmr_regioneff_devpc <- paste0(
                            "Regional price index on labor market region (LMR), 2008Q1-",
                            config_globals()[["max_year"]],
                            "Q",
                            config_globals()[["max_quarter"]],
                            " (deviations in percent)"
                        )

                        # combine everything
                        table_of_content_list <- c(
                            text_munic_regioneff_devpc,
                            text_distr_regioneff_devpc,
                            text_lmr_regioneff_devpc
                        )

                        titles_list <- c(
                            title_munic_regioneff_devpc,
                            title_distr_regioneff_devpc,
                            title_lmr_regioneff_devpc
                        )
                    } else {
                        if (data_type == "ABS") {
                            # Lines for "Worksheet Title"
                            title_munic_regioneff <- paste0(
                                "Munic_RegionEff_",
                                tolower(data_type),
                                "_",
                                time_period,
                                "ly"
                            )

                            title_distr_regioneff <- paste0(
                                "Distr_RegionEff_",
                                tolower(data_type),
                                "_",
                                time_period,
                                "ly"
                            )

                            title_lmr_regioneff <- paste0(
                                "LMR_RegionEff_",
                                tolower(data_type),
                                "_",
                                time_period,
                                "ly"
                            )
                            
                            if (time_period == "year") {
                                # Lines for "Contents"
                                text_munic_regioneff <- paste0(
                                    "Regional price index on municipality level, 2008-",
                                    config_globals()[["max_year"]],
                                    " (absolute values)"
                                )

                                text_distr_regioneff <- paste0(
                                    "Regional price index on district level, 2008-",
                                    config_globals()[["max_year"]],
                                    " (absolute values)"
                                )

                                text_lmr_regioneff <- paste0(
                                    "Regional price index on labor market region (LMR), 2008-",
                                    config_globals()[["max_year"]],
                                    " (absolute values)"
                                )
                            } else {
                                # Lines for "Contents"
                                text_munic_regioneff <- paste0(
                                    "Regional price index on municipality level, 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (absolute values)"
                                )

                                text_distr_regioneff <- paste0(
                                    "Regional price index on district level, 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (absolute values)"
                                )

                                text_lmr_regioneff <- paste0(
                                    "Regional price index on labor market region (LMR), 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (absolute values)"
                                )
                            }
                        } else {
                            # Line for "Worksheet Title"
                            title_munic_regioneff_dev <- paste0(
                                "Munic_RegionEff_dev_",
                                time_period,
                                "ly"
                            )

                            title_distr_regioneff_dev <- paste0(
                                "Distr_RegionEff_dev_",
                                time_period,
                                "ly"
                            )

                            title_lmr_regioneff_dev <- paste0(
                                "LMR_RegionEff_dev_",
                                time_period,
                                "ly"
                            )

                            title_munic_regioneff_devpc <- paste0(
                                "Munic_RegionEff_devpc_",
                                time_period,
                                "ly"
                            )

                            title_distr_regioneff_devpc <- paste0(
                                "Distr_RegionEff_devpc_",
                                time_period,
                                "ly"
                            )

                            title_lmr_regioneff_devpc <- paste0(
                                "LMR_RegionEff_devpc_",
                                time_period,
                                "ly"
                            )

                            if (time_period == "year") {
                                # Lines for "Contents"
                                text_munic_regioneff_dev <- paste0(
                                    "Regional price index on municipality level, 2008-",
                                    config_globals()[["max_year"]],
                                    " (deviations in absolute values)"
                                )

                                text_distr_regioneff_dev <- paste0(
                                    "Regional price index on district level, 2008-",
                                    config_globals()[["max_year"]],
                                    " (deviations in absolute values)"
                                )

                                text_lmr_regioneff_dev <- paste0(
                                    "Regional price index on labor market region (LMR), 2008-",
                                    config_globals()[["max_year"]],
                                    " (deviations in absolute values)"
                                )

                                text_munic_regioneff_devpc <- paste0(
                                    "Regional price index on municipality level, 2008-",
                                    config_globals()[["max_year"]],
                                    " (deviations in percent)"
                                )

                                text_distr_regioneff_devpc <- paste0(
                                    "Regional price index on district level, 2008-",
                                    config_globals()[["max_year"]],
                                    " (deviations in percent)"
                                )

                                text_lmr_regioneff_devpc <- paste0(
                                    "Regional price index on labor market region (LMR), 2008-",
                                    config_globals()[["max_year"]],
                                    " (deviations in percent)"
                                )
                            } else {
                                # Lines for "Contents"
                                text_munic_regioneff_dev <- paste0(
                                    "Regional price index on municipality level, 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (deviations in absolute values)"
                                )

                                text_distr_regioneff_dev <- paste0(
                                    "Regional price index on district level, 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (deviations in absolute values)"
                                )

                                text_lmr_regioneff_dev <- paste0(
                                    "Regional price index on labor market region (LMR), 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (deviations in absolute values)"
                                )

                                # Lines for "Contents"
                                text_munic_regioneff_devpc <- paste0(
                                    "Regional price index on municipality level, 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (deviations in percent)"
                                )

                                text_distr_regioneff_devpc <- paste0(
                                    "Regional price index on district level, 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (deviations in percent)"
                                )

                                text_lmr_regioneff_devpc <- paste0(
                                    "Regional price index on labor market region (LMR), 2008Q1-",
                                    config_globals()[["max_year"]],
                                    "Q",
                                    config_globals()[["max_quarter"]],
                                    " (deviations in percent)"
                                )
                            }
                        }
                    
                        # NOTE: Absolute values have a different number of sheets,
                        # so the table of content varies
                        if (data_type == "ABS") {
                            table_of_content_list <- c(
                                text_munic_regioneff,
                                text_distr_regioneff,
                                text_lmr_regioneff
                            )

                            titles_list <- c(
                                title_munic_regioneff,
                                title_distr_regioneff,
                                title_lmr_regioneff
                            )
                        } else {
                            table_of_content_list <- c(
                                text_munic_regioneff_dev,
                                text_distr_regioneff_dev,
                                text_lmr_regioneff_dev,
                                text_munic_regioneff_devpc,
                                text_distr_regioneff_devpc,
                                text_lmr_regioneff_devpc
                            )

                            titles_list <- c(
                                title_munic_regioneff_dev,
                                title_distr_regioneff_dev,
                                title_lmr_regioneff_dev,
                                title_munic_regioneff_devpc,
                                title_distr_regioneff_devpc,
                                title_lmr_regioneff_devpc
                            )
                        }
                    }

                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = titles_list,
                        startCol = 2,
                        startRow = 6,
                        colNames = FALSE
                    )

                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = table_of_content_list,
                        startCol = 3,
                        startRow = 6,
                        colNames = FALSE
                    )

                    #--------------------------------------------------
                    # style for "tables" (table of contents and variable explanation)

                    if (housing_types_label == "CombInd") {
                        second_table_pos <- 11
                    } else {
                        if (data_type == "ABS") {
                            second_table_pos <- 11
                        } else {
                            second_table_pos <- 14
                        }
                    }

                    openxlsx::addStyle(
                        workbook,
                        sheet = "Information",
                        rows = c(5, second_table_pos),
                        cols = c(2, 3),
                        style = openxlsx::createStyle(
                            fontSize = 11,
                            textDecoration = "bold"
                        ),
                        gridExpand = TRUE
                    )

                    #--------------------------------------------------
                    # adopt publications

                    data_report_red <- paste0(
                        "Data report RWI-GEO-RED: Thiel (",
                        config_globals()[["red_data_report_year"]],
                        "): FDZ Data description: Real Estate Data for Germany (RWI-GEO-RED ",
                        config_globals()[["red_version"]],
                        ") - Advertisements on the Internet Platform ImmobilienScout24 2007-",
                        config_globals()[["max_month"]],
                        "/",
                        config_globals()[["max_year"]]
                    )

                    data_report_redx <- paste0(
                        "Data report RWI-GEO-REDX: Thiel (",
                        config_globals()[["redx_data_report_year"]],
                        "): FDZ Data Description: Regional Real Estate Price Index for Germany, 2008-",
                        as.numeric(config_globals()[["max_month"]]) - 1,
                        "/",
                        config_globals()[["max_year"]],
                        " (",
                        config_globals()[["next_version"]],
                        "), RWI Projektberichte, Essen."
                    )

                    # define position
                    if (housing_types_label == "CombInd") {
                        publications_pos <- 25
                    } else {
                        if (data_type == "ABS") {
                            publications_pos <- 25
                        } else {
                            publications_pos <- 28
                        }
                    }

                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = c(
                            data_report_red,
                            data_report_redx
                        ),
                        startCol = 2,
                        startRow = publications_pos,
                        colNames = FALSE
                    )

                    #--------------------------------------------------
                    # adopt data sources
                    # NOTE: CombInd list all housing types as data sources
                    # as they use everything

                    if (housing_types_label == "CombInd") {
                        data_source_red <- c(
                            paste0(
                                "RWI-GEO-RED: RWI Real Estate Data - Houses for Sale. DOI: 10.7807/immo:red:hk:",
                                config_globals()[["red_version"]]
                            ),
                            paste0(
                                "RWI-GEO-RED: RWI Real Estate Data - Apartments for Sale. DOI: 10.7807/immo:red:wk:",
                                config_globals()[["red_version"]]
                            ),
                            paste0(
                                "RWI-GEO-RED: RWI Real Estate Data - Apartments for Rent. DOI: 10.7807/immo:red:wm:",
                                config_globals()[["red_version"]]
                            )
                        )
                    } else {
                        data_source_red <- paste0(
                            "RWI-GEO-RED: RWI Real Estate Data - ",
                            doi_name,
                            ". DOI: 10.7807/immo:red:",
                            tolower(housing_type_short),
                            ":",
                            config_globals()[["red_version"]]
                        )
                    }

                    # define position
                    if (housing_types_label == "CombInd") {
                        data_source_pos <- 29
                    } else {
                        if (data_type == "ABS") {
                            data_source_pos <- 29
                        } else {
                            data_source_pos <- 32
                        }
                    }
                    
                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = data_source_red,
                        startCol = 2,
                        startRow = data_source_pos,
                        colNames = FALSE
                    )

                    #--------------------------------------------------
                    # adopt data generating process

                    data_generating_process <- paste0(
                        "Repository to codebase ",
                        config_globals()[["next_version"]],
                        ": ",
                        config_globals()[["zenodo_repo_doi"]]
                    )

                    # define positon
                    # NOTE: varies because CombInd list all housing types as
                    # data source
                    if (housing_types_label == "CombInd") {
                        dgp_pos <- 34
                    } else {
                        if (data_type == "ABS") {
                            dgp_pos <- 32
                        } else {
                            dgp_pos <- 35
                        }
                    }

                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = data_generating_process,
                        startCol = 2,
                        startRow = dgp_pos,
                        colNames = FALSE
                    )

                    #--------------------------------------------------
                    # adopt citation

                    citation <- paste0(
                        "Thiel, RWI, and ImmobilienScout24 (",
                        config_globals()[["redx_data_publication_year"]],
                        "): RWI-GEO-REDX: Regional Real Estate Price Index for Germany, ",
                        anonym_type,
                        ", 2008-",
                        as.numeric(config_globals()[["max_month"]]) - 1,
                        "/",
                        config_globals()[["max_year"]],
                        " (",
                        config_globals()[["next_version"]],
                        "). Version: 1. RWI-Leibniz Institute for Economic Research. Dataset. http://doi.org/10.7807/immo:redx:",
                        tolower(anonym_type),
                        ":",
                        config_globals()[["next_version"]]
                    )

                    # define positon
                    # NOTE: varies because GRIDS and CombInd list all housing types as
                    # data source
                    if (housing_types_label == "CombInd") {
                        cit_pos <- 37
                    } else {
                        if (data_type == "ABS") {
                            cit_pos <- 35
                        } else {
                            cit_pos <- 38
                        }
                    }

                    openxlsx::writeData(
                        workbook,
                        sheet = "Information",
                        x = citation,
                        startCol = 2,
                        startRow = cit_pos,
                        colNames = FALSE
                    )

                    #--------------------------------------------------
                    # export workbook with information sheet

                    openxlsx::saveWorkbook(
                        workbook,
                        file = file.path(
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
                        ),
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