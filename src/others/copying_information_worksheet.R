copying_information_worksheet <- function(
    housing_types_labels = NA
) {
    #' @title Copying information worksheet
    #' 
    #' @description This function copies the information worksheet template
    #' to the final data outputs and updates the corresponding places according
    #' to the new version.
    #' 
    #' @param housing_types_labels Character with housing types
    #' 
    #' @return NULL, direct export
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # define options

    anonym_types <- c("SUF", "PUF")

    housing_types_labels_extended <- c(
        housing_types_labels,
        "CombInd",
        "GRIDS"
    )

    #--------------------------------------------------
    # loop through options
    for (anonym_type in anonym_types) {
        for (housing_types_label in housing_types_labels_extended) {
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
            } else if (housing_types_label == "CombInd") {
                housing_type_full_name <- "Combined Index"
            } else {
                housing_type_full_name <- "All Housing Types (Grid Level)"
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
                        housing_types_label,
                        "_",
                        config_globals()[["next_version"]],
                        "_",
                        anonym_type,
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

            if (housing_types_label == "GRIDS") {
                template_suffix <- "grids"
            } else if (housing_types_label == "CombInd") {
                template_suffix <- "combind"
            } else {
                template_suffix <- "nongrids"
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
                        template_suffix,
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

            # NOTE: exception for GRIDS and CombInd because they list more data
            # sources (all housing types). Hence the headings are shifted.
            if (housing_types_label == "GRIDS") {
                headings_pos <- c(4, 11, 23, 29, 34, 37)
            } else if (housing_types_label == "CombInd") {
                headings_pos <- c(4, 15, 28, 35, 40, 43)
            } else {
                headings_pos <- c(4, 15, 28, 35, 38, 41)
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

            text_grids_timeeff_yearly <- paste0(
                "Annual time effects on grid level, 2008-",
                config_globals()[["max_year"]],
                " (regression 1)"
            )

            text_grids_timeeff_quarterly <- paste0(
                "Quaterly time effects on grid level, 2008Q1-",
                config_globals()[["max_year"]],
                "Q",
                config_globals()[["max_quarter"]],
                " (regression 1)"
            )

            text_munic_regioneff_yearly <- paste0(
                "Regional price index on municipality level, 2008-",
                config_globals()[["max_year"]],
                " (regression 2)"
            )

            text_distr_regioneff_yearly <- paste0(
                "Regional price index on district level, 2008-",
                config_globals()[["max_year"]],
                " (regression 2)"
            )

            text_lmr_regioneff_yearly <- paste0(
                "Regional price index on labor market region (LMR), 2008-",
                config_globals()[["max_year"]],
                " (regression 2)"
            )

            text_munic_regioneff_change_yearly <- paste0(
                "Change of regional price indices on municipality level to 2008, 2008-",
                config_globals()[["max_year"]],
                " (regression 3)"
            )

            text_distr_regioneff_change_yearly <- paste0(
                "Change of regional price indices on district level to 2008, 2008-",
                config_globals()[["max_year"]],
                " (regression 3)"
            )

            text_lmr_regioneff_change_yearly <- paste0(
                "Change of regional price indices on labor market region (LMR) to 2008, 2008-",
                config_globals()[["max_year"]],
                " (regression 3)"
            )

            text_grids_regioneff_yearly <- paste0(
                "Regional price index on grid level, 2008-",
                config_globals()[["max_year"]],
                " (regression 2)"
            )

            text_grids_regioneff_change_yearly <- paste0(
                "Change of regional price indices on grid level to 2008, 2008-",
                config_globals()[["max_year"]],
                " (regression 3)"
            )

            # NOTE: GRIDS have a different number of sheets, so the table of
            # content varies
            if (housing_types_label == "GRIDS") {
                table_of_content_list <- c(
                    text_grids_timeeff_yearly,
                    text_grids_timeeff_quarterly,
                    text_grids_regioneff_yearly,
                    text_grids_regioneff_change_yearly
                )
            } else {
                table_of_content_list <- c(
                    text_grids_timeeff_yearly,
                    text_grids_timeeff_quarterly,
                    text_munic_regioneff_yearly,
                    text_distr_regioneff_yearly,
                    text_lmr_regioneff_yearly,
                    text_munic_regioneff_change_yearly,
                    text_distr_regioneff_change_yearly,
                    text_lmr_regioneff_change_yearly
                )
            }

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

            if (housing_types_label == "GRIDS") {
                second_table_pos <- 12
            } else {
                second_table_pos <- 16
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
                config_globals()[["max_year"]],
                "): FDZ Data description: Real Estate Data for Germany (RWI-GEO-RED ",
                config_globals()[["red_version"]],
                ") - Advertisements on the Internet Platform ImmobilienScout24 2007-",
                config_globals()[["max_month"]],
                "/",
                config_globals()[["max_year"]]
            )

            data_report_redx <- paste0(
                "Data report RWI-GEO-REDX: Thiel (",
                config_globals()[["max_year"]],
                "): FDZ Data Description: Regional Real Estate Price Index for Germany, 2008-",
                as.numeric(config_globals()[["max_month"]]) - 1,
                "/",
                config_globals()[["max_year"]],
                " (",
                config_globals()[["next_version"]],
                "), RWI Projektberichte, Essen."
            )

            # define position
            if (housing_types_label == "GRIDS") {
                publications_pos <- 26
            } else {
                publications_pos <- 32
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
            # NOTE: GRIDS and CombInd list all housing types as data sources
            # as they use everything

            if (housing_types_label == "GRIDS" | housing_types_label == "CombInd") {
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
            if (housing_types_label == "GRIDS") {
                data_source_pos <- 30
            } else {
                data_source_pos <- 36
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
            # NOTE: varies because GRIDS and CombInd list all housing types as
            # data source
            if (housing_types_label == "GRIDS") {
                dgp_pos <- 35
            } else if (housing_types_label == "CombInd") {
                dgp_pos <- 41
            } else {
                dgp_pos <- 39
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
                config_globals()[["max_year"]],
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
            if (housing_types_label == "GRIDS") {
                cit_pos <- 38
            } else if (housing_types_label == "CombInd") {
                cit_pos <- 44
            } else {
                cit_pos <- 42
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
                        housing_types_label,
                        "_",
                        config_globals()[["next_version"]],
                        "_",
                        anonym_type,
                        ".xlsx"
                    )
                ),
                overwrite = TRUE
            )
        }
    }

    #--------------------------------------------------
    # return

    return(NULL)
}