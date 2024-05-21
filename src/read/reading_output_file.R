reading_output_file <- function(
    housing_type_label = NA,
    sheet_names = NA,
    time_effects = NA,
    region_effects = NA
) {
    #--------------------------------------------------
    # define path to output data

    output_file_path <- file.path(
        config_paths()[["data_path"]],
        paste0("Version_", substring(config_globals()[["current_version"]], 2, 3)),
        "SUF",
        paste0(
            "RWIGEOREDX_",
            housing_type_label,
            "_",
            config_globals()[["current_version"]],
            "_SUF.xlsx"
        )
    )

    #--------------------------------------------------

    data_storage <- list()
    for (sheet_name in sheet_names) {
        #--------------------------------------------------
        # read data

        dta <- openxlsx::read.xlsx(
            output_file_path,
            sheet = sheet_name
        )

        #--------------------------------------------------
        # clean data

        if (grepl("TimeEff_yearly", sheet_name)) {
            dta_prep <- dta |>
                dplyr::mutate(
                    dplyr::across(
                        .cols = dplyr::everything(),
                        ~ as.numeric(.x)
                    )
                )

            # extract newly estimated year effects
            year_effects <- time_effects[["ejahr"]] |>
                dplyr::select(-c("nobs", dplyr::contains("mean"))) |>
                dplyr::rename(
                    new_timeeff = 2,
                    new_timeeff_p025 = 3,
                    new_timeeff_p975 = 4
                )

            # merge with old effects
            dta_merged <- merge(
                dta_prep,
                year_effects,
                by = "year"
            )

        } else if (grepl("TimeEff_quarterly", sheet_name)) {
            dta_prep <- dta |>
                dplyr::mutate(
                    quarter = as.Date(quarter, origin = "1899-12-30"),
                    dplyr::across(
                        .cols = -quarter,
                        ~ as.numeric(.x)
                    ),
                    # modify quarter to take the form "YYYY-01"
                    quarter = as.character(zoo::as.yearqtr(quarter, format = "%Y.%Q")),
                    quarter = stringr::str_replace_all(quarter, " Q", "-0")
                )

            # extract newly estimated quarter effects
            quarter_effects <- time_effects[["e_year_quarter"]] |>
                dplyr::select(-c("nobs", dplyr::contains("mean"))) |>
                dplyr::rename(
                    new_timeeff = 2,
                    new_timeeff_p025 = 3,
                    new_timeeff_p975 = 4
                )

            # merge with old effects
            dta_merged <- merge(
                dta_prep,
                quarter_effects,
                by = "quarter"
            )

        } else if (grepl("District_Pindex", sheet_name)){
            dta_prep <- dta |>
                dplyr::select(
                    1,
                    dplyr::contains("pindex")
                ) |>
                dplyr::mutate(
                    dplyr::across(
                        .cols = dplyr::contains("pindex"),
                        ~ as.numeric(.x)
                    )
                ) |>
                tidyr::pivot_longer(
                    cols = -1,
                    names_to = "year",
                    values_to = "pindex"
                ) |>
                dplyr::mutate(
                    year = stringr::str_replace_all(year, "pindex", "")
                ) |>
                dplyr::rename(kid2019 = AGS_District)

            # extract newly estimated regional effects
            district_effects <- region_effects[["district_year"]] |>
                dplyr::select(-nobs_district) |>
                dplyr::rename(new_pindex = weighted_pindex)

            # merge with old effects
            dta_merged <- merge(
                dta_prep,
                district_effects,
                by = c("kid2019", "year")
            )

        } else {
            dta_prep <- dta |>
                dplyr::select(
                    1,
                    dplyr::contains("pindex")
                ) |>
                dplyr::mutate(
                    dplyr::across(
                        .cols = dplyr::contains("pindex"),
                        ~ as.numeric(.x)
                    )
                ) |>
                tidyr::pivot_longer(
                    cols = -1,
                    names_to = "year",
                    values_to = "pindex"
                ) |>
                dplyr::mutate(
                    year = stringr::str_replace_all(year, "pindex", "")
                ) |>
                dplyr::rename(gid2019 = AGS_Municip)

            # extract newly estimated regional effects
            munic_effects <- region_effects[["municipality_year"]] |>
                dplyr::select(-nobs_munic) |>
                dplyr::rename(new_pindex = weighted_pindex)

            # merge with old effects
            dta_merged <- merge(
                dta_prep,
                munic_effects,
                by = c("gid2019", "year")
            )
        }

        data_storage[[sheet_name]] <- dta_merged
    }

    #--------------------------------------------------
    # return

    return(data_storage)
}