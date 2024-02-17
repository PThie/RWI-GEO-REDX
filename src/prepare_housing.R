prepare_housing <- function(housing_file = NA, data_type = NA) {
    #' @title Prepare housing data
    #' 
    #' @description 
    #' 
    #' @param housing_file Name of the original housing data file
    #' 
    #' @return
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # load original data

    # TODO: replace housing file name
    org_data <- haven::read_dta(
        file.path(
            config_paths()[["org_data_path"]],
            "On-site",
            config_globals()[["red_version"]],
            # NOTE: uncomment later for loop
            paste0(housing_file, ".dta")
            #"WK_allVersions_ohneText.dta"
        )
    )

    #----------------------------------------------
    # delete unnecessary string variables
    
    string_vars <- c("freiab", "courtage", "mietekaution")
    for (col in string_vars) {
        if (col %in% names(org_data)) {
            org_data[[col]] <- NULL
        }
    }

    #----------------------------------------------
    # general cleaning (identical for all types)

    org_data <- org_data |>
        dplyr::mutate(
            # recode missing values in living space
            wohnflaeche = dplyr::case_when(
                wohnflaeche < 0 ~ 0,
                TRUE ~ wohnflaeche
            )
        )

    #----------------------------------------------
    # type specific cleaning

    # Apartment sales
    if (data_type == "WK") {
        org_data <- org_data |>
            dplyr::mutate(
                # replace missings in price with 0
                kaufpreis = dplyr::case_when(
                    kaufpreis < 0 ~ 0,
                    TRUE ~ kaufpreis
                ),
                # add logarithmic price
                ln_flatprice = log(kaufpreis),
                # add flat price per sqm
                flatprice_sqmeter = kaufpreis / wohnflaeche
            )
    # Apartment rentals
    } else if (data_type == "WM") {
        org_data <- org_data |>
            dplyr::mutate(
                # replace missings in rent with 0
                mietekalt = dplyr::case_when(
                    mietekalt < 0 ~ 0,
                    TRUE ~ mietekalt
                ),
                # add logarithmic rent
                ln_rent = log(mietekalt),
                # add rent per sqm
                rent_sqmeter = mietekalt / wohnflaeche
            )
    # House sales
    } else {
        org_data <- org_data |>
            dplyr::mutate(
                # replace missings in price with 0
                kaufpreis = dplyr::case_when(
                    kaufpreis < 0 ~ 0,
                    TRUE ~ kaufpreis
                ),
                # add logarithmic price
                ln_houseprice = log(kaufpreis),
                # add house price per sqm
                houseprice_sqmeter = kaufpreis / wohnflaeche
            )
    }

    #----------------------------------------------
    # return

    return(org_data)
}