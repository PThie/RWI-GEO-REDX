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

    ##### DELETE LATER
    # Probably not needed because why would you rename the variable?
    # org_data <- org_data |>
    #     dplyr::mutate(
            # ags2019 = gid2019
    # )
    #####

    org_data <- org_data |>
        dplyr::mutate(
            # recode missing values in living space
            wohnflaeche = dplyr::case_when(
                wohnflaeche < 0 ~ 0,
                TRUE ~ wohnflaeche
            ),
            # add categories for construction year
            construction_year_cat = dplyr::case_when(
                baujahr == -5 | baujahr == -9 ~ 1,
                baujahr > 0 & baujahr < 1900 ~ 2,
                baujahr >= 1900 & baujahr < 1945 ~ 3,
                baujahr >= 1945 & baujahr < 1960 ~ 4,
                baujahr >= 1960 & baujahr < 1970 ~ 5,
                baujahr >= 1970 & baujahr < 1980 ~ 6,
                baujahr >= 1980 & baujahr < 1990 ~ 7,
                baujahr >= 1990 & baujahr < 2000 ~ 8,
                baujahr >= 2000 & baujahr < 2010 ~ 9,
                baujahr >= 2010 ~ 10,
            ),
            # define first occupation
            first_occupancy = dplyr::case_when(
                objektzustand == 1 ~ 1,
                TRUE ~ 0
            )
        )

    #----------------------------------------------
    # type specific cleaning

    # Apartment sales
    if (data_type == "WK") {
        org_data <- org_data |>
            dplyr::mutate(
                #----------------------------------------------
                # replace missings in price with 0
                kaufpreis = dplyr::case_when(
                    kaufpreis < 0 ~ 0,
                    TRUE ~ kaufpreis
                ),
                # add logarithmic price
                ln_flatprice = log(kaufpreis),
                # add flat price per sqm
                flatprice_sqmeter = kaufpreis / wohnflaeche,
                #----------------------------------------------
                # dummy for supported living
                betreut = dplyr::case_when(
                    betreut < 0 ~ 0,
                    TRUE ~ betreut
                ),
                #----------------------------------------------
                # dummy for housing benefit
                # To evaluate the transparency of the listing
                # TODO: change variable name to english
                declared_wohngeld = dplyr::case_when(
                    wohngeld > 0 & wohngeld < 2500 ~ 1,
                    TRUE ~ 0 
                ),
                #----------------------------------------------
                # number of floors (categorized)
                num_floors_cat = dplyr::case_when(
                    anzahletagen <= 0 ~ 1,
                    anzahletagen >= 1 & anzahletagen <= 3 ~ 2,
                    anzahletagen >= 4 & anzahletagen <= 5 ~ 3,
                    anzahletagen >= 6 & anzahletagen <= 10 ~ 4,
                    anzahletagen > 10 ~ 5
                ),
                #----------------------------------------------
                # floor of the flat (categorized)
                floors_cat = dplyr::case_when(
                    etage < 0 ~ 0,
                    etage == 0 ~ 1,
                    etage == 1 ~ 2,
                    etage == 2 | etage == 3 ~ 3,
                    etage == 4 | etage == 5 ~ 4,
                    etage >= 6 | etage <= 10 ~ 5,
                    etage > 10 ~ 6
                ),
                #----------------------------------------------
                # exception of Penthouse living also in multistory building
                penthouse = dplyr::case_when(
                    kategorie_Wohnung == 7 ~ 1,
                    TRUE ~ 0
                )
            )
    # Apartment rentals
    } else if (data_type == "WM") {
        org_data <- org_data |>
            dplyr::mutate(
                #----------------------------------------------
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
                #----------------------------------------------
                # replace missings in price with 0
                kaufpreis = dplyr::case_when(
                    kaufpreis < 0 ~ 0,
                    TRUE ~ kaufpreis
                ),
                # add logarithmic price
                ln_houseprice = log(kaufpreis),
                # add house price per sqm
                houseprice_sqmeter = kaufpreis / wohnflaeche,
                #----------------------------------------------
                # recode missings in plot area
                grundstuecksflaeche = dplyr::case_when(
                    grundstuecksflaeche < 0 ~ 0,
                    TRUE ~ grundstuecksflaeche
                ),
                # create categories for plot area
                # TODO: Do these categories make sense?
                # TODO: Is it even necessary to create categories? Since we are
                # not interested in the coefficient of the categories anyway. We
                # could just use the continuous variable.
                plot_area_cat = dplyr::case_when(
                    grundstuecksflaeche <= 0 ~ 0,
                    grundstuecksflaeche > 0 & grundstuecksflaeche <= 200 ~ 1,
                    grundstuecksflaeche > 200 & grundstuecksflaeche <= 400 ~ 2,
                    grundstuecksflaeche > 400 & grundstuecksflaeche <= 600 ~ 3,
                    grundstuecksflaeche > 600 & grundstuecksflaeche <= 800 ~ 4,
                    grundstuecksflaeche > 800 & grundstuecksflaeche <= 1000 ~ 5,
                    grundstuecksflaeche > 1200 ~ 6
                ),
                #----------------------------------------------
                # categories for house types
                # dummy for single family houses (SFH detached, Bungalow, Farmhouse)
                # TODO: rename these variable in English
                typ_freistehend = dplyr::case_when(
                    kategorie_Haus == 1 | kategorie_Haus == 7 | kategorie_Haus == 8 ~ 1,
                    TRUE ~ 0
                ),
                # dummy for semi-detached single family houses (SFH, semi-detached)
                typ_DHH = dplyr::case_when(
                    kategorie_Haus == 2 | kategorie_Haus == 3 ~ 1,
                    TRUE ~ 0
                ),
                # dummy for terraced single family house (all categories for
                # terraced houses)
                tpy_Reihenhaus = dplyr::case_when(
                    kategorie_Haus == 4 | kategorie_Haus == 5 | kategorie_Haus == 6 ~ 1,
                    TRUE ~ 0
                ),
                # dummy for exclusive house (Castle, Mansion)
                typ_exclusive = dplyr::case_when(
                    kategorie_Haus == 9 | kategorie_Haus == 10 ~ 1,
                    TRUE ~ 0
                ),
                # dummy for block flats (two-family house or block of flats)
                type_MFH = dplyr::case_when(
                    kategorie_Haus == 11 | kategorie_Haus == 12 ~ 1,
                    TRUE ~ 0
                ),
                # dummy of other house types
                typ_other = dplyr::case_when(
                    kategorie_Haus == 13 | kategorie_Haus == 14 | kategorie_Haus == 15 ~ 1,
                    TRUE ~ 0
                )
            )
    }

    #----------------------------------------------
    # replacing missing information

    org_data <- org_data |>
        dplyr::mutate(
            balkon = dplyr::case_when(
                balkon < 0 ~ 0,
                TRUE ~ balkon
            ),
            garten = dplyr::case_when(
                garten < 0 ~ 0,
                TRUE ~ garten
            ),
            einbaukueche = dplyr::case_when(
                einbaukueche < 0 ~ 0,
                TRUE ~ einbaukueche
            ),
            gaestewc = dplyr::case_when(
                gaestewc < 0 ~ 0,
                TRUE ~ gaestewc
            ),
            keller = dplyr::case_when(
                keller < 0 ~ 0,
                TRUE ~ keller
            ),
            aufzug = dplyr::case_when(
                aufzug < 0 ~ 0,
                TRUE ~ aufzug
            ),
            ausstattung = dplyr::case_when(
                ausstattung < 0 ~ 0,
                TRUE ~ ausstattung
            ),
            einliegerwohnung = dplyr::case_when(
                einliegerwohnung < 0 ~ 0,
                TRUE ~ einliegerwohnung
            ),
            zimmeranzahl = dplyr::case_when(
                zimmeranzahl < 0 ~ 0,
                TRUE ~ zimmeranzahl
            ),
            # round down number of rooms to nearest integer
            # TODO: rename to English
            zimmeranzahl_full = floor(zimmeranzahl) 
        )

    #----------------------------------------------
    # handle extreme values
    # TODO: adjust the extreme values to a data-driven approach (maybe use
    # percentiles); problem: the current approach is fixed and does not respond
    # to potential changes in the composition of homes

    if (data_type == "WM") {
        org_data <- org_data |>
            dplyr::filter(
                zimmeranzahl_full <= 7
            ) |>
            dplyr::filter(
                mietekalt > 0 & mietekalt <= 5000
            ) |>
            dplyr::filter(
                wohnflaeche >= 15 & wohnflaeche <= 400
            )
    } else if (data_type == "HK") {
        org_data <- org_data |>
            dplyr::filter(
                zimmeranzahl_full <= 15
            ) |>
            dplyr::filter(
                kaufpreis > 0 & kaufpreis <= 5000000
            ) |>
            dplyr::filter(
                wohnflaeche >= 50 & wohnflaeche <= 600
            ) |>
            dplyr::filter(
                # to sdrop houses with agrarian use
                grundstuecksflaeche <= 2500
            ) |>
            dplyr::filter(
                anzahletagen <= 5
            )
    } else {
        # TODO: for living space a data-driven approach is used, why? adjust
        # everything to a data-driven approach
        org_data <- org_data |>
            dplyr::filter(
                zimmeranzahl_full <= 8
            ) |>
            dplyr::filter(
                kaufpreis > 0 & kaufpreis <= 2000000
            ) |>
            dplyr::filter(
                wohnflaeche >= as.numeric(quantile(org_data$wohnflaeche, 0.01, na.rm = TRUE)) &
                wohnflaeche <= as.numeric(quantile(org_data$wohnflaeche, 0.99, na.rm = TRUE))
            )
    }

    #----------------------------------------------
    # keep only last spell, i.e. latest update of the listing

    org_data$n <- ave(
        1:length(org_data$obid),
        org_data$obid,
        FUN = length
    )

    org_data <- org_data |>
        dplyr::filter(
            n == spell
        ) |>
        dplyr::select(
            -n
        )

    # UNIT TEST: no duplicates in obid
    tar_assert_true(
        sum(duplicated(org_data$obid)) == 0
    )

    #----------------------------------------------
    # recode all negative values to NA

    org_data[org_data < 0] <- NA

    #----------------------------------------------
    # add geo information
    # add municipality verband and labor market regions
    # NOTE: replace municipality code (gid2019) with code from municipality
    # verband

    #----------------------------------------------
    # return

    return(org_data)
}