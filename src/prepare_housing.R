prepare_housing <- function(housing_file = NA) {
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
    # recode missing values in living space

    org_data <- org_data |>
        dplyr::mutate(
            wohnflaeche = dplyr::case_when(
                wohnflaeche < 0 ~ 0,
                TRUE ~ wohnflaeche
            )
        )

    #----------------------------------------------
    # type specific cleaning

    if (housing_type == "WK") {
        print("TEST")
    }

    #----------------------------------------------
    # return

    return(org_data)
}