preparing_estimation <- function(housing_type = NA, data = NA) {
    #' @title Preparing estimation
    #' 
    #' @description This function prepares the data for estimation.
    #' 
    #' @param housing_type Housing type
    #' @param data Data frame with housing data
    #' 
    #' @return Data frame with prepared data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # select dependent and independent variables from global definition
    # based on housing type

    depvar <- helpers_var_definition(housing_type = housing_type)[["depvar"]]
    indepvars <- helpers_var_definition(housing_type = housing_type)[["indepvars"]]
    regional_fes <- helpers_var_definition(housing_type = housing_type)[["regional_fe"]]
    time_fes <- helpers_var_definition(housing_type = housing_type)[["time_fe"]]

    #--------------------------------------------------
    # select only relevant columns for estimation

    # remove factor declaration for the sake of filtering the data
    clean_indepvars <- stringr::str_replace_all(
        indepvars, "as\\.factor\\(([^)]+)\\)", "\\1"
    )

    housing_data <- data |>
        dplyr::select(all_of(c(
            depvar,
            clean_indepvars,
            regional_fes,
            time_fes
        )))

    #--------------------------------------------------
    # demeaning independent variables

    housing_data <- housing_data |>
        dplyr::mutate(
            dplyr::across(
                .cols = all_of(clean_indepvars),
                ~ .x - mean(.x, na.rm = TRUE)
            )
        )

    #--------------------------------------------------
    # return

    return(housing_data)
}