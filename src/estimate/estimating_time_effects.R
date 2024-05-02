estimating_time_effects <- function(
    housing_data = NA,
    housing_type = NA
) {

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

    housing_data <- housing_data |>
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
    # perform estimation

    # define formula
    # TODO: Adjust to loop to allow for various time FE

    for (time_fe in time_fes) {
        # NOTE: analysis not performed at monthly level because the number
        # of observations is too low
        if (time_fe != "e_year_mon") {
            # define formula
            form <- as.formula(
                paste(
                    depvar,
                    paste(
                        c(
                            paste(indepvars, collapse = " + "),
                            paste("as.factor(", time_fe, ")")
                        ),
                        collapse = " + "
                    ),
                    sep = "~"
                )
            )

            # estimate the model
            base_mod <- fixest::feols(
                fml = form,
                data = housing_data,
                se = "hetero",
                fixef = regional_fes[1]
            )

            # extract time coefficients (coefficients on years or quarters)
            time_coefs <- extracting_time_effects(model = base_mod, time = time_fe)

            # export findings
            data.table::fwrite(
                time_coefs,
                file.path(
                    config_paths()[["output_path"]],
                    paste0(housing_type, "_rebuild"),
                    paste0("time_effects_grids_", time_fe, ".csv")
                ),
                sep = ";"
            )
        }
    }

    #--------------------------------------------------
    # return
    
    return(NULL)


    # etable(tst_mod)


    # df = extracting_time_effects(model = tst_mod, time = "ejahr")


}
# unique(housing_data_cleaned$ejahr)
# housing_data_cleaned = HK_cleaned

# housing_data_cleaned <- housing_data_cleaned |>
#     mutate(
#         ln_houseprice_sqm = log(houseprice_sqmeter)
#     ) |>
#     filter(ejahr != 2007)

# names(HK_cleaned)

# names(housing_data_cleaned)
