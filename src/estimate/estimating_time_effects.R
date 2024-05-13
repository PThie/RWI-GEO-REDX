estimating_time_effects <- function(
    housing_data = NA,
    housing_type = NA
) {
    #' @title Estimating time effects
    #' 
    #' @description This function estimates time effects for a given housing type.
    #' 
    #' @param housing_data Data frame with housing data
    #' @param housing_type Housing type
    #' 
    #' @note This refers to regression 1 in the former Stata coding.
    #' 
    #' @return List with estimation output
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

    results_list <- list()
    for (time_fe in time_fes) {
        # NOTE: analysis not performed at monthly level because the number
        # of observations is too low
        if (time_fe != "e_year_mon") {
            #--------------------------------------------------
            # set up for different time periods

            if (time_fe == "ejahr") {
                time_label <- "year"
            } else {
                time_label <- "quarter"
            }

            #--------------------------------------------------
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

            #--------------------------------------------------
            # estimate the model

            base_mod <- fixest::feols(
                fml = form,
                data = housing_data,
                se = "hetero",
                fixef = regional_fes[1]
            )

            #--------------------------------------------------
            # used sample

            indices_include_obs <- fixest::obs(base_mod)
            used_sample <- housing_data[indices_include_obs, ]

            #--------------------------------------------------
            # number of observations

            mean_name <- paste0("mean_", depvar)
            
            nobs <- used_sample |>
                dplyr::group_by(!!rlang::sym(time_fe)) |>
                dplyr::summarise(
                    nobs = n(),
                    !!mean_name := mean(.data[[depvar]], na.rm = TRUE)
                ) |>
                dplyr::rename(!!time_label := 1) |>
                as.data.frame()

            #--------------------------------------------------
            # extract time coefficients (coefficients on years or quarters)

            time_coefs <- extracting_time_effects(
                model = base_mod,
                time = time_fe
            )

            #--------------------------------------------------
            # merge with number of observations

            time_coefs <- merge(
                time_coefs,
                nobs,
                by = time_label,
                all.x = TRUE
            )

            #--------------------------------------------------
            # export findings
            
            openxlsx::write.xlsx(
                time_coefs,
                file.path(
                    config_paths()[["output_path"]],
                    paste0(housing_type, "_rebuild"),
                    paste0("time_effects_grids_", time_label, ".xlsx")
                )
            )
            #--------------------------------------------------
            # store results

            results_list[[time_fe]] <- time_coefs
        }
    }

    #--------------------------------------------------
    # return
    
    return(results_list)
}