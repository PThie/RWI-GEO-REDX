estimating_time_effects <- function(
    housing_data = NA,
    housing_type = NA,
    reference_periods = NA,
    export = TRUE
) {
    #' @title Estimating time effects
    #' 
    #' @description This function estimates time effects for a given housing type.
    #' 
    #' @param housing_data Data frame with housing data
    #' @param housing_type Housing type
    #' @param reference_year List, Reference periods for time effects
    #' @param export Indicator to export results to Excel, only for non-destatis results
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
    # prepare the housing data
    # includes demeaning of independent variables

    housing_data <- helpers_preparing_estimation(
        housing_type = housing_type,
        data = housing_data
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
                reference_period <- reference_periods[1]

                # exception for rents: Destatis uses here a different reference period
                # NOTE: this is only necessary for the Destatis comparison, not the
                # own data (export = FALSE means comparison with Destatis)
                if (housing_type == "WM" & export == FALSE) {
                    reference_period <- "2020"
                }
            } else {
                time_label <- "quarter"
                reference_period <- reference_periods[2]

                # exception for rents: Destatis uses here a different reference period
                # NOTE: this is only necessary for the Destatis comparison, not the
                # own data (export = FALSE means comparison with Destatis)
                # NOTE: sofar destatis does not offer quarterly data
                if (housing_type == "WM" & export == FALSE) {
                    reference_period <- "2020-01"
                }
            }

            #--------------------------------------------------
            # define formula

            form <- as.formula(
                paste(
                    depvar,
                    paste(
                        c(
                            paste(indepvars, collapse = " + "),
                            paste0("relevel(as.factor(", time_fe, "), '", reference_period,"')")
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
            
            nobs <- used_sample |>
                dplyr::group_by(!!rlang::sym(time_fe)) |>
                dplyr::summarise(
                    nobs = dplyr::n()
                ) |>
                dplyr::rename(!!time_label := 1) |>
                as.data.frame()

            #--------------------------------------------------
            # extract time coefficients (coefficients on years or quarters)
            # NOTE: on why no de-logging is necessary:
            # What we are measuring is the effect relative to 2008 measured
            # directly in percent (if multiplied by 100). Not measured in units
            # of the dependent variable (i.e. logged price/ rent).
            # In the other settings where we use the FE, these are measured in 
            # units of the dependent variable (i.e. logged price/ rent).
            time_coefs <- helpers_extracting_time_effects(
                model = base_mod,
                time = time_fe,
                reference_period = reference_period
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
            # multiple by 100 to get the percentage change

            time_coefs <- time_coefs |>
                dplyr::mutate(
                    dplyr::across(
                        .cols = dplyr::contains("timeeff"),
                        ~ .x * 100
                    )
                )

            #--------------------------------------------------
            # export findings
            
            if (export == TRUE) {
                openxlsx::write.xlsx(
                    time_coefs,
                    file.path(
                        config_paths()[["output_path"]],
                        housing_type,
                        "estimates",
                        paste0("time_effects_grids_", time_label, ".xlsx")
                    )
                )
            }

            #--------------------------------------------------
            # store results

            results_list[[time_fe]] <- time_coefs
        }
    }

    #--------------------------------------------------
    # return
    
    return(results_list)
}