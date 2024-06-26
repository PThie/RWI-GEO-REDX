estimating_regional_effects_change <- function(
    housing_data = NA,
    housing_type = NA,
    grids_municipalities = NA
) {
    #' @title Estimating regional effects change
    #' 
    #' @description This function estimates regional effects for a given housing
    #' type and computes the change relative to the first time period.
    #' 
    #' @param housing_data Data frame with housing data
    #' @param housing_type Housing type
    #' @param grids_municipalities Data frame with connection between grids
    #' and municipalities
    #' 
    #' @note This refers to regression 3 in the former Stata coding.
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

    housing_data <- preparing_estimation(
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
                time_fe_id <- 1
                string_cutoff <- 4
                time_label <- "year"
                reference_period <- "2008"
            } else {
                time_fe_id <- 2
                string_cutoff <- 7
                time_label <- "quarter"
                reference_period <- "2008-01"
            }

            #--------------------------------------------------
            # create fixed effect combination between regional and time fixed effects

            est_data <- housing_data |>
                dplyr::mutate(
                    id_fe = paste0(
                        !!rlang::sym(time_fes[time_fe_id]),
                        "_",
                        !!rlang::sym(regional_fes[1])
                    )
                )
    
            #--------------------------------------------------
            # define formula

            form <- as.formula(
                paste(
                    depvar,
                    paste(
                        c(
                            paste(indepvars, collapse = " + ")
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
                data = est_data,
                se = "hetero",
                fixef = "id_fe"
            )

            #--------------------------------------------------
            # extract fixed effects

            fixed_effects <- fixest::fixef(base_mod) |>
                as.data.frame() |>
                dplyr::rename(pindex_FE = id_fe)

            # replace rownames
            fixed_effects$id_fe <- rownames(fixed_effects)
            rownames(fixed_effects) <- seq(1, nrow(fixed_effects))

            #--------------------------------------------------
            # calculate deviation from overall mean (= constant)
            # calculate price index, i.e. delogarithmize the fixed effect deviation
            # NOTE:: replace orginal fixed effects with deviation (as Stata does
            # with predict, u)

            fixed_effects <- fixed_effects |>
                dplyr::mutate(
                    pindex = (exp(pindex_FE) - 1) * 100,
                    # separate region and time into separate columns
                    !!time_label := substring(id_fe, 1, string_cutoff),
                    grid = substring(id_fe, string_cutoff + 2, nchar(id_fe))
                ) |>
                dplyr::select(-c(pindex_FE, id_fe))

            #--------------------------------------------------
            # calculate the change between District_Year - District_2008

            fixed_effects <- fixed_effects |>
                merge(
                    fixed_effects |>
                        dplyr::filter(!!rlang::sym(time_label) == reference_period) |>
                        dplyr::rename(pindex_ref = pindex) |>
                        dplyr::select(-time_label),
                    by = "grid",
                    all.x = TRUE
                ) |>
                dplyr::mutate(
                    pindex_change = dplyr::case_when(
                        !is.na(pindex_ref) ~ (
                            (pindex - pindex_ref) / pindex_ref
                        ) * 100,
                        TRUE ~ NA_real_
                    )
                ) |>
                dplyr::select(-c(pindex_ref))
            
            #--------------------------------------------------
            # used sample

            indices_include_obs <- fixest::obs(base_mod)
            used_sample <- est_data[indices_include_obs, ]

            #--------------------------------------------------
            # calculate number of observations and mean of dependent variable

            nobs <- used_sample |>
                dplyr::group_by(
                    !!rlang::sym(time_fes[time_fe_id]),
                    !!rlang::sym(regional_fes[1])
                ) |>
                dplyr::summarise(
                    nobs_grid = n()
                    #!!mean_name_grid := mean(.data[[depvar]], na.rm = TRUE)
                ) |>
                dplyr::rename(!!time_label := 1) |>
                dplyr::ungroup() |>
                merge(
                    grids_municipalities,
                    by = "ergg_1km",
                    all.x = TRUE
                ) |>
                # number of observations at higher level of aggregation
                dplyr::group_by(
                    !!rlang::sym(time_label),
                    gid2019
                ) |>
                dplyr::mutate(
                    nobs_munic = sum(nobs_grid, na.rm = TRUE),
                    #!!mean_name_munic := mean(.data[[mean_name_grid]], na.rm = TRUE),
                    kid2019 = substring(gid2019, 1, 5)
                ) |>
                dplyr::ungroup() |>
                dplyr::group_by(
                    !!rlang::sym(time_label),
                    kid2019
                ) |>
                dplyr::mutate(
                    nobs_district = sum(nobs_grid, na.rm = TRUE)
                    #!!mean_name_district := mean(.data[[mean_name_grid]], na.rm = TRUE)
                ) |>
                dplyr::filter(!is.na(!!rlang::sym(regional_fes[1]))) |>
                dplyr::rename(grid = ergg_1km) |>
                as.data.frame()

            #--------------------------------------------------
            # merge FE and NOBS data

            regional_coef <- merge(
                fixed_effects,
                nobs,
                by = c(time_label, "grid"),
                all.x = TRUE
            )

            #--------------------------------------------------
            # export findings

            openxlsx::write.xlsx(
                regional_coef,
                file.path(
                    config_paths()[["output_path"]],
                    paste0(housing_type, "_rebuild"),
                    paste0("regional_effects_grids_", time_label, "_change.xlsx")
                )
            )

            #--------------------------------------------------
            # store results

            results_list[[time_label]] <- regional_coef
        }
    }

    #--------------------------------------------------
    # return

    return(results_list)
}