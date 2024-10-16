estimating_regional_effects <- function(
    housing_data = NA,
    housing_type = NA,
    grids_municipalities = NA
) {
    #' @title Estimating regional effects
    #' 
    #' @description This function estimates regional effects for a given housing
    #' type.
    #' 
    #' @param housing_data Data frame with housing data
    #' @param housing_type Housing type
    #' @param grids_municipalities Data frame with connection between grids
    #' 
    #' @note This refers to regression 2 in the former Stata coding.
    #' @note Only performed at yearly level (running cross-sections at level
    #' below year would result in too many output).
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
    # cross-sectional estimation
    # loop through all years

    years <- unique(housing_data$ejahr)

    results_list <- list()
    for (year in years) {
        #--------------------------------------------------
        # subset data to year

        est_data <- housing_data |>
            dplyr::filter(ejahr == year)

        #--------------------------------------------------
        # make sure that all factor variables actually have more than one level

        # remove factor declaration for the sake of filtering the data
        clean_indepvars <- stringr::str_replace_all(
            indepvars, "as\\.factor\\(([^)]+)\\)", "\\1"
        )

        # get number of levels for each independent variable
        num_levels <- sapply(
            lapply(
                est_data |>
                    dplyr::select(dplyr::all_of(clean_indepvars)),
                unique
            ),
            length
        ) |>
            as.data.frame()

        # rename and reshape
        colnames(num_levels) <- c("number_of_levels")
        num_levels$indepvar <- rownames(num_levels)
        rownames(num_levels) <- NULL

        # get independent variables with only one level
        solo_indepvars <- num_levels |>
            dplyr::filter(number_of_levels == 1) |>
            dplyr::pull(indepvar)

        if (length(solo_indepvars) > 0) {
            # remove independent variables with only one level
            complete_indepvars <- indepvars[
                stringr::str_detect(indepvars, solo_indepvars) == FALSE
            ]
        } else {
            complete_indepvars <- indepvars
        }

        #--------------------------------------------------
        # define formula

        form <- as.formula(
            paste(
                depvar,
                paste(
                    c(
                        paste(complete_indepvars, collapse = " + ")
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
            fixef = regional_fes[1]
        )
    
        #--------------------------------------------------
        # extract fixed effects

        fixed_effects <- fixest::fixef(base_mod) |>
            as.data.frame() |>
            dplyr::rename(pindex_FE = regional_fes[1])

        # replace rownames
        fixed_effects$grid <- rownames(fixed_effects)
        rownames(fixed_effects) <- seq(1, nrow(fixed_effects))

        #--------------------------------------------------
        # calculate the deviation from the German (national) mean in FE
        # delog values

        mean_FE <- mean(fixed_effects$pindex_FE, na.rm = TRUE)

        fixed_effects <- fixed_effects |>
            dplyr::mutate(
                pindex_FE = pindex_FE - mean_FE,
                pindex = (exp(pindex_FE) - 1) * 100,
                year = year
            ) |>
            dplyr::select(-pindex_FE)

        #--------------------------------------------------
        # used sample

        indices_include_obs <- fixest::obs(base_mod)
        used_sample <- est_data[indices_include_obs, ]

        #--------------------------------------------------
        # number of observations

        nobs <- used_sample |>
            dplyr::group_by(
                !!rlang::sym(regional_fes[1])
            ) |>
            dplyr::summarise(
                nobs_grid = n()
            ) |>
            dplyr::ungroup() |>
            merge(
                grids_municipalities,
                by = "ergg_1km",
                all.x = TRUE
            ) |>
            # number of observations at higher level of aggregation
            dplyr::group_by(
                gid2019
            ) |>
            dplyr::mutate(
                nobs_munic = sum(nobs_grid, na.rm = TRUE),
                kid2019 = substring(gid2019, 1, 5)
            ) |>
            dplyr::ungroup() |>
            dplyr::group_by(
                kid2019
            ) |>
            dplyr::mutate(
                nobs_district = sum(nobs_grid, na.rm = TRUE)
            ) |>
            dplyr::filter(!is.na(!!rlang::sym(regional_fes[1]))) |>
            dplyr::rename(grid = ergg_1km) |>
            as.data.frame()

            #--------------------------------------------------
            # merge FE and NOBS data

            regional_coef <- merge(
                fixed_effects,
                nobs,
                by = "grid",
                all.x = TRUE
            )

            #--------------------------------------------------
            # store results

            results_list[[year]] <- regional_coef
    }

    # combine all years
    results <- data.table::rbindlist(results_list)

    #--------------------------------------------------
    # export findings

    openxlsx::write.xlsx(
        results,
        file.path(
            config_paths()[["output_path"]],
            housing_type,
            "estimates",
            paste0("regional_effects_grids_year.xlsx")
        )
    )

    #--------------------------------------------------
    # return

    return(results)
}