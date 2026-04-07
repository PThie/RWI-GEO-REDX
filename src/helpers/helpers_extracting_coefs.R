helpers_extracting_coefs <- function(
    model = NA,
    means_independent = NA,
    est_sample = NA
) {
    #' @title Extracting Coefficients from Model
    #' 
    #' @description This function extracts and processes coefficients from the
    #' regression merging them with categories of independent.
    #' 
    #' @param model Fitted model
    #' @param means_independent Dataframe with means of all independent variables
    #' @param est_sample Dataframe with estimation sample
    #' 
    #' @note See for more explanations on the calculation GitHub Issue #4
    #' 
    #' @return Data frame with processed coefficients
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # extract coefficients from model

    coefs <- as.data.frame(model$coefficients)
    colnames(coefs) <- "estimate"
    coefs$var_aux <- rownames(coefs)
    rownames(coefs) <- NULL

    coefs <- coefs |>
        dplyr::mutate(
            var_name = stringr::str_extract(var_aux, "(?<=\\()[^()]+(?=\\))"),
            demeaned_cat = as.numeric(
                stringr::str_extract(var_aux, "(?<=\\)).*")
            ),
            var_name = data.table::fcoalesce(var_name, var_aux)
        ) |>
        dplyr::select(-var_aux) |>
        merge(
            means_independent,
            by = "var_name"
        ) |>
        dplyr::mutate(
            org_cat = as.character(round(
                demeaned_cat + mean_var,
                0
            ))
        )

    #--------------------------------------------------
    # get reference categories

    vars <- unique(coefs$var_name)
    reference_list <- list()
    for (var in vars) {
        # get mean for variable
        mean_var <- means_independent[means_independent$var_name == var, "mean_var"] |>
                as.numeric()
        
        # get the unique values in the estimation sample
        # NOTE: add mean to get the original category
        unique_values_data <- (unique(est_sample[[var]]) + mean_var) |>
                as.character()

        # exclude NAs
        unique_values_data <- unique_values_data[
            !is.na(unique_values_data)
        ]

        # get unique categories in coefficients
        unique_values_coefs <- unique(coefs[coefs$var_name == var, "org_cat"])

        # identify reference category as category that is in data but not in
        # coefficients
        reference <- setdiff(unique_values_data, unique_values_coefs)

        # check that only one reference is return
        targets::tar_assert_true(
            length(reference) == 1,
            msg = glue::glue(
                "!!! WARNING: ",
                "There is more than one reference category reported for {var}.",
                " (Error code: hec#1)"
            )
        )

        # store results in dataframe
        df <- data.frame(cbind(
            var_name = var,
            estimate = 0,
            demeaned_cat = as.numeric(reference) - mean_var,
            mean_var = mean_var,
            org_cat = reference
        ))

        reference_list[[var]] <- df
    }

    references <- data.table::rbindlist(reference_list)

    #--------------------------------------------------
    # combine coefficients and reference categories

    coefs_ref <- rbind(
        references,
        coefs
    ) |>
    dplyr::arrange(var_name, org_cat)

    #--------------------------------------------------
    # calculate weight
    # the weight is the share of observations for each category

    weights <- est_sample |>
        dplyr::select(unique(means_independent$var_name)) |>
        tidyr::pivot_longer(
            cols = everything(),
            names_to = "var_name",
            values_to = "demeaned_cat"
        ) |>
        dplyr::count(var_name, demeaned_cat) |>
        dplyr::mutate(
            weight = n / nrow(est_sample),
            demeaned_cat = as.character(demeaned_cat)
        ) |>
        dplyr::select(-n)

    #--------------------------------------------------
    # merge weights with coefficients

    coefs_weights <- merge(
        coefs_ref,
        weights,
        by = c("var_name", "demeaned_cat")
    )

    #--------------------------------------------------
    # compute baseline effect

    coefs_weights <- coefs_weights |>
        dplyr::mutate(
            estimate = as.numeric(estimate),
            aux = weight * estimate
        ) |>
        dplyr::group_by(var_name) |>
        dplyr::mutate(
            baseline = -sum(aux)
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-c("aux", "weight"))

    #--------------------------------------------------
    # compute category contribution (category effect)

    coefs_contrs <- coefs_weights |>
        dplyr::mutate(
            contribution_cat = baseline + estimate
        ) |>
        dplyr::select(-estimate)

    #--------------------------------------------------
    # return

    return(coefs_contrs)
}