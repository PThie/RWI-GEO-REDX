helpers_extracting_coefs <- function(
    model = NA,
    means_independent = NA
) {
    #' @title Extracting Coefficients from Model
    #' 
    #' @description This function extracts and processes coefficients from the
    #' regression merging them with categories of independent.
    #' 
    #' @param model Fitted model
    #' @param means_independent Dataframe with means of all independent variables
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
    # identify dummies to assign the mean to the base category of the dummy

    coefs <- coefs |>
        dplyr::group_by(var_name) |>
        dplyr::mutate(
            n_cats = dplyr::n(),
            dummy = dplyr::case_when(
                n_cats == 1 ~ 1,
                TRUE ~ 0
            )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-dplyr::all_of(c("n_cats", "demeaned_cat")))

    # NOTE: for the dummies the base category is equal to the mean
    # and the base category is "0"
    dummies <- coefs |>
        dplyr::filter(dummy == 1) |>
        dplyr::mutate(
            org_cat = 0,
            estimate = 0 - mean_var
        )

    # combine both (coefficients and dummy base coefficients)
    coefs <- rbind(
        coefs,
        dummies
    ) |>
    dplyr::select(-dummy) |>
    dplyr::arrange(var_name, org_cat)

    #--------------------------------------------------
    # return

    return(coefs)
}