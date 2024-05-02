extracting_time_effects <- function(model = NA, time = NA) {
    #' @title Extract time effects
    #' 
    #' @description This function extracts time effects from a regression model.
    #' 
    #' @param model Regression model
    #' @param time Time variable
    #' 
    #' @return Time effects with confidence intervals
    #' @author Patrick Thiel

    #--------------------------------------------------
    # extract confidence intervals (CI 95%)

    ci <- as.data.frame(confint(model, level = 0.95))
    ci$var <- rownames(ci)

    #--------------------------------------------------
    # extract coefficients

    coefs <- as.data.frame(model$coefficients)
    coefs$var <- rownames(coefs)

    #--------------------------------------------------
    # combine, clean and define base time

    if (time == "ejahr") {
        cutoff <- 3
        base_time <- "2008"
        time_name <- "year"
    } else if (time == "e_year_quarter") {
        cutoff <- 6
        base_time <- "2008-01"
        time_name <- "quarter"
    }

    coefs_ci <- merge(coefs, ci, by = "var") |>
        dplyr::filter(stringr::str_detect(var, time)) |>
        dplyr::mutate(
            var = substring(var, nchar(var) - cutoff, nchar(var))
        ) |>
        dplyr::rename(
            !!time_name := var,
            timeeff = `model$coefficients`,
            timeeff_p025 = `2.5 %`,
            timeeff_p975 = `97.5 %`
        ) |>
        dplyr::bind_rows(
            data.frame(
                placeholder = base_time,
                timeeff = 0,
                timeeff_p025 = 0,
                timeeff_p975 = 0
            ) |>
            dplyr::rename(!!time_name := placeholder)
        ) |>
        # NOTE: Arrange does not work yet
        dplyr::arrange(!!rlang::sym(time_name))

    #--------------------------------------------------
    # return

    return(coefs_ci)
}
