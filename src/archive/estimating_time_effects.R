estimating_time_effects <- function(
    housing_data = NA,
    data_type = NA
) {
    #' @title Estimate time effects (Regression 1)
    #' 
    #' @description
    #' 
    #' @param housing_data Housing data (type-specific), comes from target
    #' pipeline
    #' @param data_type Type of housing data (e.g. "WK")
    #' 
    #' @return Estimation output
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # read data

    #org_data <- targets::tar_read(housing_data)
    #--------------------------------------------------
    # drop last month in the sample to prevent look-ahead bias
    # drop 2007 since the data provided ImmoScout is a bit different from 
    # the waves afterward

    print(typeof(housing_data$e_year_mon))

    org_data_prep <- housing_data |>
        dplyr::filter(
            e_year_mon != paste0(
                config_globals()[["max_year"]],
                "-",
                config_globals()[["max_month"]]
            )
        ) |>
        dplyr::filter(ejahr > 2007)
    
    #--------------------------------------------------
    # regression 1: Time effects

    regions <- c("kid2019")
    times <- c("ejahr", "e_year_quarter")
    
    for (region in regions) {
        for (time in times) {
            #--------------------------------------------------
            # TODO: move variable definition to config.R
            # estimation

            mod <- fixest::feols(
                ln_flatprice_sqm ~ as.factor(construction_year_cat) +
                    as.factor(first_occupancy) + as.factor(balkon) +
                    as.factor(garten) + as.factor(einbaukueche) +
                    as.factor(keller) + as.factor(aufzug) +
                    as.factor(gaestewc) + as.factor(ausstattung) +
                    as.factor(declared_wohngeld) + as.factor(betreut) +
                    zimmeranzahl_full + as.factor(num_floors_cat) + 
                    as.factor(floors_cat) + as.factor(e_year_quarter),
                    fixef = region,
                    cluster = region,
                    data = org_data_prep
            )

            #--------------------------------------------------
            # extract confidence intervals (CI 95%)

            ci <- as.data.frame(confint(mod, level = 0.95))
            ci$var <- rownames(ci)

            #--------------------------------------------------
            # extract coefficients

            coefs <- as.data.frame(mod$coefficient)
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
                    timeeff = `mod$coefficient`,
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
            # export

            openxlsx::write.xlsx(
                coefs_ci,
                file.path(
                    config_paths()[["output_path"]],
                    paste0(data_type, "_rebuild"),
                    "output",
                    paste0(
                        region,
                        "_timeeff_",
                        data_type,
                        "_",
                        time,
                        "_reg1.xlsx"
                    )
                ),
                row.names = FALSE
            )
        }
    }
#     etable(mod, digits = "r9")

# # names(org_data_prep)

# # extract fixed effects
# fixed_effects <- fixest::fixef(mod) |>
#     as.data.frame() |>
#     dplyr::rename(pindex_FE = id_fe)

# # replace rownames
# fixed_effects$id_fe <- rownames(fixed_effects)
# rownames(fixed_effects) <- seq(1, nrow(fixed_effects))

# # determine constant
# constant <- mean(tst_mod$sumFE)

# # org_data_prep |>
# #     select(contains("flat")) |>
# #     names()

#     org_findings = haven::read_dta(
#         "M:/_FDZ/RWI-GEO/RWI-GEO-REDX/output/WK/data/Jun2023/kid2019_WK_ejahr1.dta"
#     )

#     org_findings <- org_findings |>
#         filter(ejahr > 2007)

# bla = data.table::fread("M:/_FDZ/RWI-GEO/RWI-GEO-REDX/output/WK/data/Jun2023/kid2019_timeeff_WK_ejahr_reg1.csv")



}