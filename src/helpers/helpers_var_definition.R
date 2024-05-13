helpers_var_definition <- function(housing_type = NA) {
    #' @description The function sets the dependent and independent variables
    #' for the different housing types used in the estimations. Define also
    #' fixed effects.
    
    #--------------------------------------------------
    # housing characteristics

    var_list <- list()
    if (housing_type == "HK") {
        var_list[["depvar"]] <- "ln_houseprice_sqm"
        var_list[["indepvars"]] <- c(
            "as.factor(construction_year_cat)",
            "as.factor(first_occupancy)",
            "as.factor(gaestewc)",
            "as.factor(einliegerwohnung)",
            "as.factor(ausstattung)",
            "as.factor(zimmeranzahl_full)",
            "as.factor(plot_area_cat)",
            "as.factor(typ_freistehend)",
            "as.factor(typ_DHH)",
            "as.factor(typ_Reihenhaus)",
            "as.factor(typ_exclusive)",
            "as.factor(typ_MFH)",
            "as.factor(typ_other)"
        )
    } else if (housing_type == "WM") {
        var_list[["depvar"]] <- "ln_rent_sqm"
        var_list[["indepvars"]] <- c(
            "as.factor(construction_year_cat)",
            "as.factor(first_occupancy)",
            "as.factor(balkon)",
            "as.factor(garten)",
            "as.factor(einbaukueche)",
            "as.factor(gaestewc)",
            "as.factor(keller)",
            "as.factor(ausstattung)",
            "as.factor(zimmeranzahl_full)"
        )
    } else {
        var_list[["depvar"]] <- "ln_flatprice_sqm"
        var_list[["indepvars"]] <- c(
            "as.factor(construction_year_cat)",
            "as.factor(first_occupancy)",
            "as.factor(balkon)",
            "as.factor(garten)",
            "as.factor(einbaukueche)",
            "as.factor(gaestewc)",
            "as.factor(aufzug)",
            "as.factor(keller)",
            "as.factor(betreut)",
            "as.factor(ausstattung)",
            "as.factor(declared_wohngeld)",
            "as.factor(zimmeranzahl_full)",
            "as.factor(num_floors_cat)",
            "as.factor(floors_cat)"
        )
    }

    #--------------------------------------------------
    # fixed effects

    if (housing_type %in% c("HK", "WM", "WK")) {
        var_list[["regional_fe"]] <- c("ergg_1km")
        var_list[["time_fe"]] <- c("ejahr", "e_year_quarter")
    }

    #--------------------------------------------------
    # return

    return(var_list)
}