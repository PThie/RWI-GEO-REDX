helpers_regional_effects_settings <- function(
    agg_level = NA
) {
    #' @title Regional effects settings
    #' 
    #' @description This function defines the settings (names) for the aggregation
    #' and export of the regional effects estimations.
    #' 
    #' @param agg_level Character defining the regional level
    #' 
    #' @return List with settings
    #' @author Patrick Thiel

    #--------------------------------------------------
    # define settings for the different regional levels

    if (agg_level == "munic" | agg_level == "municipality") {
        nobs_var <- "nobs_munic"
        region_id <- "gid2019"
        region_label <- "municipality"
        sheet_name <- "Munic"
    } else if (agg_level == "district") {
        nobs_var <- "nobs_district"
        region_id <- "kid2019"
        region_label <- "district"
        sheet_name <- "Distr"
    } else if (agg_level == "lmr"){
        nobs_var <- "nobs_lmr"
        region_id <- "lmrid"
        region_label <- "lmr"
        sheet_name <- "LMR"
    } else {
        nobs_var <- "nobs_zipcode"
        region_id <- "zipcodeid"
        region_label <- "zipcode"
        sheet_name <- "Zip"
    }

    #--------------------------------------------------
    # combine all setting

    all_settings <- list(
        "nobs_var" = nobs_var,
        "region_id" = region_id,
        "region_label" = region_label,
        "sheet_name" = sheet_name
    )

    #--------------------------------------------------
    # return

    return(all_settings)
}
