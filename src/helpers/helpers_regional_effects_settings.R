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
    } else {
        nobs_var <- "nobs_lmr"
        region_id <- "lmrid"
        region_label <- "lmr"
        sheet_name <- "LMR"
    }

    # time level
    # NOTE: if the temporal aggregation level should be expanded to other levels
    # (e.g. quarters), the option here but also the corresponding code needs to
    # be adjusted
    time_label <- "year"

    #--------------------------------------------------
    # combine all setting

    all_settings <- list(
        "nobs_var" = nobs_var,
        "region_id" = region_id,
        "region_label" = region_label,
        "sheet_name" = sheet_name,
        "time_label" = time_label
    )

    #--------------------------------------------------
    # return

    return(all_settings)
}
