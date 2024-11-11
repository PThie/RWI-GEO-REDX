helpers_regional_effects_change_settings <- function(
    time_period = NA
) {
    #' @title Regional effects change ettings
    #' 
    #' @description This function defines the settings (names) for the aggregation
    #' and export of the regional effects changes estimations with the focus on
    #' temporal variables.
    #' 
    #' @param time_period Character defining the temporal level
    #' 
    #' @return List with settings
    #' @author Patrick Thiel

    #--------------------------------------------------
    # define settings for different time periods

    if (time_period == "ejahr" | time == "year") {
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
    # combine all settings

    all_settings <- list(
        "time_fe_id" = time_fe_id,
        "string_cutoff" = string_cutoff,
        "time_label" = time_label,
        "reference_period" = reference_period
    )

    #--------------------------------------------------
    # return

    return(all_settings)
}