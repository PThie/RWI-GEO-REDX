helpers_regional_effects_change_settings <- function(
    time_period = NA,
    destatis = NA,
    housing_type = NA
) {
    #' @title Regional effects change ettings
    #' 
    #' @description This function defines the settings (names) for the aggregation
    #' and export of the regional effects changes estimations with the focus on
    #' temporal variables.
    #' 
    #' @param time_period Character defining the temporal level
    #' @param destatis Logical indicating whether to use Destatis references.
    #' @param housing_type Character string specifying the housing type.
    #' 
    #' @return List with settings
    #' @author Patrick Thiel

    #--------------------------------------------------
    # define settings for different time periods

    if (time_period == "ejahr" | time_period == "year") {
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
    # settings for destatis comparison

    if (destatis == TRUE) {
        if (time_period == "ejahr" | time_period == "year") {
            time_label <- "year"
            if (housing_type == "WM") {
                reference_period <- "2020"
            } else {
                reference_period <- "2015"
            }
        } else {
            time_label <- "quarter"
            if (housing_type == "WM") {
                reference_period <- "2020-01"
            } else {
                reference_period <- "2015-01"
            }
        }
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