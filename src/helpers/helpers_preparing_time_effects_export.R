helpers_preparing_time_effects_export <- function(
    time_effects_data = NA
) {
    #' @title Prepare time effects data for export
    #' 
    #' @description This function prepares the time effects data for export by
    #' keeping only the time and effects columns.
    #' 
    #' @param time_effects_data Data frame with time effects for different time
    #' periods.
    #' 
    #' @return List with time effects data frames.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # keep only time and effects

    results_list <- list()
    for (result in names(time_effects_data)) {
        if (result == "ejahr") {
            time_label <- "year"
        } else {
            time_label <- "quarter"
        }

        time_effects_prep <- time_effects_data[[result]] |>
            dplyr::select(
                !!rlang::sym(time_label),
                dplyr::contains("timeeff")
            )

        results_list[[result]] <- time_effects_prep
    }

    #--------------------------------------------------
    # return

    return(results_list)
}