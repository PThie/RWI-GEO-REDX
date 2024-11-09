reading_estimated_region_effects_prev_version <- function(data_file_path = NA) {
    #' @title Read estimated region effects from previous version
    #' 
    #' @description This function reads the estimated region effects from the
    #' previous version in order to compare the current results to it.
    #' 
    #' @param data_file_path Character string with the path to the data file.
    #' 
    #' @return Dataframe with the estimated region effects from the previous version.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # load data

    dta <- openxlsx::read.xlsx(
        data_file_path
    )

    #--------------------------------------------------
    # return

    return(dta)
}
