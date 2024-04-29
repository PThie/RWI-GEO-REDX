reading_housing_data_org <- function(data_file_path = NA) {
    #' @title Reading the original housing data
    #' 
    #' @description This function reads the original housing data.
    #' 
    #' @param data_file_path Path to original housing data.
    #' 
    #' @return Data frame with the original housing data.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # read data

    org_data <- arrow::read_parquet(
        data_file_path
    )

    #--------------------------------------------------
    # return

    return(org_data)
}