reading_housing_data_org <- function(housing_data_org_file_name = NA) {
    
    org_data <- haven::read_dta(housing_data_org_file_name)

    return(org_data)
}