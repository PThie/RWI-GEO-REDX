cleaning_districts <- function(districts_raw = NA) {
    #' @title Creating a clean districts data set
    #' 
    #' @description This function cleans up the geographical information on the
    #' districts.
    #' 
    #' @param districts_raw Original district data
    #' 
    #' @return qs object
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # clean districts

    districts <- districts_raw |>
        dplyr::select(
            kid2019 = AGS,
            kid2019_name = GEN,
            geometry
        ) |>
        dplyr::mutate(
            kid2019_name = stringi::stri_trans_general(
                kid2019_name,
                "de-ASCII; Latin-ASCII"
            )
        ) |>
        sf::st_transform(config_globals()[["utmcrs"]])

    # set as sf object
    districts <- sf::st_as_sf(districts)

    #----------------------------------------------
    # return
    
    return(districts)
}