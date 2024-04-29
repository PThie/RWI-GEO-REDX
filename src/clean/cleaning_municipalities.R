cleaning_municipalities <- function(municipalities_raw) {
    #' @title Creating a clean municipality data set
    #' 
    #' @description This function cleans up the geographical information on the
    #' municipalities.
    #' 
    #' @param municipalities_raw Original municipality data
    #' 
    #' @return qs object
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # clean municipalities

    municipalities <- municipalities_raw |>
        dplyr::select(
            gid2019 = AGS,
            gid2019_name = GEN,
            geometry
        ) |>
        dplyr::mutate(
            gid2019_name = stringi::stri_trans_general(
                gid2019_name,
                "de-ASCII; Latin-ASCII"
            )
        ) |>
        sf::st_transform(config_globals()[["utmcrs"]])

    # set as sf object
    municipalities <- sf::st_as_sf(municipalities)

    #----------------------------------------------
    # return
    
    return(municipalities)
}
