cleaning_labor_market_regions <- function(labor_market_regions_raw = NA) {
    #' @title Creating a clean labor market regions data set
    #' 
    #' @description This function cleans up the geographical information on the
    #' labor market regions based on RWI defintion.
    #' 
    #' @param labor_market_regions_raw Original labor market regions
    #' 
    #' @return qs object
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # clean labor market regions

    # rename geometry column
    sf::st_geometry(labor_market_regions_raw) <- "geometry"

    # more cleaning
    lmr <- labor_market_regions_raw |>
        dplyr::select(
            amr = AMR2,
            geometry
        ) |>
        dplyr::mutate(
            # add leading zeros to amr
            amr = stringr::str_pad(amr, 5, pad = "0")
        ) |>
        sf::st_transform(config_globals()[["utmcrs"]])

    # set as sf object
    lmr <- sf::st_as_sf(lmr)

    #----------------------------------------------
    # return

    return(lmr)
}
