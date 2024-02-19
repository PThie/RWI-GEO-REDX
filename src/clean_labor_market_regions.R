clean_labor_market_regions <- function() {
    #' @title Creating a clean labor market regions data set
    #' 
    #' @description This function cleans up the geographical information on the
    #' labor market regions based on RWI defintion.
    #' 
    #' @return qs object
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # load labor market regions

    lmr <- sf::st_read(
        file.path(
            config_paths()[["gebiete_path"]],
            "Arbeitsmarktregion",
            "RWI2018",
            "shapefiles",
            "amr2.gpkg"
        ),
        quiet = TRUE
    )

    #----------------------------------------------
    # clean labor market regions

    # rename geometry column
    sf::st_geometry(lmr) <- "geometry"

    # more cleaning
    lmr <- lmr |>
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
