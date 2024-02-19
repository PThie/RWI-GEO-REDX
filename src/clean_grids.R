clean_grids <- function() {
    #' @title Creating a clean grid data set
    #' 
    #' @description This function cleans up the geographical information on the
    #' grids.
    #' 
    #' @return qs object
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # load grids

    grids <- sf::st_read(
        file.path(
            config_paths()[["gebiete_path"]],
            "Raster",
            "ger_1km_rectangle",
            "ger_1km_rectangle.shp"
        ),
        quiet = TRUE
    )

    #----------------------------------------------
    # rename grid ID and drop long ID
    # transform to UTM (32632) reference system

    grids <- grids |>
        dplyr::rename(
            ergg_1km = idm
        ) |>
        dplyr::select(-id) |>
        sf::st_transform(config_globals()[["utmcrs"]])

    #----------------------------------------------
    # return

    return(grids)
}