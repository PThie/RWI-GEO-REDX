cleaning_grids <- function(grids_raw = NA) {
    #' @title Creating a clean grid data set
    #' 
    #' @description This function cleans up the geographical information on the
    #' grids.
    #' 
    #' @param grids_raw Original grid data set
    #' 
    #' @return qs object
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # rename grid ID and drop long ID
    # transform to UTM (32632) reference system

    grids <- grids_raw |>
        dplyr::rename(
            ergg_1km = idm
        ) |>
        dplyr::select(-id) |>
        sf::st_transform(config_globals()[["utmcrs"]])

    # set as sf object
    grids <- sf::st_as_sf(grids)

    #----------------------------------------------
    # return

    return(grids)
}