clean_grids <- function() {
    #' @title
    #' 
    #' @description
    #' 
    #' @param
    #' 
    #' @return
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


}