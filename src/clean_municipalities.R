clean_municipalities <- function() {
    #' @title Creating a clean municipality data set
    #' 
    #' @description This function cleans up the geographical information on the
    #' municipalities.
    #' 
    #' @note The municipalities considered here are not the direct municipalities
    #' (Gemeinde) but rather the municipality associations (Gemeindeverband), i.e.
    #' one aggregation level higher than municipalities. The reason is that the
    #' number of observations is (partially) too low for the direct municipalities
    #' when doing the estimations in the next step.
    #' 
    #' @return qs object
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # load municipalities

    municipalities <- sf::st_read(
        file.path(
            config_paths()[["gebiete_path"]],
            "Verwaltungsgemeinschaften",
            "2019",
            "VG250_VWG.shp"
        ),
        quiet = TRUE
    )

    
}