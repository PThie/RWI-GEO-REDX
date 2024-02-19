connect_grids_municipality <- function (
    grid_cleaned = NA,
    municipalities_cleaned = NA
) {
    #' @title Connecting grids and municipality associations
    #' 
    #' @description This file uses the geo information of the grids and the
    #' municipality associations to connect them.
    #' 
    #' @note The municipalities are not municipalities in the sense of Gemeinden
    #' but rather municipality associations (Gemeindeverband). See also the
    #' function clean_municipalities() for more information.
    #' 
    #' @param grid_cleaned Clean data sets of geo information of grids
    #' @param municipalities_cleaned Clean data sets of geo information of
    #' municipality associations
    #' 
    #' @return Dataframe connecting grids and municipality associations
    #' @author Patrick Thiel

    #----------------------------------------------
    #  join both data sets together

    suppressWarnings(
        grids_munic <- sf::st_join(
            grid_cleaned,
            municipalities_cleaned,
            left = TRUE,
            largest = TRUE
        )
    )

    # remove geometry
    grids_munic <- sf::st_drop_geometry(grids_munic)

    #----------------------------------------------
    # return

    return(grids_munic)
}