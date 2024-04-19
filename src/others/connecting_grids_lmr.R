connecting_grids_lmr <- function (
    grid_cleaned = NA,
    labor_market_regions_cleaned = NA
) {
    #' @title Connecting grids and labor market regions
    #' 
    #' @description This file uses the geo information of the grids and the
    #' labor market regions to connect them.
    #' 
    #' @param grid_cleaned Clean data sets of geo information of grids
    #' @param labor_market_regions_cleaned Clean data sets of geo information of
    #' labor market regions (LMR)
    #' 
    #' @return Dataframe connecting grids and labor market regions
    #' @author Patrick Thiel

    #----------------------------------------------
    #  join both data sets together

    suppressWarnings(
        grids_lmr <- sf::st_join(
            grid_cleaned,
            labor_market_regions_cleaned,
            left = TRUE,
            largest = TRUE
        )
    )

    # remove geometry
    grids_lmr <- sf::st_drop_geometry(grids_lmr)

    #----------------------------------------------
    # return

    return(grids_lmr)
}