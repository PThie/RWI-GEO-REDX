testing_regional_effects_pattern <- function(
    old_output_data = NA,
    housing_type = NA,
    districts_cleaned = NA
) {
    #' @title Plotting regional effects
    #' 
    #' @description This function plots regional effects for all housing types.
    #' It plots the change but also the levels at district level for both, the
    #' old REDX version (V12) and the newly calculated estimates.
    #' 
    #' @param old_output_data List with estimated regional effects
    #' @param housing_type Housing type
    #' @param districts_cleaned Cleaned districts data
    #' 
    #' @return NULL, graphs
    #' @author Patrick Thiel

    #--------------------------------------------------
    # extract data

    output_data <- old_output_data[["2__District_Pindex_yearly"]]
    output_data_change <- old_output_data[["3__District_Change_yearly"]]

    #--------------------------------------------------
    # merge geographical information

    merging_geo <- function(out_data) {
        out_data_sf <- out_data |>
            merge(
                districts_cleaned,
                by = "kid2019",
                all.x = TRUE
            )

        # define geometry
        out_data_sf <- sf::st_set_geometry(out_data_sf, out_data_sf$geometry)
    
        # return
        return(out_data_sf)
    }

    output_data_sf <- merging_geo(output_data)
    output_data_change_sf <- merging_geo(output_data_change)

    #--------------------------------------------------
    # function to plot change

    plotting_pindex <- function(
        out_data,
        var_name,
        figure_name,
        year,
        change = TRUE
    ) {
        if (change == TRUE) {
            legend_name <- "Pindex (change in %)"
            br <- seq(-60, 350, 50)
            lim <- c(-60, 350)
            figure_add_name <- "_change_"
            folder_name <- "change"
        } else {
            legend_name <- "Pindex"
            br <- seq(-100, 320, 50)
            lim <- c(-100, 320)
            figure_add_name <- "_level_"
            folder_name <- "level"
        }

        pindex_plot <- ggplot()+
            geom_sf(
                data = out_data |>
                    dplyr::filter(year == year),
                aes(
                    geometry = geometry,
                    fill = .data[[var_name]]
                )
            )+
            scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                name = legend_name,
                breaks = br,
                limits = lim
            )+
            theme_void()+
            theme(
                legend.key.size = unit(1, "cm"),
                legend.title = element_text(size = 18, vjust = 0),
                legend.text = element_text(size = 16, angle = 90),
                legend.position = "bottom"
            )

        ggsave(
            plot = pindex_plot,
            file.path(
                config_paths()[["output_path"]],
                paste0(housing_type, "_rebuild"),
                "maps",
                folder_name,
                paste0(figure_name, figure_add_name, year, ".png")
            ),
            dpi = config_globals()[["owndpi"]]
        )
    }
    

    #--------------------------------------------------
    # plotting change
    # NOTE: For change exclude 2008 because its 0 (reference period)

    years_change <- unique(output_data_change$year)[
        unique(output_data_change$year) != 2008
    ]

    for (year in years_change) {
        plotting_pindex(
            out_data = output_data_change_sf,
            var_name = "pindex_change",
            figure_name = "District_pindex_V12",
            year = year,
            change = TRUE
        )

        plotting_pindex(
            out_data = output_data_change_sf,
            var_name = "weighted_pindex_change",
            figure_name = "District_pindex_New",
            year = year,
            change = TRUE
        )
    }

    #--------------------------------------------------
    # plotting level

    years <- unique(output_data$year)

    for (year in years) {
        plotting_pindex(
            out_data = output_data_sf,
            var_name = "pindex",
            figure_name = "District_pindex_V12",
            year = year,
            change = FALSE
        )

        plotting_pindex(
            out_data = output_data_sf,
            var_name = "new_pindex",
            figure_name = "District_pindex_New",
            year = year,
            change = FALSE
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}