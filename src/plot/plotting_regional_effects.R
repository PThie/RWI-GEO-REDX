plotting_regional_effects <- function(
    HK_regional_effects = NA,
    HK_regional_effects_change = NA,
    districts_cleaned = NA
) {
    #' @title Plotting regional effects and their changes
    #' 
    #' @description This function plots the regional effects and their changes
    #' at the district level.
    #' 
    #' @param HK_regional_effects List with regional effects for house sales
    #' @param HK_regional_effects_change List with regional effects changes for
    #' house sales
    #' @param districts_cleaned Spatial Dataframe with regional information of
    #' districts
    #' 
    #' @note Currently only for one type (house sales and one year (max year))
    #' 
    #' @return NULL, direct plot return
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # filter for latest year

    HK_regional_effects_current <- HK_regional_effects |>
        dplyr::filter(year == config_globals()[["max_year"]])

    HK_regional_effects_change_current <- HK_regional_effects_change |>
        dplyr::filter(year == config_globals()[["max_year"]])

    # combine in list
    HK_data_list <- list(
        "HK_levels" = HK_regional_effects_current,
        "HK_changes" = HK_regional_effects_change_current
    )

    #--------------------------------------------------
    # merge regional information

    for (dta in names(HK_data_list)) {
        HK_data_list[[dta]] <- HK_data_list[[dta]] |>
            merge(
                districts_cleaned,
                by = "kid2019",
                all.x = TRUE
            )

        # set geometry
        HK_data_list[[dta]] <- sf::st_set_geometry(
            HK_data_list[[dta]],
            HK_data_list[[dta]]$geometry
        ) 
    }

    #--------------------------------------------------
    # function for maps

    mapping_function <- function(
        data = NA,
        var_of_interest = NA,
        legend_name = NA,
        figure_name = NA
    ) {
        # define breaks (rounded to nearest 100)
        brk <- seq(
            round(min(data[[var_of_interest]], na.rm = TRUE), digits = -2),
            round(max(data[[var_of_interest]], na.rm = TRUE), digits = -2),
            by = 50
        )

        # generate map
        map <- ggplot()+
            geom_sf(
                data = data,
                aes(
                    geometry = geometry,
                    fill = .data[[var_of_interest]]
                )
            )+
            scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                name = legend_name,
                breaks = brk
            )+
            theme_void()+
            theme(
                legend.position = "bottom",
                legend.title = element_text(vjust = 0.9, size = 14),
                legend.text = element_text(angle = 90, size = 12)
            )

        suppressMessages(ggsave(
            plot = map,
            file.path(
                config_paths()[["output_path"]],
                "visualizations",
                paste0(figure_name, ".png")
            ),
            dpi = config_globals()[["owndpi"]]
        ))
    }

    #--------------------------------------------------
    # generate maps

    for (dta in names(HK_data_list)) {
        # define variable of interest for plotting
        if (grepl("levels", dta)) {
            var <- "weighted_pindex"
            lgd_name <- "Pindex"
            fgr_name <- paste0(
                "HK_",
                config_globals()[["max_year"]],
                "_district_levels.png"
            )
        } else {
            var <- "weighted_pindex_change"
            lgd_name <- "Pindex (change in %)"
            fgr_name <- paste0(
                "HK_",
                config_globals()[["max_year"]],
                "_district_changes.png"
            )
        }

        mapping_function(
            data = HK_data_list[[dta]],
            var_of_interest = var,
            legend_name = lgd_name,
            figure_name = fgr_name
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}