testing_regional_effects_time <- function(
    old_output_data = NA,
    housing_type = NA
) {
    #' @title Plotting regional effects (by year)
    #' 
    #' @description This function plots regional effects for all housing types
    #' aggregated to the yearly level. Plots are generated for the levels and
    #' changes of the Pindex.
    #' 
    #' @param old_output_data List with estimated regional effects
    #' @param housing_type Housing type
    #' 
    #' @return NULL, graphs
    #' @author Patrick Thiel

    #--------------------------------------------------
    # extract data

    output_data <- old_output_data[["2__District_Pindex_yearly"]]
    output_data_change <- old_output_data[["3__District_Change_yearly"]]

    #--------------------------------------------------
    # summarize data to yearly level

    year_effects <- output_data |>
        dplyr::group_by(year) |>
        dplyr::summarise(
            pindex = median(pindex, na.rm = TRUE),
            new_pindex = median(new_pindex, na.rm = TRUE)
        ) |>
        tidyr::pivot_longer(
            cols = -1,
            names_to = "coef_origin",
            values_to = "pindex"
        ) |>
        as.data.frame()

    year_effects_change <- output_data_change |>
        dplyr::group_by(year) |>
        dplyr::summarise(
            pindex_change = median(pindex_change, na.rm = TRUE),
            weighted_pindex_change = median(weighted_pindex_change, na.rm = TRUE)
        ) |>
        tidyr::pivot_longer(
            cols = -1,
            names_to = "coef_origin",
            values_to = "pindex"
        ) |>
        as.data.frame()

    #--------------------------------------------------
    # plotting function

    plotting_function <- function(year_data, change = TRUE, figure_name) {
        if (change == TRUE) {
            old_pindex_name <- "pindex_change"
            new_pindex_name <- "weighted_pindex_change"
            br <- seq(-10, 130, 50)
            lim <- c(-10, 130)
            figure_add_name <- "_change_"
        } else {
            old_pindex_name <- "pindex"
            new_pindex_name <- "new_pindex"
            br <- seq(-30, -1, 10)
            lim <- c(-30, -1)
            figure_add_name <- "_level_"
        }

        # generate plot
        base_plot <- ggplot(
                data = year_data,
                aes(
                    x = year,
                    y = pindex,
                    col = as.factor(coef_origin),
                    group = as.factor(coef_origin)
                )
            )+
                geom_line(linewidth = 1.2)+
                geom_point(size = 3)+
                scale_y_continuous(
                    breaks = br,
                    limits = lim
                )+
                scale_x_continuous(
                    breaks = seq(2008, 2023, 1)
                )
                scale_color_manual(
                    values = c(
                        old_pindex_name = config_globals()[["java_five_colors"]][3],
                        new_pindex_name = config_globals()[["java_five_colors"]][5]
                    ),
                    labels = c(
                        old_pindex_name = "Estimates (V12)",
                        new_pindex_name = "Estimates based on grids (aggregated)"
                    ),
                    name = ""
                )+
            labs(
                y = "Estimated effect",
                x = ""
            )+
            theme_classic()+
            theme(
                panel.border = element_rect(linewidth = 1, fill = NA),
                axis.text = element_text(size = 17),
                axis.title = element_text(size = 19),
                legend.key.size = unit(1, "cm"),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 16),
                axis.text.x = element_text(angle = 90),
                legend.position = "bottom"
            )+
            guides(legend_guide = guide_legend(nrow = 2))

        # export
        ggsave(
            plot = pindex_plot,
            file.path(
                config_paths()[["output_path"]],
                paste0(housing_type, "_rebuild"),
                "graphs",
                paste0(figure_name, figure_add_name, ".png")
            ),
            dpi = config_globals()[["owndpi"]]
        )
    }

    #--------------------------------------------------
    # apply plotting function

    plotting_function(
        year_effects,
        change = FALSE,
        figure_name = "District_time_regional_effects"
    )
    plotting_function(
        year_effects_change,
        change = TRUE,
        figure_name = "District_time_regional_effects_change"
    )

    #--------------------------------------------------
    # return

    return(NULL)
}

