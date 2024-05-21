test_plotting_time_effects <- function(
    output_data = NA
) {
    #' @title Plotting time effects
    #' 
    #' @description This function plots time effects for all housing types and
    #' comparing the estimates based on the original data and the grid data.
    #' 
    #' @param output_data List with estimated time effects
    #' 
    #' @return NULL, graphs
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # loop through time estimates
    
    for (result in results) {
        #--------------------------------------------------
        # global variables

        if (grepl("yearly", result)) {
            time_label <- "year"
        } else if (grepl("quarterly", result)) {
            time_label <- "quarter"
        }

        #--------------------------------------------------
        # extract and reshape data

        time_effects <- output_data[[result]] |>
            dplyr::select(1, c("timeeff", "new_timeeff")) |>
            tidyr::pivot_longer(
                cols = -1,
                names_to = "coef_origin",
                values_to = "timeeff"
            )
    
        #--------------------------------------------------
        # base plot which is identical for both time periods (year and quarters)

        base_plot <- ggplot(
            data = time_effects,
            aes(
                x = .data[[time_label]],
                y = timeeff,
                col = coef_origin
            )
        )+
            geom_line(linewidth = 1.2)+
            geom_point(size = 3)+
            geom_hline(
                yintercept = 0,
                linewidth = 0.6,
                linetype = "solid",
                col = "grey80"
            )+
            scale_color_manual(
                values = c(
                    "timeeff" = config_globals()[["java_five_colors"]][3],
                    "new_timeeff" = config_globals()[["java_five_colors"]][5]
                ),
                labels = c(
                    "timeeff" = "Estimates (V12)",
                    "new_timeeff" = "Estimates based on grids"
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

        #--------------------------------------------------
        # add time labels for specific periods

        if (grepl("yearly", result)) {
            expanded_plot <- base_plot+
                scale_x_continuous(
                    breaks = seq(2007, 2023, 2)
                )
        } else {
            unique_times <- unique(construction_costs_long$yearquart)
            breaks_to_show <- unique_times[grepl("-01$", unique_times)]
            breaks_to_show <- breaks_to_show[seq(1, length(breaks_to_show), 2)]

            expanded_plot <- base_plot+
                scale_x_continuous(
                    breaks = breaks_to_show
                )
        }

        #--------------------------------------------------
        # export

        ggsave(
            expanded_plot,
            file.path(
                config_paths()[["output_path"]],
                "Combined_rebuild",
                "graphs",
                paste0("combined_time_effects_", time_label, ".png")
            ),
            dpi = owndpi
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}
