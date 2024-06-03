testing_time_effects <- function(
    old_output_data = NA,
    housing_type = NA
) {
    #' @title Plotting time effects
    #' 
    #' @description This function plots time effects for all housing types and
    #' comparing the estimates based on the original data and the grid data.
    #' 
    #' @param old_output_data List with estimated time effects
    #' @param housing_type Housing type
    #' 
    #' @return NULL, graphs
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # loop through time estimates
    results <- c(
        "1__District_TimeEff_yearly",
        "1__District_TimeEff_quarterly"
    )
    
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

        time_effects <- old_output_data[[result]] |>
            dplyr::select(1, c("timeeff", "new_timeeff")) |>
            tidyr::pivot_longer(
                cols = -1,
                names_to = "coef_origin",
                values_to = "timeeff"
            )

        #--------------------------------------------------
        # calculate the differences by year

        time_diff <- old_output_data[[result]] |>
            dplyr::select(1, c("timeeff", "new_timeeff")) |>
            dplyr::mutate(
                diff = timeeff - new_timeeff
            )
    
        #--------------------------------------------------
        # base plot which is identical for both time periods (year and quarters)

        base_plot <- ggplot(
            data = time_effects,
            aes(
                x = .data[[time_label]],
                y = timeeff,
                col = as.factor(coef_origin),
                group = as.factor(coef_origin)
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
            scale_y_continuous(
                breaks = seq(-5, 100, 10),
                limits = c(-7, 100)
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
        guides(col = guide_legend(nrow = 2, byrow = TRUE))
    
        #--------------------------------------------------
        # plot differences

        diff_plot <- ggplot(
            data = time_diff,
            aes(
                x = .data[[time_label]],
                y = diff,
                group = 1
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
            scale_y_continuous(
                breaks = seq(-8, 2, 1),
                limits = c(-9, 2)
            )+
            labs(
                y = "Differnce in estimated effect (V12 - VGrids)",
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
            )

        #--------------------------------------------------
        # add time labels for specific periods

        if (grepl("yearly", result)) {
            expanded_plot <- base_plot+
                scale_x_continuous(
                    breaks = seq(2008, 2023, 1)
                )

            expanded_diff <- diff_plot+
                scale_x_continuous(
                    breaks = seq(2008, 2023, 1)
                )
        } else {
            unique_times <- unique(time_effects[[time_label]])
            breaks_to_show <- unique_times[grepl("-01$", unique_times)]
            breaks_to_show <- breaks_to_show[seq(1, length(breaks_to_show), 2)]

            expanded_plot <- base_plot+
                scale_x_discrete(
                    breaks = breaks_to_show
                )

            expanded_diff <- diff_plot+
                scale_x_discrete(
                    breaks = breaks_to_show
                )
        }

        #--------------------------------------------------
        # export

        suppressMessages(ggsave(
            plot = expanded_plot,
            filename = file.path(
                config_paths()[["output_path"]],
                "Combined_rebuild",
                "graphs",
                paste0(housing_type, "_combined_time_effects_", time_label, ".png")
            ),
            dpi = config_globals()[["owndpi"]]
        ))

        suppressMessages(ggsave(
            plot = expanded_diff,
            filename = file.path(
                config_paths()[["output_path"]],
                "Combined_rebuild",
                "graphs",
                paste0(housing_type, "_combined_time_effects_", time_label, "_difference.png")
            ),
            dpi = config_globals()[["owndpi"]]
        ))      
    }

    #--------------------------------------------------
    # return

    return(NULL)
}
