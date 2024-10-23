plotting_combined_individual_effects <- function(
    WM_estimated_time_effects = NA,
    HK_estimated_time_effects = NA,
    WK_estimated_time_effects = NA,
    combined_time_effects = NA
) {
    #' @title Plotting time effects (combined individual effects)
    #' 
    #' @description This function plots time effects for all housing types and
    #' the combined index.
    #' 
    #' @param WM_estimated_time_effects Time effects for apartment rents
    #' @param HK_estimated_time_effects Time effects for house sales
    #' @param WK_estimated_time_effects Time effects for apartment sales
    #' @param combined_time_effects Combined time effects
    #'  
    #' @return NULL, graphs
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # loop through results
    for (result in names(WM_estimated_time_effects)) {
        if (result == "ejahr") {
            time_label <- "year"
        } else {
            time_label <- "quarter"
        }

        #--------------------------------------------------
        # cleaning function

        cleaning_function <- function(
            results_data = NA,
            housing_type = NA
        ) {
            results_prep <- results_data |>
                dplyr::select(
                    !!rlang::sym(time_label),
                    timeeff
                ) |>
                dplyr::mutate(
                    coef_origin = housing_type
                )

            return(results_prep)
        }

        #--------------------------------------------------
        # combine all data

        time_effects <- rbind(
            cleaning_function(WM_estimated_time_effects[[result]], "WM"),
            cleaning_function(HK_estimated_time_effects[[result]], "HK"),
            cleaning_function(WK_estimated_time_effects[[result]], "WK"),
            cleaning_function(combined_time_effects[[result]], "combined")
        )

        if (result == "ejahr") {
            time_effects <- time_effects |>
                dplyr::mutate(
                    year = as.numeric(.data[[time_label]])
                )
        }

        #--------------------------------------------------
        # generate plot

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
                    "WM" = config_globals()[["java_five_colors"]][1],
                    "HK" = config_globals()[["java_five_colors"]][2],
                    "WK" = config_globals()[["java_five_colors"]][3],
                    "combined" = config_globals()[["java_five_colors"]][5]
                ),
                labels = c(
                    "WM" = "Apartment rents (WM)",
                    "HK" = "House salses (HK)",
                    "WK" = "Apartment sales (WK)",
                    "combined" = "Combined index"
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
        # add time labels for specific periods

        if (grepl("ejahr", result)) {
            expanded_plot <- base_plot+
                scale_x_continuous(
                    breaks = seq(2008, max(time_effects$year), 1)
                )
        } else {
            unique_times <- unique(time_effects[[time_label]])
            breaks_to_show <- unique_times[grepl("-01$", unique_times)]
            breaks_to_show <- breaks_to_show[seq(1, length(breaks_to_show), 2)]

            expanded_plot <- base_plot+
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
                "CI",
                "graphs",
                paste0("Combined_individual_effects_", time_label, ".png")
            ),
            dpi = config_globals()[["owndpi"]]
        ))
    }

    #--------------------------------------------------
    # return

    return(NULL)
}