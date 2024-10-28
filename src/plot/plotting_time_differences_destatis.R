plotting_time_differences_destatis <- function(
    HK_effects = NA,
    WK_effects = NA,
    WM_effects = NA,
    destatis_effects = NA
) {
    #' @title Plotting Time Differences Destatis
    #' 
    #' @description This function plots the time differences between the RWI
    #' and Destatis for the different housing types.
    #' 
    #' @param HK_effects Dataframe, RWI effects for house sales
    #' @param WK_effects Dataframe, RWI effects for apartment sales
    #' @param WM_effects Dataframe, RWI effects for apartment rents
    #' @param destatis_effects List, cleaned Destatis data
    #' 
    #' @return List, merged results
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # merge price effects

    merged_results <- list()

    for (TYPE in c("WK", "HK")) {
        if (TYPE == "WK") {
            rwi_effects <- WK_effects
            destatis_effects_name <- "apartment_price_index_"
        } else {
            rwi_effects <- HK_effects
            destatis_effects_name <- "house_price_index_"
        }

        # merge annual time effects
        merged_results[[paste0(TYPE, "_year")]] <- rbind(
            rwi_effects[["ejahr"]] |>
                dplyr::select(year, timeeff) |>
                dplyr::mutate(
                    data_type = "rwi_index",
                    timeeff = timeeff + 100
                ),
            destatis_effects[[paste0(destatis_effects_name, "years")]] |>
                dplyr::filter(year >= 2008)
        ) |>
        # clean up
        dplyr::filter(!is.na(data_type))

        # merge quarterly time effects
        merged_results[[paste0(TYPE, "_quarter")]] <- rbind(
            rwi_effects[["e_year_quarter"]] |>
                dplyr::select(quarter, timeeff) |>
                dplyr::mutate(
                    data_type = "rwi_index",
                    timeeff = timeeff + 100
                ),
            destatis_effects[[paste0(destatis_effects_name, "quarters")]] |>
                dplyr::filter(quarter >= "2008-01")
        ) |>
        # clean up
        dplyr::filter(!is.na(data_type))
    }

    # merge apartment rents
    merged_results[["WM_year"]] <- rbind(
        WM_effects[["ejahr"]] |>
            dplyr::select(year, timeeff) |>
            dplyr::mutate(
                data_type = "rwi_index",
                timeeff = timeeff + 100
            ),
        destatis_effects[["net_rents_index_states_years"]] |>
            dplyr::mutate(
                data_type = "destatis_index"
            ) |>
            dplyr::filter(year >= 2008)
    )

    #--------------------------------------------------
    # plotting function

    plotting_function <- function(
        data,
        WM = FALSE,
        period = c("year", "quarter"),
        color_values = NA,
        color_labels = NA,
        figure_name = NA
    ) {
        #' @title Plotting Function
        #' 
        #' @description This function plots the time effects for the different
        #' housing types.
        #' 
        #' @param data Dataframe, data to plot
        #' @param WM Logical, whether the data is for apartment rents
        #' @param period Character, period to plot
        #' @param color_values Vector, color values matching data types
        #' @param color_labels Vector, color labels matching data types
        #' @param figure_name Character, name of the figure to export
        #' 
        #' @return NULL, direct export to directory
        #' @author Patrick Thiel
        
        #--------------------------------------------------
        # define period specific layouts
        if (period == "year") {
            intercept_pos <- "2015"
            ylabel <- "Hedonic Price Index (2015 = 100)"
        } else {
            intercept_pos <- "2015-01"
            ylabel <- "Hedonic Price Index (2015 = 100)"
        }

        # define WM specific layouts
        if (WM == TRUE) {
            intercept_pos <- "2020"
            ylabel <- "Hedonic Rent Index (2020 = 100)"
        }

        plot <- ggplot(
            data = data,
            aes(
                x = .data[[period]],
                y = timeeff,
                col = data_type,
                group = data_type
            )
        )+
            geom_line(linewidth = 1)+
            geom_point(size = 2)+
            geom_hline(
                yintercept = 100,
                linetype = "dashed",
                color = "black"
            )+
            geom_vline(
                xintercept = intercept_pos,
                linetype = "dashed",
                color = "black"
            )+
            scale_y_continuous(
                limits = c(0, 200),
                breaks = seq(0, 200, 20)
            )+
            scale_color_manual(
                values = color_values,
                labels = color_labels,
                name = ""
            )+
            labs(
                x = "",
                y = ylabel
            )+
            theme_light()+
            theme(
                legend.position = "bottom",
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                axis.text.x = element_text(angle = 90),
                legend.text = element_text(size = 12),
                legend.key.size = unit(1, "cm")
            )

        # export figure
        ggsave(
            plot = plot,
            file.path(
                config_paths()[["output_path"]],
                "testing",
                paste0(figure_name, "_destatis_time_effects.png")
            ),
            dpi = config_globals()[["owndpi"]]
        )

        #--------------------------------------------------
        # return

        return(NULL)
    }

    #--------------------------------------------------
    # plot time effects

    for (dta_name in names(merged_results)) {
        # extract period
        period <- stringr::str_extract(dta_name, "year|quarter")

        # subset for data
        dta <- merged_results[[dta_name]]

        # subset further for HK and WK
        # because Destatis has multiple data types
        if (grepl("HK", dta_name)) {
            dta_general_types <- dta |>
                dplyr::filter(data_type %in% c("rwi_index", "house_price_index"))

            dta_specific_types <- dta |>
                dplyr::filter(data_type %in% c("rwi_index", "price_index_new_buildings", "price_index_existing_buildings"))

            # define color values and labels
            color_values_general <- c(
                "rwi_index" = config_globals()[["eqypt_four_colors"]][2],
                "house_price_index" = config_globals()[["eqypt_four_colors"]][1]
            )

            color_labels_general <- c(
                "rwi_index" = "House Sales Index (RWI)",
                "house_price_index" = "House Price Index (Destatis)"
            )

            color_values_specific <- c(
                "rwi_index" = config_globals()[["eqypt_four_colors"]][2],
                "price_index_new_buildings" = config_globals()[["eqypt_four_colors"]][1],
                "price_index_existing_buildings" = config_globals()[["eqypt_four_colors"]][3]
            )

            color_labels_specific <- c(
                "rwi_index" = "House Sales Index (RWI)",
                "price_index_new_buildings" = "Price Index New Buildings (Destatis)",
                "price_index_existing_buildings" = "Price Index Existing Buildings (Destatis)"
            )

            # plot general types
            plotting_function(
                data = dta_general_types,
                WM = FALSE,
                period = period,
                color_values = color_values_general,
                color_labels = color_labels_general,
                figure_name = paste0(dta_name, "_general")
            )

            # plot specific types
            plotting_function(
                data = dta_specific_types,
                WM = FALSE,
                period = period,
                color_values = color_values_specific,
                color_labels = color_labels_specific,
                figure_name = paste0(dta_name, "_specific")
            )
        } else if (grepl("WK", dta_name)){
            # define color values and labels
            color_values <- c(
                "rwi_index" = config_globals()[["eqypt_four_colors"]][2],
                "price_index_owner_occupied_building" = config_globals()[["eqypt_four_colors"]][1]
            )

            color_labels <- c(
                "rwi_index" = "House Sales Index (RWI)",
                "price_index_owner_occupied_building" = "Price Index Owner Occupied Building (Destatis)"
            )

            # plot WK
            plotting_function(
                data = dta,
                WM = FALSE,
                period = period,
                color_values = color_values,
                color_labels = color_labels,
                figure_name = dta_name
            )
        } else {
            # define color values and labels
            color_values <- c(
                "rwi_index" = config_globals()[["eqypt_four_colors"]][2],
                "destatis_index" = config_globals()[["eqypt_four_colors"]][1]
            )

            color_labels <- c(
                "rwi_index" = "House Sales Index (RWI)",
                "destatis_index" = "Net Rent Index (Destatis)"
            )

            # plot WM
            plotting_function(
                data = dta,
                WM = TRUE,
                period = period,
                color_values = color_values,
                color_labels = color_labels,
                figure_name = dta_name
            )
        }
    }

    #--------------------------------------------------
    # return

    return(merged_results)
}