helpers_summary_stats <- function(
    data = NA,
    housing_type = NA,
    change_effects = NA
) {
    #' @title Summary statistics
    #' 
    #' @description This function calculates summary statistics for the pindex variable.
    #' 
    #' @param data Dataframe with the pindex variable (housing type specific).
    #' @param housing_type Character string with the housing type.
    #' @param change_effects Logical if supplied data contains change in regional
    #' effects
    #' 
    #' @return Dataframe with summary statistics.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # rename pindex variable for change data
    # rename pindex variable (only for CI)
    # because variable is named differently because its the weighted pindex

    if (change_effects == FALSE) {
        if (housing_type == "CI") {
            data <- data |>
                dplyr::rename(
                    pindex = weighted_pindex
                )
        }
    } else {      
        if (housing_type == "CI") {
            # remove original pindex variable which is not change
            data <- data |>
                dplyr::select(-weighted_pindex) |>
                dplyr::rename(
                    pindex = weighted_pindex_change
                )
        } else {
            data <- data |>
                dplyr::select(-pindex) |>
                dplyr::rename(
                    pindex = pindex_change
                )
        }
    }

    #--------------------------------------------------
    # basic calculate summary statistics

    stats <- c(
        "min",
        "1st_quartile",
        "mean",
        "median",
        "3rd_quartile",
        "max",
        "iqr"
    )

    values <- c(
        min(data$pindex, na.rm = TRUE),
        quantile(data$pindex, probs = 0.25, na.rm = TRUE),
        mean(data$pindex, na.rm = TRUE),
        median(data$pindex, na.rm = TRUE),
        quantile(data$pindex, probs = 0.75, na.rm = TRUE),
        max(data$pindex, na.rm = TRUE),
        quantile(data$pindex, probs = 0.75, na.rm = TRUE) - quantile(data$pindex, probs = 0.25, na.rm = TRUE)
    )

    # combine stats and values in dataframe
    stats_df <- as.data.frame(cbind(
        "stats" = stats,
        "values" = values
        )
    ) |>
    dplyr::mutate(
        values = as.numeric(values)
    )

    # reset row numbers
    rownames(stats_df) <- NULL

    #--------------------------------------------------
    # classify outliers
    # bottom outlier = Q1 - 1.5 * IQR
    # top outlier = Q3 + 1.5 * IQR

    bottom_outlier_threshold <- (stats_df$values[stats_df$stats == "1st_quartile"]
        - (1.5 * stats_df$values[stats_df$stats == "iqr"]))

    top_outlier_threshold <- (stats_df$values[stats_df$stats == "3rd_quartile"]
        + (1.5 * stats_df$values[stats_df$stats == "iqr"]))

    data <- data |>
        dplyr::mutate(
            outlier = dplyr::case_when(
                pindex < bottom_outlier_threshold ~ "bottom",
                pindex > top_outlier_threshold ~ "top",
                TRUE ~ "no"
            )
        )

    # add outlier thresholds to stats_df
    stats_df <- rbind(
        stats_df,
        as.data.frame(cbind(
            "stats" = c("bottom_outlier_threshold", "top_outlier_threshold"),
            "values" = c(bottom_outlier_threshold, top_outlier_threshold)
        ))
    )

    #--------------------------------------------------
    # calculate the share of values that might be outliers

    outlier_share <- data |>
        dplyr::filter(
            outlier != "no"
        ) |>
        dplyr::group_by(outlier) |>
        dplyr::summarise(
            outlier_n = dplyr::n(),
            outlier_perc = dplyr::n() / nrow(data) * 100
        ) |>
        tidyr::pivot_longer(
            cols = c("outlier_n", "outlier_perc"),
            names_to = "outlier_stat",
            values_to = "values"
        ) |>
        # create name column
        dplyr::mutate(
            stats = paste0(outlier, "_", outlier_stat)
        ) |>
        dplyr::select(-c("outlier", "outlier_stat")) |>
        dplyr::relocate(stats) |>
        as.data.frame()

    # combine with stats_sf
    stats_df <- rbind(stats_df, outlier_share)

    #--------------------------------------------------
    # specify housing type
    # set type

    stats_df <- stats_df |>
        dplyr::mutate(
            housing_type = housing_type,
            values = as.numeric(values)
        ) |>
        dplyr::relocate(housing_type)

    #--------------------------------------------------
    # return

    return(stats_df)
}