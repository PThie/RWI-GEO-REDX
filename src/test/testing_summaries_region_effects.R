testing_summaries_region_effects <- function(
    HK_region_effects = NA,
    WK_region_effects = NA,
    WM_region_effects = NA,
    CI_region_effects = NA
) {
    #' @title Testing summaries for region effects
    #' 
    #' @description This function calculates summary statistics for the region
    #' effects (overall summary stats)
    #' 
    #' @param HK_region_effects Dataframe with the region effects for the HK region.
    #' @param WK_region_effects Dataframe with the region effects for the WK region.
    #' @param WM_region_effects Dataframe with the region effects for the WM region.
    #' @param CI_region_effects Dataframe with the region effects for the CI region.
    #' 
    #' @note The summary stats function could be expanded to yearly summary
    #' stats by filtering the data by year and applying the summary stats function.
    #' However, this is not done to avoid an overkill of output numbers.
    #' 
    #' @return List with summary statistics for each housing type.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # group datasets together in list

    region_effects <- list(
        "HK" = HK_region_effects,
        "WK" = WK_region_effects,
        "WM" = WM_region_effects,
        "CI" = CI_region_effects
    )

    #--------------------------------------------------
    # summary statistics function

    summary_stats <- function(data = NA, housing_type = NA) {
        #' @title Summary statistics
        #' 
        #' @description This function calculates summary statistics for the pindex variable.
        #' 
        #' @param data Dataframe with the pindex variable (housing type specific).
        #' @param housing_type Character string with the housing type.
        #' 
        #' @return Dataframe with summary statistics.
        #' @author Patrick Thiel
        
        #--------------------------------------------------
        # rename pindex variable (only for CI)
        # because variable is named differently because its the weighted pindex

        if (housing_type == "CI") {
            data <- data |>
                dplyr::rename(
                    pindex = weighted_pindex
                )
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

    #--------------------------------------------------
    # calculate overall summary statistics for each housing type

    overall_summary_stats <- list()
    for (housing_type in names(region_effects)) {
        overall_summary_stats[[housing_type]] <- summary_stats(
            data = region_effects[[housing_type]],
            housing_type = housing_type
        )
    }

    #--------------------------------------------------
    # check for too many outliers

    for (housing_type in names(overall_summary_stats)) {
        targets::tar_assert_true(
            overall_summary_stats[[housing_type]]$values[
                overall_summary_stats[[housing_type]]$stats == "top_outlier_perc"
            ] <= config_globals()[["critical_outlier_threshold"]],
            msg = glue::glue(
                "!!! WARNING: {housing_type} has more than
                {config_globals()[['critical_outlier_threshold']]}% top outliers"
            )
        )
    }

    #--------------------------------------------------
    # write report

    # create empty text file for output
    # because otherwise every time the pipeline is run the file is appended
    # and potentially old output is stored
    directory <- file.path(
        config_paths()[["output_path"]],
        "testing",
        "overall_summary_stats.txt"
    )

    write("", directory, append = FALSE)

    for (housing_type in names(overall_summary_stats)) {
        header <- glue::glue(
            "
            #--------------------------------------------------
            Summary statistics for {housing_type} region effects
            #--------------------------------------------------
            "
        )

        # export header
        write(
            header,
            file.path(
                config_paths()[["output_path"]],
                "testing",
                "overall_summary_stats.txt"
            ),
            append = TRUE
        )

        # export stats
        gdata::write.fwf(
            overall_summary_stats[[housing_type]],
            file.path(
                config_paths()[["output_path"]],
                "testing",
                "overall_summary_stats.txt"
            ),
            append = TRUE,
            rownames = FALSE
        )
    }

    #--------------------------------------------------
    # return

    return(overall_summary_stats)
}