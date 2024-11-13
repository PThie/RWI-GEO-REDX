testing_summaries_region_effects <- function(
    HK_region_effects = NA,
    WK_region_effects = NA,
    WM_region_effects = NA,
    CI_region_effects = NA,
    output_text_file = NA,
    change_effects = NA
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
    #' @param output_text_file Character indicating the storage text file for the summary stats.
    #' @param change_effects Logical if supplied data contains change in regional
    #' effects
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
    # remove values for reference year
    # because they are zero by definition and therefore, shouldn't count in the
    # summary stats

    if (change_effects == TRUE) {
        for (dta in names(region_effects)) {
            region_effects[[dta]] <- region_effects[[dta]] |>
                dplyr::filter(
                    year != helpers_regional_effects_change_settings(time_period = "year")[["reference_period"]]
                )
        }
    }

    #--------------------------------------------------
    # calculate overall summary statistics for each housing type

    overall_summary_stats <- list()
    for (housing_type in names(region_effects)) {
        overall_summary_stats[[housing_type]] <- helpers_summary_stats(
            data = region_effects[[housing_type]],
            housing_type = housing_type,
            change_effects = change_effects
        )
    }

    #--------------------------------------------------
    # check for too many outliers

    # for (housing_type in names(overall_summary_stats)) {
    #     targets::tar_assert_true(
    #         overall_summary_stats[[housing_type]]$values[
    #             overall_summary_stats[[housing_type]]$stats == "top_outlier_perc"
    #         ] <= config_globals()[["critical_outlier_threshold"]],
    #         msg = glue::glue(
    #             "!!! WARNING: {housing_type} has more than
    #             {config_globals()[['critical_outlier_threshold']]}% top outliers"
    #         )
    #     )
    # }

    #--------------------------------------------------
    # write report

    # create empty text file for output
    # because otherwise every time the pipeline is run the file is appended
    # and potentially old output is stored
    directory <- file.path(
        config_paths()[["output_path"]],
        "testing",
        paste0(output_text_file, ".txt")
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
            directory,
            append = TRUE
        )

        # export stats
        gdata::write.fwf(
            overall_summary_stats[[housing_type]],
            directory,
            append = TRUE,
            rownames = FALSE
        )
    }

    #--------------------------------------------------
    # return

    return(overall_summary_stats)
}