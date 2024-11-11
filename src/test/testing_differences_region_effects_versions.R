testing_differences_region_effects_versions <- function(
    summary_stats = NA,
    summary_stats_prev_version = NA,
    output_text_file = NA
) {
    #' @title Comparison region effects summary
    #' 
    #' @description This function compares the summary statistics for the regional
    #' effects for the upcoming version and the previous/current version.
    #' 
    #' @param summary_stats List with dataframes of summary statistics of upcoming
    #' version
    #' @param summary_stats_prev_version List with dataframes of summary statistics
    #' of previous/current version
    #' @param output_test_file Character for name of output text file
    #' 
    #' @return List with merged summary statistics for both versions
    #' @author Patrick Thiel  

    #--------------------------------------------------
    # merging summary stats for the both versions
    # calculate deviation between upcoming version and pervious/current version

    stats_list <- list()
    for (housing_type in names(summary_stats)) {
        stats <- merge(
            summary_stats[[housing_type]],
            summary_stats_prev_version[[housing_type]] |>
                dplyr::select(-housing_type) |>
                dplyr::rename(values_prev_version = values),
            by = "stats"
        ) |>
        dplyr::mutate(
            deviation_curr_prev_version = values - values_prev_version,
            deviation_curr_prev_version_perc = ((values - values_prev_version) / values_prev_version) * 100,
            deviation_above_critical_threshold = deviation_curr_prev_version_perc > config_globals()[["critical_outlier_threshold"]]
        )
        stats_list[[housing_type]] <- stats
    }

    #--------------------------------------------------
    # check if mean/median is too far off (defined through critical threshold in globals)

    for (housing_type in names(stats_list)) {
        targets::tar_assert_true(
            stats_list[[housing_type]]$deviation_curr_prev_version_perc[
                stats_list[[housing_type]]$stats == "mean"
            ] <= config_globals()[["critical_outlier_threshold"]],
            msg = glue::glue(
                "!!! WARNING: {housing_type} shows stronger deviation in MEAN
                from previous version, i.e., deviation larger than
                {config_globals()[['critical_outlier_threshold']]}%"
            )
        )
    }

    for (housing_type in names(stats_list)) {
        targets::tar_assert_true(
            stats_list[[housing_type]]$deviation_curr_prev_version_perc[
                stats_list[[housing_type]]$stats == "median"
            ] <= config_globals()[["critical_outlier_threshold"]],
            msg = glue::glue(
                "!!! WARNING: {housing_type} shows stronger deviation in MEDIAN
                from previous version, i.e., deviation larger than
                {config_globals()[['critical_outlier_threshold']]}%"
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
        paste0(output_text_file, ".txt")
    )

    write("", directory, append = FALSE)

    for (housing_type in names(stats_list)) {
        header <- glue::glue(
            "
            #--------------------------------------------------
            Summary statistics for {housing_type} region effects
            Comparison between {config_globals()[['current_version']]} and {config_globals()[['next_version']]}
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
            stats_list[[housing_type]],
            directory,
            append = TRUE,
            rownames = FALSE
        )
    }

    #--------------------------------------------------
    # retrun

    return(stats_list)
}