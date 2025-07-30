testing_completeness_output <- function() {
    #' @title Testing completeness of exported output
    #' 
    #' @description This function tests the completeness of the exported output
    #' files. It checks whether the expected number of files is exported,
    #' whether the files are exported for all housing types, and whether the files
    #' are exported for all anonymization types.
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # get all files in exported output folder

    all_files <- list.files(
        file.path(
            config_paths()[["output_path"]],
            "export"
        )
    )

    #--------------------------------------------------
    # clean file names

    # remove archive files
    all_files <- all_files[!grepl("archive", all_files)]

    # drop file extensions
    clean_files <- c()
    for (file in all_files) {
        clean_files <- c(
            clean_files,
            stringr::str_split(file, "\\.")[[1]][1]
        )
    }

    #--------------------------------------------------
    # check number of total files

    total_num_files <- 100 # expected value
    targets::tar_assert_true(
        length(clean_files) == total_num_files,
        msg = glue::glue(
            "The number of totally exported files is not correct. ",
            "Expected: 100, Actual: {length(clean_files)}.",
            " (Error code: tco#1)"
        )
    )

    #--------------------------------------------------
    # check number of files by what is measured (ABS: absolute values, DEV:
    # deviations, DEV_PERC: percentage deviations)

    # absolute values
    files_abs <- clean_files[grepl("ABS", clean_files)]
    
    # deviations
    # NOTE: disregard percentage diavtions here
    files_dev <- clean_files[grepl("DEV", clean_files)][
        !(
            clean_files[grepl("DEV", clean_files)] %in%
            clean_files[grepl("DEV_PERC", clean_files)]
        )
    ]
    
    # deviations in percent
    # NOTE: disregard absolute deviations here
    files_dev_perc <- clean_files[grepl("DEV_PERC", clean_files)][
        !clean_files[grepl("DEV_PERC", clean_files)] %in% files_dev
    ]
    
    # define expected values
    total_num_files_abs <- 20
    total_num_files_dev <- 48
    total_num_files_dev_perc <- 32

    # test that total number of files is still correct
    targets::tar_assert_true(
        (
            length(files_abs) + length(files_dev) + length(files_dev_perc)
        ) == total_num_files,
        msg = glue::glue(
            "The number of exported files by type is not correct. ",
            "Expected: {total_num_files}, Actual: {length(files_abs) + length(files_dev) + length(files_dev_perc)}. ",
            " (Error code: tco#2)"
        )
    )

    # test that number of files by type is correct
    targets::tar_assert_true(
        length(files_abs) == total_num_files_abs,
        msg = glue::glue(
            "The number of exported absolute value files is not correct. ",
            "Expected: {total_num_files_abs}, Actual: {length(files_abs)}. ",
            " (Error code: tco#3)"
        )
    )

    targets::tar_assert_true(
        length(files_dev) == total_num_files_dev,
        msg = glue::glue(
            "The number of exported deviation files is not correct. ",
            "Expected: {total_num_files_dev}, Actual: {length(files_dev)}. ",
            " (Error code: tco#4)"
        )
    )

    targets::tar_assert_true(
        length(files_dev_perc) == total_num_files_dev_perc,
        msg = glue::glue(
            "The number of exported percentage deviation files is not correct. ",
            "Expected: {total_num_files_dev_perc}, Actual: {length(files_dev_perc)}. ",
            " (Error code: tco#5)"
        )
    )

    #--------------------------------------------------
    # check that all files are exported for all housing types

    # define expected values
    total_num_files_housing_types <- 12
    total_num_files_grids <- 56
    total_num_files_ci <- 8

    # loop through all housing types
    for (housing_type in helpers_target_names()[["static_housing_types_labels"]]) {
        files <- clean_files[grepl(toupper(housing_type), clean_files)]

        targets::tar_assert_true(
            length(files) == total_num_files_housing_types,
            msg = glue::glue(
                "The number of exported files for housing type {housing_type} is not correct. ",
                "Expected: {total_num_files_housing_types}, Actual: {length(files)}. ",
                " (Error code: tco#6)"
            )
        )
    }

    # check the same for grids
    # NOTE: includes 16 files for CI at grid level
    files_grids <- clean_files[grepl("GRIDS", clean_files)]
    targets::tar_assert_true(
        length(files_grids) == total_num_files_grids,
        msg = glue::glue(
            "The number of exported files for grids is not correct. ",
            "Expected: {total_num_files_grids}, Actual: {length(files_grids)}. ",
            " (Error code: tco#7)"
        )
    )
    
    # check the same for combined index (CombInd)
    files_ci <- clean_files[grepl("COMBIND", clean_files)]
    targets::tar_assert_true(
        length(files_ci) == total_num_files_ci,
        msg = glue::glue(
            "The number of exported files for combined index (CI) is not correct. ",
            "Expected: {total_num_files_ci}, Actual: {length(files_ci)}. ",
            " (Error code: tco#8)"
        )
    )

    #--------------------------------------------------
    # check that all files for all anonymization types are exported

    # define expected values
    total_num_files_anonymization <- 50

    for (anonym_type in c("SUF", "PUF")) {
        files <- clean_files[grepl(anonym_type, clean_files)]

        targets::tar_assert_true(
            length(files) == total_num_files_anonymization,
            msg = glue::glue(
                "The number of exported files for anonymization type {anonym_type} is not correct. ",
                "Expected: {total_num_files_anonymization}, Actual: {length(files)}. ",
                " (Error code: tco#9)"
            )
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}