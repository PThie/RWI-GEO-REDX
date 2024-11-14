#----------------------------------------------
# description

# This file is the main file that orchestrates the other coding files. It
# controls the data pipeline and defines the global settings.

###################################################
# PIPELINE SETTINGS
###################################################

#----------------------------------------------
# load libraries

suppressPackageStartupMessages({
    library(targets)
    library(renv)
    library(dplyr)
    library(here)
    library(tarchetypes)
    library(haven)
    library(sf)
    library(stringr)
    library(lubridate)
    library(fst)
    library(qs)
    library(fixest)
    library(openxlsx)
    library(future)
    library(future.callr)
    library(MetBrewer)
    library(ggplot2)
    library(docstring)
    library(arrow)
    library(crew)
})

#----------------------------------------------
# set working directory

setwd(here::here())

#--------------------------------------------------
# Pipeline settings

# target options
tar_option_set(
    resources = tar_resources(
        fst = tar_resources_fst(compress = 50)
    ),
    seed = 1,
    garbage_collection = TRUE,
    memory = "transient",
    controller = crew_controller_local(
        name = "my_controller",
        workers = 3,
        seconds_idle = 10
    ),
    retrieval = "worker",
    storage = "worker"
)

#----------------------------------------------
# load configurations

source(
    file.path(
        here::here(),
        "src",
        "helpers",
        "config.R"
    )
)

#----------------------------------------------
# load R scripts

sub_directories <- list.dirs(
    config_paths()[["code_path"]],
    full.names = FALSE,
    recursive = FALSE
)

for (sub_directory in sub_directories) {
    if (sub_directory != "helpers") { 
        lapply(
            list.files(
                file.path(
                    config_paths()[["code_path"]],
                    sub_directory
                ),
                pattern = "\\.R$",
                full.names = TRUE,
                ignore.case = TRUE
            ),
            source
        )
    } else {
        files <- list.files(
            file.path(
                config_paths()[["code_path"]],
                sub_directory
            ),
            pattern = "\\.R$",
            full.names = TRUE,
            ignore.case = TRUE
        )
        files <- files[
            stringr::str_detect(
                files,
                "config.R$"
            ) == FALSE
        ]
        lapply(files, source)
    }
}

###################################################
# ACTUAL PIPELINE
###################################################

#--------------------------------------------------
# Folder and file generation

targets_preparation_folders <- rlang::list2(
    # Creating empty workbooks for the exporting functions
    tar_target(
        empty_export_workbooks,
        creating_export_workbooks(),
        deployment = "main"
    ),
    tar_target(
        empty_folders,
        creating_folder_structure(),
        deployment = "main"
    )
)

#----------------------------------------------
# Preparation of the geo information

targets_preparation_geo <- rlang::list2(
    #--------------------------------------------------
    # Grids (1 x 1 km)
    tar_file_read(
        grids_raw,
        file.path(
            config_paths()[["gebiete_path"]],
            "Raster",
            "ger_1km_rectangle",
            "ger_1km_rectangle.shp"
        ),
        sf::st_read(!!.x, quiet = TRUE),
        format = "qs"
    ),
    tar_qs(
        grid_cleaned,
        cleaning_grids(grids_raw = grids_raw)
    ),
    #--------------------------------------------------
    # Municipalities
    # NOTE: Not using Gemeindeverbände as before in older REDX versions but
    # switching to Gemeinden as the new data basis is a combination of grid
    # values
    tar_file_read(
        municipalities_raw,
        file.path(
            config_paths()[["gebiete_path"]],
            "Gemeinde",
            "2019",
            "VG250_GEM.shp"
        ),
        sf::st_read(!!.x, quiet = TRUE),
        format = "qs"
    ),
    tar_qs(
        municipalities_cleaned,
        cleaning_municipalities(municipalities_raw = municipalities_raw)
    ),
    #--------------------------------------------------
    # Districts
    tar_file_read(
        districts_raw,
        file.path(
            config_paths()[["gebiete_path"]],
            "Kreis",
            "2019",
            "VG250_KRS.shp"
        ),
        sf::st_read(!!.x, quiet = TRUE),
        format = "qs"
    ),
    tar_qs(
        districts_cleaned,
        cleaning_districts(districts_raw = districts_raw)
    ),
    #--------------------------------------------------
    # Local labor market regions
    tar_file_read(
        labor_market_regions_raw,
        file.path(
            config_paths()[["gebiete_path"]],
            "Arbeitsmarktregion",
            "RWI2018",
            "shapefiles",
            "amr2.gpkg"
        ),
        sf::st_read(!!.x, quiet = TRUE),
        format = "qs"
    ),
    tar_qs(
        labor_market_regions_cleaned,
        cleaning_labor_market_regions(
            labor_market_regions_raw = labor_market_regions_raw
        )
    ),
    #--------------------------------------------------
    # Connection between grids and other regional classifications
    tar_file_read(
        grids_municipalities,
        file.path(
            config_paths()[["gebiete_path"]],
            "Zuordnung",
            "_Gemeinde",
            "2019_Grids_Municipality_Exact_unambiguous.csv"
        ),
        data.table::fread(!!.x) |>
            dplyr::select(
                ergg_1km = r1_id,
                gid2019 = AGS
            ) |>
            dplyr::mutate(
                gid2019 = stringr::str_pad(gid2019, 8, pad = "0")
            ),
        format = "fst"
    ),
    tar_file_read(
        grids_districts,
        file.path(
            config_paths()[["gebiete_path"]],
            "Zuordnung",
            "Raster_Kreis",
            "zuordnung_r1_krs_2019.csv"
        ),
        data.table::fread(!!.x) |>
            dplyr::select(
                ergg_1km = r1_id,
                kid2019 = AGS
            ) |>
            dplyr::mutate(
                kid2019 = stringr::str_pad(kid2019, 5, pad = "0")
            ),
        format = "fst"
    ),
    tar_fst(
        grids_lmr,
        connecting_grids_lmr(
            grid_cleaned = grid_cleaned,
            labor_market_regions_cleaned = labor_market_regions_cleaned
        )
    )
)

#--------------------------------------------------
# Preparation of the housing data

targets_preparation_housing <- rlang::list2(
    tar_eval(
        list(
            tar_file(
                housing_data_org_file_names,
                file.path(
                    config_paths()[["org_data_path"]],
                    "On-site",
                    config_globals()[["red_version"]],
                    "parquet",
                    paste0(housing_types, "_allVersions_ohneText.parquet")
                )
            ),
            tar_file_read(
                housing_data_org_names,
                housing_data_org_file_names,
                reading_housing_data_org(!!.x)
            ),
            tar_fst(
                housing_cleaned,
                cleaning_housing_data(
                    housing_data_org = housing_data_org_names,
                    housing_type = housing_types,
                    grids_municipalities = grids_municipalities,
                    grids_lmr = grids_lmr
                )
            )
        ),
        values = list(
            housing_types = helpers_target_names()[["static_housing_types"]],
            housing_data_org_file_names = rlang::syms(helpers_target_names()[["static_housing_org_file_names"]]),
            housing_data_org_names = rlang::syms(helpers_target_names()[["static_housing_data_org_names"]]),
            housing_cleaned = rlang::syms(helpers_target_names()[["static_housing_data_cleaned"]])
        )
    )
)

#--------------------------------------------------
# Estimation of time effects
# Currently done for: years and quarters
# NOTE: This reflects regression 1 in the Stata routine.

targets_estimation_time <- rlang::list2(
    tar_eval(
        list(
            tar_target(
                estimated_time_effects,
                estimating_time_effects(
                    housing_data = housing_cleaned,
                    housing_type = housing_types,
                    reference_periods = c("2008", "2008-01"),
                    export = TRUE
                )
            ),
            tar_target(
                estimated_time_effects_destatis,
                estimating_time_effects(
                    housing_data = housing_cleaned,
                    housing_type = housing_types,
                    reference_periods = c("2015", "2015-01"),
                    export = FALSE
                )
            ),
            tar_target(
                exported_time_effects,
                exporting_time_effects(
                    time_effects = estimated_time_effects,
                    housing_type_label = housing_type_labels
                )
            )
        ),
        values = list(
            housing_types = helpers_target_names()[["static_housing_types"]],
            housing_type_labels = helpers_target_names()[["static_housing_types_labels"]],
            housing_cleaned = rlang::syms(helpers_target_names()[["static_housing_data_cleaned"]]),
            estimated_time_effects = rlang::syms(helpers_target_names()[["static_estimated_time_effects"]]),
            estimated_time_effects_destatis = rlang::syms(helpers_target_names()[["static_estimated_time_effects_destatis"]]),
            exported_time_effects = rlang::syms(helpers_target_names()[["static_exported_time_effects"]])
        )
    ),
    tar_target(
        combined_time_effects,
        combining_time_effects(
            WM_estimated_time_effects = WM_estimated_time_effects,
            HK_estimated_time_effects = HK_estimated_time_effects,
            WK_estimated_time_effects = WK_estimated_time_effects
        )
    ),
    tar_target(
        combined_individual_time_plot,
        plotting_combined_individual_effects(
            WM_estimated_time_effects = WM_estimated_time_effects,
            HK_estimated_time_effects = HK_estimated_time_effects,
            WK_estimated_time_effects = WK_estimated_time_effects,
            combined_time_effects = combined_time_effects
        )
    ),
    tar_target(
        exported_combined_time_effects,
        exporting_time_effects(
            time_effects = combined_time_effects,
            housing_type_label = "CombInd"
        )
    ),
    tar_target(
        exported_time_effects_grids,
        exporting_time_effects_grids(
            HK_estimated_time_effects = HK_estimated_time_effects,
            WK_estimated_time_effects = WK_estimated_time_effects,
            WM_estimated_time_effects = WM_estimated_time_effects,
            combined_time_effects = combined_time_effects
        )
    )
)

#--------------------------------------------------
# Estimation of regional effects
# NOTE: This reflects regression 2 in the Stata routine.

targets_estimation_region <- rlang::list2(
    tar_eval(
        list(
            tar_target(
                estimated_region_effects,
                estimating_regional_effects(
                    housing_data = housing_cleaned,
                    housing_type = housing_types,
                    grids_municipalities = grids_municipalities,
                    grids_lmr = grids_lmr
                )
            ),
            tar_target(
                aggregated_region_effects,
                aggregating_regional_effects(
                    estimated_region_effects = estimated_region_effects,
                    housing_type = housing_types
                )
            ),
            # TODO: in function
            tar_target(
                exported_aggregated_region_effects,
                exporting_aggregated_regional_effects(
                    aggregated_region_effects = aggregated_region_effects,
                    housing_type = housing_types,
                    housing_type_label = housing_type_labels
                )
            )
        ),
        values = list(
            housing_types = helpers_target_names()[["static_housing_types"]],
            housing_type_labels = helpers_target_names()[["static_housing_types_labels"]],
            housing_cleaned = rlang::syms(helpers_target_names()[["static_housing_data_cleaned"]]),
            estimated_region_effects = rlang::syms(helpers_target_names()[["static_estimated_region_effects"]]),
            aggregated_region_effects = rlang::syms(helpers_target_names()[["static_aggregated_region_effects"]]),
            exported_aggregated_region_effects = rlang::syms(helpers_target_names()[["static_exported_aggregated_region_effects"]])
        )
    ),
    tar_target(
        combined_region_effects,
        combining_regional_effects(
            HK_estimated_region_effects = HK_estimated_region_effects,
            WK_estimated_region_effects = WK_estimated_region_effects,
            WM_estimated_region_effects = WM_estimated_region_effects
        )
    ),
    # TODO: in function
    tar_target(
        exported_region_effects_grids,
        exporting_region_effects_grids(
            HK_estimated_region_effects = HK_estimated_region_effects,
            WK_estimated_region_effects = WK_estimated_region_effects,
            WM_estimated_region_effects = WM_estimated_region_effects,
            combined_region_effects = combined_region_effects
        )
    ),
    tar_target(
        aggregated_combined_region_effects,
        aggregating_combined_regional_effects(
            combined_region_effects = combined_region_effects,
            grids_municipalities = grids_municipalities,
            grids_lmr = grids_lmr
        )
    ),
    # TODO: in function
    tar_target(
        exported_aggregated_combined_region_effects,
        exporting_aggregated_regional_effects(
            aggregated_region_effects = aggregated_combined_region_effects,
            housing_type = "CI",
            housing_type_label = "CombInd"
        )
    )
)

#--------------------------------------------------
# Estimation of regional effects and calculating their change
# NOTE: This reflects regression 3 in the Stata routine.

targets_estimation_change_region <- rlang::list2(
    tar_eval(
        list(
            tar_target(
                estimated_region_effects_change,
                estimating_regional_effects_change(
                    housing_data = housing_cleaned,
                    housing_type = housing_types,
                    grids_municipalities = grids_municipalities,
                    grids_lmr = grids_lmr
                )
            ),
            tar_target(
                aggregated_region_effects_change,
                aggregating_regional_effects_change(
                    estimated_effects_list = estimated_region_effects_change,
                    housing_type = housing_types
                )
            ),
            # TODO: in function
            tar_target(
                exported_aggregated_region_effects_change,
                exporting_aggregated_regional_effects_change(
                    aggregated_region_effects_change = aggregated_region_effects_change,
                    housing_type = housing_types,
                    housing_type_label = housing_type_labels
                )
            )
        ),
        values = list(
            housing_types = helpers_target_names()[["static_housing_types"]],
            housing_type_labels = helpers_target_names()[["static_housing_types_labels"]],
            housing_cleaned = rlang::syms(helpers_target_names()[["static_housing_data_cleaned"]]),
            estimated_region_effects_change = rlang::syms(helpers_target_names()[["static_estimated_region_effects_change"]]),
            aggregated_region_effects_change = rlang::syms(helpers_target_names()[["static_aggregated_region_effects_change"]]),
            exported_aggregated_region_effects_change = rlang::syms(helpers_target_names()[["static_exported_aggregated_region_effects_change"]])
        )
    ),
    tar_target(
        combined_region_effects_change,
        combining_regional_effects_change(
            HK_estimated_region_effects_change = HK_estimated_region_effects_change,
            WK_estimated_region_effects_change = WK_estimated_region_effects_change,
            WM_estimated_region_effects_change = WM_estimated_region_effects_change
        )
    ),
    # TODO: in function
    tar_target(
        exported_region_effects_change_grids,
        exporting_region_effects_change_grids(
            HK_estimated_region_effects_change = HK_estimated_region_effects_change,
            WK_estimated_region_effects_change = WK_estimated_region_effects_change,
            WM_estimated_region_effects_change = WM_estimated_region_effects_change,
            combined_region_effects_change = combined_region_effects_change
        )
    ),
    tar_target(
        aggregated_combined_region_effects_change,
        aggregating_combined_regional_effects_change(
            combined_region_effects_change = combined_region_effects_change,
            grids_municipalities = grids_municipalities,
            grids_lmr = grids_lmr
        )
    ),
    # TODO: in function
    tar_target(
        exported_aggregated_combined_region_effects_change,
        exporting_aggregated_regional_effects_change(
            aggregated_region_effects_change = aggregated_combined_region_effects_change,
            housing_type = "CI",
            housing_type_label = "CombInd"
        )
    )
)

#--------------------------------------------------
# Testing output

targets_test <- rlang::list2(
    #--------------------------------------------------
    # reading previous version output
    tar_eval(
        list(
            # regional effects
            tar_file_read(
                estimated_region_effects_prev_version,
                file.path(
                    stringr::str_replace(
                        config_paths()[["output_path"]],
                        paste0("version_", config_globals()[["next_version"]]),
                        paste0("version_", config_globals()[["current_version"]])    
                    ),
                    housing_type,
                    "estimates",
                    "regional_effects_grids_year.xlsx"
                ),
                reading_estimated_region_effects_prev_version(!!.x),
                format = "fst"
            ),
            # change in regional effects
            tar_file_read(
                estimated_region_effects_change_prev_version,
                file.path(
                    stringr::str_replace(
                        config_paths()[["output_path"]],
                        paste0("version_", config_globals()[["next_version"]]),
                        paste0("version_", config_globals()[["current_version"]])    
                    ),
                    housing_type,
                    "estimates",
                    "regional_effects_grids_year_change.xlsx"
                ),
                reading_estimated_region_effects_prev_version(!!.x),
                format = "fst"
            )
        ),
        values = list(
            housing_type = helpers_target_names()[["static_housing_types"]],
            estimated_region_effects_prev_version = rlang::syms(helpers_target_names()[["static_estimated_region_effects_prev_version"]]),
            estimated_region_effects_change_prev_version = rlang::syms(helpers_target_names()[["static_estimated_region_effects_change_prev_version"]])
        )
    ),
    # regional effects
    tar_file_read(
        combined_estimated_region_effects_prev_version,
        file.path(
            stringr::str_replace(
                config_paths()[["output_path"]],
                paste0("version_", config_globals()[["next_version"]]),
                paste0("version_", config_globals()[["current_version"]])    
            ),
            "CI",
            "estimates",
            "combined_regional_effects_grids_year.xlsx"
        ),
        reading_estimated_region_effects_prev_version(!!.x),
        format = "fst"
    ),
    tar_file_read(
        combined_estimated_region_effects_change_prev_version,
        file.path(
            stringr::str_replace(
                config_paths()[["output_path"]],
                paste0("version_", config_globals()[["next_version"]]),
                paste0("version_", config_globals()[["current_version"]])    
            ),
            "CI",
            "estimates",
            "combined_regional_effects_grids_year_change.xlsx"
        ),
        reading_estimated_region_effects_prev_version(!!.x),
        format = "fst"
    ),
    #--------------------------------------------------
    # reading destatis indices (just for time effects)
    # comparing RWI-GEO-REDX with Destatis
    tar_file_read(
        destatis_time_effects,
        file.path(
            config_paths()[["data_path"]],
            "external_data",
            "destatis_price_index"
        ),
        reading_destatis(!!.x)
    ),
    tar_target(
        time_effects_comparison_plots,
        plotting_time_differences_destatis(
            HK_effects = HK_estimated_time_effects_destatis,
            WK_effects = WK_estimated_time_effects_destatis,
            WM_effects = WM_estimated_time_effects_destatis,
            destatis_effects = destatis_time_effects
        )
    ),
    #--------------------------------------------------
    # summarise regional effects (current/upcoming and previous version)
    # regional effects upcoming version
    tar_target(
        summary_stats_region_test,
        testing_summaries_region_effects(
            HK_region_effects = HK_estimated_region_effects[["year"]],
            WK_region_effects = WK_estimated_region_effects[["year"]],
            WM_region_effects = WM_estimated_region_effects[["year"]],
            CI_region_effects = combined_region_effects[["year"]],
            output_text_file = "overall_summary_stats",
            change_effects = FALSE
        )
    ),
    # regional effects previous version
    tar_target(
        summary_stats_region_test_prev_version,
        testing_summaries_region_effects(
            HK_region_effects = HK_estimated_region_effects_prev_version,
            WK_region_effects = WK_estimated_region_effects_prev_version,
            WM_region_effects = WM_estimated_region_effects_prev_version,
            CI_region_effects = combined_estimated_region_effects_prev_version,
            output_text_file = "overall_summary_stats_prev_version",
            change_effects = FALSE
        )
    ),
    # regional effects changes upcoming version
    # NOTE: currently works only for yearly changes. code needs to be adjusted
    # if quarterly changes should be considered as well
    tar_target(
        summary_stats_region_change_test,
        testing_summaries_region_effects(
            HK_region_effects = HK_estimated_region_effects_change[["year"]],
            WK_region_effects = WK_estimated_region_effects_change[["year"]],
            WM_region_effects = WM_estimated_region_effects_change[["year"]],
            CI_region_effects = combined_region_effects_change[["year"]],
            output_text_file = "overall_summary_stats_change",
            change_effects = TRUE
        )
    ),
    # regional effects changes previous version
    tar_target(
        summary_stats_region_change_test_prev_version,
        testing_summaries_region_effects(
            HK_region_effects = HK_estimated_region_effects_change_prev_version,
            WK_region_effects = WK_estimated_region_effects_change_prev_version,
            WM_region_effects = WM_estimated_region_effects_change_prev_version,
            CI_region_effects = combined_estimated_region_effects_change_prev_version,
            output_text_file = "overall_summary_stats_change_prev_version",
            change_effects = TRUE
        )
    ),
    # comparison regional effects upcoming and previous version
    tar_target(
        summary_stats_region_test_comparison,
        testing_differences_region_effects_versions(
            summary_stats = summary_stats_region_test,
            summary_stats_prev_version = summary_stats_region_test_prev_version,
            output_text_file = "overall_summary_stats_comparison_versions"
        )
    ),
    # comparison regional effects changes upcoming and previos version
    tar_target(
        summary_stats_region_change_test_comparison,
        testing_differences_region_effects_versions(
            summary_stats = summary_stats_region_change_test,
            summary_stats_prev_version = summary_stats_region_change_test_prev_version,
            output_text_file = "overall_summary_stats_change_comparison_versions"
        )
        
    )
)

#--------------------------------------------------
# clean up

targets_cleanup <- rlang::list2(
    tar_target(
        reorder_worksheets,
        reordering_worksheets(),
    )
)

#--------------------------------------------------
# pipeline stats

targets_pipeline_stats <- rlang::list2(
    # NOTE: targets type has to be tar_file in order to use tar_progress_summary
    # and tar_crew within the pipeline
    tar_file(
        pipeline_stats,
        helpers_monitoring_pipeline(),
        cue = tar_cue(mode = "always")
    )
)

#----------------------------------------------
# combine all

rlang::list2(
    targets_preparation_folders,
    targets_preparation_geo,
    targets_preparation_housing,
    targets_estimation_time,
    targets_estimation_region,
    targets_estimation_change_region,
    targets_test,
    targets_pipeline_stats,
    targets_cleanup
)