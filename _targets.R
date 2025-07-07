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
    library(stats)
    library(gdata)
    library(tidyr)
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
            "Raster_Gemeinde",
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
# Estimation of time-region effects
# NOTE: This reflects regression 3 in the Stata routine but without taking the
# change in logs.

targets_estimation_region_abs <- rlang::list2(
    tar_eval(
        list(
            tar_target(
                estimated_region_effects_abs,
                estimating_regional_effects_abs(
                    housing_data = housing_cleaned,
                    housing_type = housing_types,
                    grids_municipalities = grids_municipalities,
                    grids_lmr = grids_lmr
                )
            ),
            tar_target(
                aggregated_region_effects_abs,
                aggregating_regional_effects_abs(
                    estimated_effects_list = estimated_region_effects_abs,
                    housing_type = housing_types
                )
            ),
            tar_target(
                exported_aggregated_region_effects_abs,
                # TODO: Change function name since ABS is not true
                exporting_aggregated_regional_effects_abs(
                    aggregated_region_effects_list = aggregated_region_effects_abs,
                    housing_type = housing_types,
                    housing_type_label = housing_type_labels,
                    pindex_col_name = "weighted_pindex",
                    sheet_name_addendum = "abs",
                    export_name_addendum = "abs",
                    dependencies = empty_export_workbooks
                )
            )
        ),
        values = list(
            housing_types = helpers_target_names()[["static_housing_types"]],
            housing_type_labels = helpers_target_names()[["static_housing_types_labels"]],
            housing_cleaned = rlang::syms(helpers_target_names()[["static_housing_data_cleaned"]]),
            estimated_region_effects_abs = rlang::syms(helpers_target_names()[["static_estimated_region_effects_abs"]]),
            aggregated_region_effects_abs = rlang::syms(helpers_target_names()[["static_aggregated_region_effects_abs"]]),
            exported_aggregated_region_effects_abs = rlang::syms(helpers_target_names()[["static_exported_aggregated_region_effects_abs"]])
        )
    ),
    tar_target(
        exported_region_effects_abs_grids,
        exporting_region_effects_grids(
            HK_estimated_region_effects = HK_estimated_region_effects_abs,
            WK_estimated_region_effects = WK_estimated_region_effects_abs,
            WM_estimated_region_effects = WM_estimated_region_effects_abs,
            pindex_col_name = "pindex",
            export_name_addendum = "abs"
        )
    )
)

# branch: deviation within region (absolute and percent) -> refers to table 2 in notes
targets_deviation_region <- rlang::list2(
    tar_eval(
        list(
            tar_target(
                calculated_deviations_regions_grids,
                calculating_deviations_regions_grids(
                    grid_effects_abs = estimated_region_effects_abs
                )
            ),
            tar_target(
                calculated_deviations_regions,
                calculating_deviations_regions(
                    aggregated_effects = aggregated_region_effects_abs
                )
            ),
            # TODO: Adjust function name (see comment above)
            tar_target(
                exported_aggregated_region_effects_dev,
                exporting_aggregated_regional_effects_abs(
                    aggregated_region_effects_list = calculated_deviations_regions,
                    housing_type = housing_types,
                    housing_type_label = housing_type_labels,
                    pindex_col_name = "pindex_dev",
                    sheet_name_addendum = "dev",
                    export_name_addendum = "dev_region",
                    dependencies = empty_export_workbooks
                )
            ),
            # TODO: Adjust function name (see comment above)
            tar_target(
                exported_aggregated_region_effects_dev_perc,
                exporting_aggregated_regional_effects_abs(
                    aggregated_region_effects_list = calculated_deviations_regions,
                    housing_type = housing_types,
                    housing_type_label = housing_type_labels,
                    pindex_col_name = "pindex_dev_perc",
                    sheet_name_addendum = "devpc",
                    export_name_addendum = "dev_region",
                    dependencies = empty_export_workbooks
                )
            )
        ),
        values = list(
            housing_types = helpers_target_names()[["static_housing_types"]],
            housing_type_labels = helpers_target_names()[["static_housing_types_labels"]],
            calculated_deviations_regions_grids = rlang::syms(helpers_target_names()[["static_calculated_deviations_regions_grids"]]),
            estimated_region_effects_abs = rlang::syms(helpers_target_names()[["static_estimated_region_effects_abs"]]),
            calculated_deviations_regions = rlang::syms(helpers_target_names()[["static_calculated_deviations_regions"]]),
            aggregated_region_effects_abs = rlang::syms(helpers_target_names()[["static_aggregated_region_effects_abs"]]),
            exported_aggregated_region_effects_dev = rlang::syms(helpers_target_names()[["static_exported_aggregated_region_effects_dev"]]),
            exported_aggregated_region_effects_dev_perc = rlang::syms(helpers_target_names()[["static_exported_aggregated_region_effects_dev_perc"]])
        )
    ),
    tar_target(
        exported_deviations_regions_grids_dev,
        exporting_region_effects_grids(
            HK_estimated_region_effects = HK_calculated_deviations_regions_grids,
            WK_estimated_region_effects = WK_calculated_deviations_regions_grids,
            WM_estimated_region_effects = WM_calculated_deviations_regions_grids,
            pindex_col_name = "pindex_dev",
            export_name_addendum = "dev_region"
        )
    ),
    tar_target(
        exported_deviations_regions_grids_dev_perc,
        exporting_region_effects_grids(
            HK_estimated_region_effects = HK_calculated_deviations_regions_grids,
            WK_estimated_region_effects = WK_calculated_deviations_regions_grids,
            WM_estimated_region_effects = WM_calculated_deviations_regions_grids,
            pindex_col_name = "pindex_dev_perc",
            export_name_addendum = "dev_perc_region"
        )
    ),
    # TODO: Calculate means
    tar_target(
        combined_deviations_regions_grids,
        combining_regional_effects_grids(
            HK_estimated_region_effects = HK_calculated_deviations_regions_grids,
            WK_estimated_region_effects = WK_calculated_deviations_regions_grids,
            WM_estimated_region_effects = WM_calculated_deviations_regions_grids
        )
    ),
    # TODO: Calculate means
    tar_target(
        combined_deviations_regions,
        combining_regional_effects(
            HK_estimated_region_effects = HK_calculated_deviations_regions,
            WK_estimated_region_effects = WK_calculated_deviations_regions,
            WM_estimated_region_effects = WM_calculated_deviations_regions
        )
    )

    # TODO: Step: export aggregated combined deviations (percent)
)

# branch: deviation between regions (absolute and percent) -> refers to table 3 in notes

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
# visualization
# mainly for data report

targets_visualization <- rlang::list2(
    tar_target(
        map_regional_effects_district,
        plotting_regional_effects(
            HK_regional_effects = HK_aggregated_region_effects[["district_year"]],
            HK_regional_effects_change = HK_aggregated_region_effects_change[["district_year"]],
            districts_cleaned = districts_cleaned
        )
    )
)

#--------------------------------------------------
# clean up

targets_cleanup <- rlang::list2(
    tar_target(
        copy_information_worksheet,
        copying_information_worksheet(
            housing_types_labels = helpers_target_names()[["static_housing_types_labels"]],
            dependencies = list(
                exported_region_effects_grids,
                exported_region_effects_change_grids
            )
        )
    ),
    tar_target(
        reorder_worksheets,
        reordering_worksheets(
            housing_types_labels = helpers_target_names()[["static_housing_types_labels"]],
            dependency = copy_information_worksheet
        )
    ),
    tar_target(
        calculate_num_rows_cols_doi,
        calculating_num_rows_cols_doi(
            housing_types_labels = helpers_target_names()[["static_housing_types_labels"]],
            dependency = reorder_worksheets
        )
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
    # targets_estimation_time, #TODO: DELETE LATER
    # targets_estimation_region, TODO: DELETE POTENTIALLY LATER
    targets_estimation_region_abs,
    targets_deviation_region,
    # targets_test, TODO: TURN ON LATER (ADJUST)
    # targets_visualization, TODO: TURN ON LATER (ADJUST)
    # targets_cleanup, TODO: TURN ON LATER (ADJUST)
    targets_pipeline_stats
)