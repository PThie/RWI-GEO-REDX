#----------------------------------------------
# description

# This file is the main file that orchestrates the other coding files. It
# controls the data pipeline and defines the global settings.

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
    storage = "worker",
    retrieval = "worker"
)

# tar_make_future() configuration:
plan(callr)

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

#----------------------------------------------
# data frame for loop through the housing data

##### General names
static_housing_types <- c("WK", "HK", "WM")
static_housing_types_labels <- c("ApPurc", "HouPurc", "ApRent")

##### Names for preparation of housing data
static_housing_org_file_names <- glue::glue(
    "{static_housing_types}_allVersions_ohneText"
)

static_housing_data_org_names <- glue::glue(
    "{static_housing_types}_housing_data_org"
)

static_housing_data_cleaned <- glue::glue(
    "{static_housing_types}_cleaned"
)

##### Names for estimation of time effects
# NOTE: This reflects regression 1 in the Stata routine.
static_estimated_time_effects <- glue::glue(
    "{static_housing_types}_estimated_time_effects"
)

static_exported_time_effects <- glue::glue(
    "{static_housing_types}_exported_time_effects"
)

##### Names for estimation of regional effects
# NOTE: This reflects regression 2 in the Stata routine.

static_estimated_region_effects <- glue::glue(
    "{static_housing_types}_estimated_region_effects"
)

static_aggregated_region_effects <- glue::glue(
    "{static_housing_types}_aggregated_region_effects"
)

##### Names for estimation of regional effects (change)
# NOTE: This reflects regression 3 in the Stata routine.
static_estimated_region_effects_change <- glue::glue(
    "{static_housing_types}_estimated_region_effects_change"
)

static_aggregated_region_effects_change <- glue::glue(
    "{static_housing_types}_aggregated_region_effects_change"
)

##### names for testing
sheet_names <- c(
    "1__District_TimeEff_yearly",
    "1__District_TimeEff_quarterly",
    "2__District_Pindex_yearly",
    "3__District_Change_yearly"
)

static_old_outputs <- glue::glue(
    "{static_housing_types}_old_output_data"
)

static_time_effects_test_plot <- glue::glue(
    "{static_housing_types}_time_effects_test_plot"
)

static_regional_effects_pattern_test_plot <- glue::glue(
    "{static_housing_types}_regional_effects_pattern_test_plot"
)

static_regional_effects_time_test_plot <- glue::glue(
    "{static_housing_types}_regional_effects_time_test_plot"
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
            housing_types = static_housing_types,
            housing_data_org_file_names = rlang::syms(static_housing_org_file_names),
            housing_data_org_names = rlang::syms(static_housing_data_org_names),
            housing_cleaned = rlang::syms(static_housing_data_cleaned)
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
                    housing_type = housing_types 
                )
            ),
            tar_target(
                exported_time_effects,
                exporting_time_effects(
                    time_effects = estimated_time_effects,
                    housing_type = housing_types,
                    housing_type_label = housing_type_labels
                )
            )
        ),
        values = list(
            housing_types = static_housing_types,
            housing_type_labels = static_housing_types_labels,
            housing_cleaned = rlang::syms(static_housing_data_cleaned),
            estimated_time_effects = rlang::syms(static_estimated_time_effects),
            exported_time_effects = rlang::syms(static_exported_time_effects)
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
    # TODO: For documentation add to which output file this leads
    tar_target(
        exported_combined_time_effects,
        exporting_time_effects(
            time_effects = combined_time_effects,
            housing_type = "Combined",
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
                    grids_municipalities = grids_municipalities
                )
            ),
            tar_target(
                aggregated_region_effects,
                aggregating_regional_effects(
                    estimated_region_effects = estimated_region_effects,
                    housing_type = housing_types
                )
            )
        ),
        values = list(
            housing_types = static_housing_types,
            housing_cleaned = rlang::syms(static_housing_data_cleaned),
            estimated_region_effects = rlang::syms(static_estimated_region_effects),
            aggregated_region_effects = rlang::syms(static_aggregated_region_effects)
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
            grids_municipalities = grids_municipalities
        )
    ),
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
                    grids_municipalities = grids_municipalities
                )
            ),
            tar_target(
                aggregated_region_effects_change,
                aggregating_regional_effects_change(
                    estimated_effects_list = estimated_region_effects_change,
                    housing_type = housing_types
                )
            )
        ),
        values = list(
            housing_types = static_housing_types,
            housing_cleaned = rlang::syms(static_housing_data_cleaned),
            estimated_region_effects_change = rlang::syms(static_estimated_region_effects_change),
            aggregated_region_effects_change = rlang::syms(static_aggregated_region_effects_change)
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
            grids_municipalities = grids_municipalities
        )
    ),
)

#--------------------------------------------------
# Testing output
# Needs rework since regression 3 changed

targets_test <- rlang::list2(
    tar_eval(
        list(
            # Reading old data output
            tar_target(
                old_output_data,
                reading_old_output_file(
                    housing_type_label = housing_type_labels,
                    sheet_names = sheet_names,
                    time_effects = estimated_time_effects,
                    region_effects = aggregated_region_effects,
                    region_effects_change = aggregated_region_effects_change
                )
            ),
            # Plotting time effects
            tar_target(
                time_effects_test_plot,
                testing_time_effects(
                    old_output_data = old_output_data,
                    housing_type = housing_types
                )
            ),
            # Plotting regional effects (focus on regional pattern)
            tar_target(
                regional_effects_pattern_test_plot,
                testing_regional_effects_pattern(
                    old_output_data = old_output_data,
                    housing_type = housing_types,
                    districts_cleaned = districts_cleaned
                )
            ),
            # Plotting regional effects (focus on time) 
            tar_target(
                regional_effects_time_test_plot,
                testing_regional_effects_time(
                    old_output_data = old_output_data,
                    housing_type = housing_types
                )
            )
        ),
        values = list(
            housing_types = static_housing_types,
            old_output_data = rlang::syms(static_old_outputs),
            housing_type_labels = static_housing_types_labels,
            estimated_time_effects = rlang::syms(static_estimated_time_effects),
            aggregated_region_effects = rlang::syms(static_aggregated_region_effects),
            aggregated_region_effects_change = rlang::syms(static_aggregated_region_effects_change),
            time_effects_test_plot = rlang::syms(static_time_effects_test_plot),
            regional_effects_pattern_test_plot = rlang::syms(static_regional_effects_pattern_test_plot),
            regional_effects_time_test_plot = rlang::syms(static_regional_effects_time_test_plot)
        )
    )
)

#----------------------------------------------
# combine all

rlang::list2(
    targets_preparation_geo,
    targets_preparation_housing,
    targets_estimation_time,
    targets_estimation_region,
    targets_estimation_change_region,
    targets_test
)