#----------------------------------------------
# description

# Thhis fule is the main file that orchestrates the other coding files. It
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
    }
}

#----------------------------------------------
# data frame for loop through the housing data

# define housing types
static_housing_types <- c("WK", "HK", "WM")

# define housing types labels as in output data
static_housing_types_labels <- c("ApPurc", "HouPurc", "ApRent")

# for branching in tar_eval
# defines the names of the target objects
static_housing_org_file_names <- glue::glue(
    "{static_housing_types}_allVersions_ohneText"
)

static_housing_data_org_names <- glue::glue(
    "{static_housing_types}_housing_data_org"
)

static_housing_data_cleaned <- glue::glue(
    "{static_housing_types}_cleaned"
)



static_output_names_current <- glue::glue(
    "{static_housing_types_labels}_output_data_{config_globals()[['current_version']]}"
)

static_output_names_previous <- glue::glue(
    "{static_housing_types_labels}_output_data_{config_globals()[['previous_version']]}"
)

static_output_test_names <- glue::glue(
    "{static_housing_types_labels}_output_test"
)


# housing_data_info <- data.frame(
#     cbind(
#         #housing_type = c("HK", "WK", "WM")
#         housing_type = c("WK") # for testing DELETE later
#     )
# ) |>
#     dplyr::mutate(
#         housing_type_file_name = rlang::syms(paste0(
#             housing_type,
#             "_allVersions_ohneText"
#         )),
#         housing_type_processed = rlang::syms(paste0(
#             "housing_data_",
#             housing_type
#         ))
#     )

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


# Preparation of the housing data
# targets_preparation_housing <- tar_map(list(
#     tar_fst(
#         housing_data,
#         prepare_housing(
#             housing_file = housing_type_file_name,
#             data_type = substring(
#                 housing_type_file_name, 1, 2
#             ),
#             grids_municipalities = grids_municipalities,
#             grids_lmr = grids_lmr
#         )
#     ),
#     tar_target(
#         estimated_time_effects,
#         estimate_time_effects(
#             housing_data = housing_type_processed,
#             data_type = substring(
#                 housing_type_processed,
#                 nchar(housing_type_processed) - 1,
#                 nchar(housing_type_processed)
#             )
#         )
#     )),
#     values = housing_data_info,
#     names = housing_type
# )

# Preparation and estimation
targets_prep_est <- rlang::list2(
    tar_eval(
        list(
            # define paths orginal data
            tar_target(
                housing_data_org_file_names,
                making_housing_data_org_file_names(
                    data_type = housing_types
                )
            ),
            tar_file_read(
                housing_data_org_names,
                housing_data_org_file_names,
                reading_housing_data_org(!!.x)
            )
        ),
        values = list(
            housing_types = static_housing_types,
            housing_org_file_names = rlang::syms(static_housing_org_file_names),
            housing_data_org_names = rlang::syms(static_housing_data_org_names)
        )
    )
)



# Estimation
# targets_estimation <- tar_map(
#     tar_target(
#         estimated_time_effects,
#         estimate_time_effects(
#             housing_data = housing_type_processed,
#             data_type = substring(
#                 housing_type_processed,
#                 nchar(housing_type_processed) - 1,
#                 nchar(housing_type_processed)
#             )
#         )
#     ),
#     values = housing_data_info,
#     names = housing_type
# )

#--------------------------------------------------
# Testing output

targets_test <- rlang::list2(
    tar_eval(
        list(
            tar_fst(
                output_data_current,
                reading_output_file(
                    housing_type_label = housing_type_labels,
                    housing_type = housing_types,
                    delivery = config_globals()[["current_delivery"]],
                    version = config_globals()[["current_version"]]
                )
            ),
            tar_fst(
                output_data_previous,
                reading_output_file(
                    housing_type_label = housing_type_labels,
                    housing_type = housing_types,
                    delivery = config_globals()[["previous_delivery"]],
                    version = config_globals()[["previous_version"]]
                )
            ),
            tar_fst(
                output_data_tests,
                testing_output_data(
                    output_data_current = output_data_current,
                    output_data_previous = output_data_previous
                )
            )
        ),
        values = list(
            housing_type_labels = static_housing_types_labels,
            housing_types = static_housing_types,
            output_data_current = rlang::syms(static_output_names_current),
            output_data_previous = rlang::syms(static_output_names_previous),
            output_data_tests = rlang::syms(static_output_test_names)
        )
    )
)

#----------------------------------------------
# combine all

rlang::list2(
    targets_preparation_geo,
    targets_preparation_housing,
    # targets_prep_est,
    #targets_estimation,
    #targets_test
)