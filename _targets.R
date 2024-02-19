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
})

#----------------------------------------------
# set working directory

setwd(here::here())

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

lapply(
    list.files(
        config_paths()[["code_path"]],
        pattern = "\\.R$",
        full.names = TRUE,
        ignore.case = TRUE
    ),
    source
)

#----------------------------------------------
# data frame for loop through the housing data

housing_data_info <- data.frame(
    cbind(
        #housing_type = c("HK", "WK", "WM")
        housing_type = c("WK") # for testing DELETE later
    )
) |>
    dplyr::mutate(
        housing_type_file_name = paste0(
            housing_type,
            "_allVersions_ohneText"
        )
    )

#----------------------------------------------
# processing steps

# Preparation of the geo information
targets_preparation_geo <- rlang::list2(
    tar_qs(
        grid_cleaned,
        clean_grids()
    ),
    tar_qs(
        municipalities_cleaned,
        clean_municipalities()
    ),
    tar_qs(
        labor_market_regions_cleaned,
        clean_labor_market_regions()
    )
)

# Preparation of the housing data
targets_preparation_housing <- tar_map(
    tar_fst(
        housing_data,
        prepare_housing(
            housing_file = housing_type_file_name,
            data_type = substring(
                housing_type_file_name, 1, 2
            )
        )
    ),
    values = housing_data_info,
    names = housing_type
)

#----------------------------------------------
# combine all

rlang::list2(
    #targets_preparation_geo,
    targets_preparation_housing
)