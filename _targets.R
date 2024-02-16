#----------------------------------------------
# description

# Thhis fule is the main file that orchestrates the other coding files. It
# controls the data pipeline and defines the global settings.

#----------------------------------------------
# load libraries

suppressPackageStartupMessages({
    library(targets)
    library(renv)
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