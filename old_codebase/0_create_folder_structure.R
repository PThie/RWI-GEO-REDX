#--------------------------------------------------
# Description

# This script creates the folder structure for the REDX project once
# a new version is started.

#--------------------------------------------------
# paths

main_path <- MAIN_PATH # add if needed
output_path <- file.path(main_path, "output")

#--------------------------------------------------
# globals

types <- c("WK", "WM", "HK")

# TODO: change version folder
version_folder <- "Dez2023"

#--------------------------------------------------
# create folders

for (type in types) {
    # output folder
    dir.create(
        file.path(
            output_path,
            type,
            "output",
            version_folder
        )
    )

    # data folder
    dir.create(
        file.path(
            output_path,
            type,
            "data",
            version_folder
        )
    )

    # log folder
    dir.create(
        file.path(
            output_path,
            type,
            "log",
            version_folder
        )
    )
}




