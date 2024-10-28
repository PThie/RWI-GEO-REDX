creating_folder_structure <- function() {
    #' @title Create folder structure
    #' 
    #' @description This function creates the folder structure for the output
    #' of a new wave.
    #' 
    #' @return NULL
    #' @author Patrick Thiel

    #--------------------------------------------------
    # create type folders with subdirectories

    for (TYPE in config_globals()[["housing_types"]]) {
        for (FOLDER in config_globals()[["folders"]]) {
            directory <- file.path(
                config_paths()[["output_path"]],
                TYPE,
                FOLDER
            )

            if (!dir.exists(directory)) {
                dir.create(directory, recursive = TRUE)
            }
        }
    }

    #--------------------------------------------------
    # create special folders

    special_folders <- c("export", "testing")

    for (special_folder in special_folders) {
        directory <- file.path(
            config_paths()[["output_path"]],
            special_folder
        )

        if (!dir.exists(directory)) {
            dir.create(directory, recursive = TRUE)
        }
    }

    #--------------------------------------------------
    # return

    return(NULL)
}