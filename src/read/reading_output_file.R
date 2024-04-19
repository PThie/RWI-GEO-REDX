reading_output_file <- function(
    housing_type_label = NA,
    housing_type = NA,
    delivery = NA,
    version = NA
) {
    
    #--------------------------------------------------
    # define path to output data

    output_file_path <- file.path(
        config_paths()[["output_path"]],
        housing_type,
        "output",
        delivery,
        paste0(
            "RWIGEOREDX_",
            housing_type_label,
            "_",
            version,
            "_SUF.xlsx"
        )
    )

    #--------------------------------------------------
    # read data

    dta <- openxlsx::read.xlsx(
        output_file_path,
        sheet = "1__District_TimeEff_yearly"
    ) |>
        # set all columns to numeric
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::everything(),
                ~ as.numeric(.x)
            )
        )

    #--------------------------------------------------
    # return

    return(dta)
}