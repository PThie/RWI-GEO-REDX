helpers_anonymizing_region_effects <- function(
    region_effects_data = NA,
    SUF = TRUE
) {
    #' @title Anonymize region effects
    #' 
    #' @description This function anonymizes region effects by setting all values
    #' below a certain threshold of NOBS to NA (applies to price indices but also
    #' to the NOBS itself).
    #' 
    #' @param region_effects_data Data frame with region effects
    #' @param SUF Logical. If TRUE, SUF anonymization thresholds are used.
    #' If FALSE, PUF thresholds are used.
    #' 
    #' @return Data frame with anonymized region effects
    #' @author Patrick Thiel

    #--------------------------------------------------
    # SUF and PUF files have different anonymization thresholds
    # PUF are stricter than SUF

    if (SUF == TRUE) {
        anonymization_threshold <- config_globals()[["SUF_anonymization_threshold"]]
    } else {
        anonymization_threshold <- config_globals()[["PUF_anonymization_threshold"]]
    }

    #--------------------------------------------------
    # create anonymized data

    anonymized_data <- region_effects_data |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::starts_with("pindex"),
                # DESCRIPTION: gsub() replaces "pindex" with "NOBS" in the
                # the current column and get() retrieves the column value then 
                ~ dplyr::case_when(
                    get(
                        gsub("pindex", "NOBS", dplyr::cur_column())
                    ) < anonymization_threshold ~ NA_real_,
                    TRUE ~ .x
                )
            ),
            dplyr::across(
                .cols = dplyr::starts_with("NOBS"),
                ~ dplyr::case_when(
                    .x < anonymization_threshold ~ NA_integer_,
                    TRUE ~ .x
                )
            )
        )

    #--------------------------------------------------
    # return

    return(anonymized_data)        
}