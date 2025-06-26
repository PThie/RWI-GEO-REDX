helpers_target_names <- function() {
    #' @title Create target names
    #' 
    #' @description This function creates a list of target names used in the
    #' pipeline when dynamic branching is used (i.e. when tar_eval is used).
    #'  
    #' @return List, target names
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # list of target names

    ##### General names
    static_housing_types <- c("WK", "HK", "WM")
    static_housing_types_labels <- c("ApPurc", "HouPurc", "ApRent")

    target_names <- list(
        "static_housing_types" = static_housing_types,
        "static_housing_types_labels" = static_housing_types_labels,
        #--------------------------------------------------
        # Names for preparation of housing data
        "static_housing_org_file_names" = glue::glue(
            "{static_housing_types}_allVersions_ohneText"
        ),
        "static_housing_data_org_names" = glue::glue(
            "{static_housing_types}_housing_data_org"
        ),
        "static_housing_data_cleaned" = glue::glue(
            "{static_housing_types}_cleaned"
        ),
        #--------------------------------------------------
        # TODO: DELETE LATER
        # Names for estimation of time effects
        # NOTE: This reflects regression 1 in the Stata routine.
        # "static_estimated_time_effects" = glue::glue(
        #     "{static_housing_types}_estimated_time_effects"
        # ),
        # "static_estimated_time_effects_destatis" = glue::glue(
        #     "{static_housing_types}_estimated_time_effects_destatis"
        # ),
        # "static_exported_time_effects" = glue::glue(
        #     "{static_housing_types}_exported_time_effects"
        # ),
        #--------------------------------------------------
        # TODO: DELETE LATER
        # Names for estimation of regional effects
        # NOTE: This reflects regression 2 in the Stata routine.
        # "static_estimated_region_effects" = glue::glue(
        #     "{static_housing_types}_estimated_region_effects"
        # ),
        # "static_aggregated_region_effects" = glue::glue(
        #     "{static_housing_types}_aggregated_region_effects"
        # ),
        # "static_exported_aggregated_region_effects" = glue::glue(
        #     "{static_housing_types}_exported_aggregated_region_effects"
        # ),
        #--------------------------------------------------
        # Names for estimation of regional effects (change)
        # NOTE: This reflects regression 3 in the Stata routine.
        "static_estimated_region_effects_abs" = glue::glue(
            "{static_housing_types}_estimated_region_effects_abs"
        ),
        "static_aggregated_region_effects_abs" = glue::glue(
            "{static_housing_types}_aggregated_region_effects_abs"
        ),
        "static_exported_aggregated_region_effects_abs" = glue::glue(
            "{static_housing_types}_exported_aggregated_region_effects_abs"
        ),
        #--------------------------------------------------
        # Names for testing
        "static_estimated_region_effects_prev_version" = glue::glue(
            "{static_housing_types}_estimated_region_effects_prev_version"
        ),
        "static_estimated_region_effects_change_prev_version" = glue::glue(
            "{static_housing_types}_estimated_region_effects_change_prev_version"
        )
        #--------------------------------------------------
        # names for testing
        # "sheet_names" = c(
        #     "1__District_TimeEff_yearly",
        #     "1__District_TimeEff_quarterly",
        #     "2__District_Pindex_yearly",
        #     "3__District_Change_yearly"
        # ),
        # "static_old_outputs" = glue::glue(
        #     "{static_housing_types}_old_output_data"
        # ),
        # "static_time_effects_test_plot" = glue::glue(
        #     "{static_housing_types}_time_effects_test_plot"
        # ),
        # "static_regional_effects_pattern_test_plot" = glue::glue(
        #     "{static_housing_types}_regional_effects_pattern_test_plot"
        # ),
        # "static_regional_effects_time_test_plot" = glue::glue(
        #     "{static_housing_types}_regional_effects_time_test_plot"
        # )
    )

    #--------------------------------------------------
    # return

    return(target_names)
}