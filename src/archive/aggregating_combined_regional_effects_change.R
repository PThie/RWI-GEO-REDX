# aggregating_combined_regional_effects_change <- function(
#     combined_region_effects_change = NA,
#     grids_municipalities = NA,
#     grids_lmr = NA,
#     housing_type = NA,
#     destatis = NA
# ) {
#     #' @title Aggregating combined regional effects (change values)
#     #' 
#     #' @description This function aggregates combined regional effects to a larger
#     #' regional level (municipality and districts). It also calculates the change
#     #' to the reference period.
#     #' 
#     #' @param combined_region_effects_change List with combined regional effects (with
#     #' change values)
#     #' @param grids_municipalities Data frame with connection between grids and
#     #' municipalities
#     #' @param grids_lmr Dataframe with connection between grids and labor market
#     #' regions (LMR/AMR)
#     #' @param destatis Logical indicating whether to use Destatis references.
#     #' @param housing_type Character string specifying the housing type.
#     #' 
#     #' @return List with aggregated combined regional effects
#     #' @author Patrick Thiel
    
#     #--------------------------------------------------
#     # loop through results and aggregation levels

#     results_list <- list()
#     for (result in names(combined_region_effects_change)) {
#         for (agg_level in c("munic", "district", "lmr")) {
#             #--------------------------------------------------
#             # set up for specific regional level
#             # and time definitions
            
#             nobs_var <- helpers_regional_effects_settings(agg_level = agg_level)[["nobs_var"]]
#             region_id <- helpers_regional_effects_settings(agg_level = agg_level)[["region_id"]]
#             region_label <- helpers_regional_effects_settings(agg_level = agg_level)[["region_label"]]

#             time_label <- helpers_regional_effects_change_settings(
#                 time_period = result,
#                 destatis = FALSE,
#                 housing_type = housing_type
#             )[["time_label"]]
#             reference_period <- helpers_regional_effects_change_settings(
#                 time_period = result,
#                 destatis = FALSE,
#                 housing_type = housing_type
#             )[["reference_period"]]

#             #--------------------------------------------------
#             # prepare combined regional effects

#             combined_effects_prep <- combined_region_effects_change[[result]] |>
#                 dplyr::select(-c("HK_nobs", "WK_nobs", "WM_nobs")) |>
#                 dplyr::rename(
#                     nobs_grid = total_nobs,
#                     pindex = weighted_pindex
#                 ) |>
#                 merge(
#                     grids_municipalities,
#                     by.x = "grid",
#                     by.y = "ergg_1km",
#                     all.x = TRUE
#                 ) |>
#                 dplyr::mutate(
#                     kid2019 = substring(gid2019, 1, 5)
#                 )

#             # merge LMR IDs
#             combined_effects_prep <- combined_effects_prep |>
#                 merge(
#                     grids_lmr,
#                     by.x = "grid",
#                     by.y = "ergg_1km",
#                     all.x = TRUE
#                 ) |>
#                 dplyr::rename(lmrid = amr)
            
#             #--------------------------------------------------
#             # add number of observations at higher level of aggregation

#             combined_effects_prep <- combined_effects_prep |>
#                 dplyr::group_by(
#                     !!rlang::sym(time_label),
#                     !!rlang::sym(region_id)
#                 ) |>
#                 dplyr::mutate(
#                     !!nobs_var := sum(nobs_grid, na.rm = TRUE)
#                 ) |>
#                 dplyr::ungroup()

#             #--------------------------------------------------
#             # calculate weight
#             # NOTE: weight follows the definition of:
#             # NOBS_ejahr_time_grid / SUM(NOBS_ejahr_time_region)

#             combined_effects_prep <- combined_effects_prep |>
#                 dplyr::mutate(
#                     weight = nobs_grid / .data[[nobs_var]],
#                     weighted_pindex = pindex * weight
#                 )

#             #--------------------------------------------------
#             # aggregate to higher level

#             combined_effects_agg <- combined_effects_prep |>
#                 dplyr::group_by(
#                     !!rlang::sym(time_label),
#                     !!rlang::sym(region_id)
#                 ) |>
#                 dplyr::summarise(
#                     weighted_pindex = sum(weighted_pindex, na.rm = TRUE),
#                     !!nobs_var := dplyr::first(.data[[nobs_var]])
#                 ) |>
#                 dplyr::ungroup()
            
#             #--------------------------------------------------
#             # calculate the change between District_Year - District_2008

#             combined_effects_agg <- combined_effects_agg |>
#                 merge(
#                     combined_effects_agg |>
#                         dplyr::filter(!!rlang::sym(time_label) == reference_period) |>
#                         dplyr::rename(weighted_pindex_ref = weighted_pindex) |>
#                         dplyr::select(-c(time_label, nobs_var)),
#                     by = region_id,
#                     all.x = TRUE
#                 ) |>
#                 dplyr::mutate(
#                     weighted_pindex_change = dplyr::case_when(
#                         !is.na(weighted_pindex) ~ (
#                             (weighted_pindex - weighted_pindex_ref) / weighted_pindex_ref
#                         ) * 100,
#                         TRUE ~ NA_real_
#                     )
#                 ) |>
#                 dplyr::select(-c(weighted_pindex_ref))

#             #--------------------------------------------------
#             # export
            
#             openxlsx::write.xlsx(
#                 combined_effects_agg,
#                 file.path(
#                     config_paths()[["output_path"]],
#                     "CI",
#                     "estimates",
#                     paste0(
#                         "combined_regional_effects_",
#                         region_label,
#                         "_",
#                         time_label,
#                         "_change.xlsx"
#                     )
#                 )
#             )

#             #--------------------------------------------------
#             # store results

#             results_list[[paste0(region_label, "_", time_label)]] <- combined_effects_agg
#         }
#     }
    
#     #--------------------------------------------------
#     # return

#     return(results_list)
# }