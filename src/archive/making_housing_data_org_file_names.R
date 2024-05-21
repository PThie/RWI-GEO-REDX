# making_housing_data_org_file_names <- function(data_type = NA) {

#     #--------------------------------------------------
#     # define path to original data

#     org_data_file_path <- file.path(
#         config_paths()[["org_data_path"]],
#         "On-site",
#         config_globals()[["red_version"]],
#         # NOTE: uncomment later for loop
#         paste0(data_type, "_allVersions_ohneText.dta")
#         #"WK_allVersions_ohneText.dta"
#     )

#     #--------------------------------------------------
#     # return

#     return(org_data_file_path)
# }