# RWI-GEO-REDX: A Housing Price Index for Germany

This repository shows the detailed preparation and generation of RWI-GEO-REDX price index.

## Abstract Data Description (V15)

The FDZ Ruhr at RWI provides regional price indices for apartments and houses (rentals and sales) in Germany since 2008 through the RWI-GEO-REDX dataset, based on property listings from ImmoScout24 (RWI-GEO-RED).

RWI-GEO-REDX stands out for its high spatial resolution: the exact location of each listing allows price indices to be calculated at the level of 1×1 km grid cells. Results are aggregated at regional levels including grid cells, municipalities, counties, and labor market regions. The indices are reported as quality-adjusted absolute prices, including annual and regional changes. 
A key feature is the location-adjusted national index, which accounts for shifts in the regional composition of listings over time.

This repository constitutes an updated version of previous reports and refers to RWI-GEO-REDX V15, which encompasses data up to November 2025.

## Access

The data can be obtained as Public Use File (PUF) or Scientific Use File (SUF) from the FDZ Ruhr at RWI. The FDZ Ruhr is the research data center of the RWI - Leibniz Institute for Economic Research. In order to ensure that the indices are not driven by small sample sizes, the PUF dataset includes only those indices that are based on at least 50 observations per year and region. The indices based on less than 50 observations per year and region are also available on request as an SUF for scientific research purposes only. Since the RWI-GEO-REDX subsumes aggregated information, it does not contain any information that is restricted for data security reasons. The indices presented here are available as Excel (.xlsx) files and as CSV / parquet files for the grid indices.

Data access does not require a data use agreement, but users must register to access the data. Interested users should visit our website https://www.rwi-essen.de/en/research-advice/further/research-data-center-ruhr-fdz/data-access.

It is not permitted for users to undertake any action that might result in the re-identification of individual homes or apartments.

Users are requested to cite the source correctly and to inform the FDZ Ruhr about publications using the data. When using the dataset RWI-GEO-REDX, please cite the data as RWI (2025): RWI-GEO-REDX: Regional Real Estate Price Index for Germany, 2008-11/2024 (V15). Version: 1. RWI – Leibniz Institute for Economic Research. Dataset. http://doi.org/10.7807/immo:redx:puf:v15. Further, we recommend citing the data description.

For more details, especially on the estimates, please refer to the [data report](https://www.rwi-essen.de/fileadmin/user_upload/RWI/FDZ/Datenbeschreibung-REDX-v15.pdf).

## More Information

- [General information on RWI-GEO-RED/X](https://www.rwi-essen.de/en/research-advice/further/research-data-center-ruhr-fdz/data-sets/rwi-geo-red/x-real-estate-data-and-price-indices)
- [Data report RWI-GEO-REDX V15](https://www.rwi-essen.de/fileadmin/user_upload/RWI/FDZ/Datenbeschreibung-REDX-v15.pdf). Please cite the data report as: Thiel (2025), FDZ Data Description: Regional Real Estate Price Index for Germany, 2008-11/2024 (V15), RWI Projektberichte, Essen

## DOI
- Repository for V15: [![DOI:10.5281/zenodo.16609434](http://img.shields.io/badge/DOI-10.5281/zenodo.16609434-048BC0.svg)](https://zenodo.org/account/settings/github/repository/PThie/RWI-GEO-REDX)
- RWI-GEO-REDX V15 (PUF): http://doi.org/10.7807/immo:redx:puf:v15
- RWI-GEO-REDX V15 (SUF): http://doi.org/10.7807/immo:redx:suf:v15

## Contact Person

Please contact [Dr. Patrick Thiel](https://www.rwi-essen.de/rwi/team/person/patrick-thiel) in case of questions.

## Disclaimer

All rights reserved to RWI and the author of the code, [Dr. Patrick Thiel](https://www.rwi-essen.de/rwi/team/person/patrick-thiel). In the case of used libraries, the main file ([_targets.R](https://github.com/PThie/RWI-GEO-REDX/blob/main/_targets.R)) should be consulted. For a comprehensive list, including direct dependencies, refer to the [renv.lock file](https://github.com/PThie/RWI-GEO-REDX/blob/main/renv.lock). Please note that the terms of conditions of each library apply.
