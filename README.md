## README (Project: Sea ice analysis, Snowmodel, Ringed Seals)

### Overview
This repository contains scripts and data related to the analysis of NSIDC-0051 sea ice concentration data. Note that the project does not include the data (in .GDAT format) as that requires too much storage space.
The project is structured into several distinct phases, each responsible for different aspects of the data processing and analysis pipeline.

### Directory Structure

- `1_nsidc-0051/` - Root directory for the NSIDC-0051 dataset processing.
  - `1_info/` - Documentation and metadata about the NSIDC-0051 dataset.
  - `2_download/` - Scripts for downloading the dataset.
  - `3_nc_to_gdat/` - Conversion scripts from NetCDF to GDAT format.
    - `1_looking_at_nc_files/` - Scripts for inspecting NetCDF files.
    - `2_mk_gdat_from_nc/` - Scripts to convert NetCDF files to GDAT format.
    - `3_fill_missing_days/` - Scripts to handle missing days in the dataset.
  - `4_extract_temporal_subdomain/` - Scripts to extract specific temporal ranges and subdomains.
    - `1_maxiter_offset/` - Adjustments for maximum iteration offsets.
    - `2_extract_years/` - Yearly data extraction scripts.
    - `3_fill_polar_hole/` - Scripts to fill data gaps in the polar regions.
  - `5_daily_to_yearly/` - Conversion from daily data to annual summaries.
  - `6_regrid_merra2/` - Scripts to regrid data to match the MERRA-2 reanalysis dataset.

### Branching Strategy

- The `main` branch contains the general project framework and documentation.
- Specific analyses like `1_nsidc-0051` are maintained on separate branches to isolate development and make specific pull requests easier to manage.
- Over the course of the project branches like 2_merra-2 and 3_snowmodelsimulation will be added as seperate branches.
