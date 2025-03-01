## README (Project: Sea ice analysis, Snowmodel, Ringed Seals)

### Overview
This repository contains scripts and data related to the analysis of NSIDC-0051 sea ice concentration data. Note that the project does not include the data (in .GDAT format) as that requires too much storage space.
The project is structured into several distinct phases, each responsible for different aspects of the data processing and analysis pipeline.

### Directory Structure

- **1_nsidc-0051/**: Root directory for the NSIDC-0051 dataset processing.
  - **1_info/**: Documentation and metadata about the NSIDC-0051 dataset.
  - **2_download/**: Scripts for downloading the dataset.
  - **3_nc_to_gdat/**: Conversion scripts from NetCDF to GDAT format.
    - **1_looking_at_nc_files/**: Scripts for inspecting NetCDF files.
    - **2_mk_gdat_from_nc/**: Scripts to convert NetCDF files to GDAT format.
    - **3_fill_missing_days/**: Scripts to handle missing days in the dataset.
    - **4_extract_temporal_subdomain/**: Scripts to extract specific temporal ranges and subdomains.
      - **1_maxiter_offset/**: Adjustments for maximum iteration offsets.
      - **2_extract_years/**: Scripts for extraction of the select temporal domain.
      - **3_fill_polar_hole/**: Scripts to fill data gaps in the polar regions.
  - **5_daily_to_yearly/**: Conversion from daily data to annual summaries, trends, and analysis.
    - **1_mk_yearly_data/**: Scripts to aggregate daily data into monthly data.
    - **2_mk_aves_and_trends/**: Scripts to calculate averages and identify trends.
    - **3_mk_figs/**: Scripts and resources to generate figures and graphs for trend data analysis.
- **2_snowmodel/**: Contains scripts and configurations for SnowModel simulations.
- **3_post_process/**: Scripts for post-processing data, including data aggregations (i.e., make yearly data) and visualizations.
- **4_figures/**: Directory for storing generated figures and graphics (Currently from Kovacs et al., 2024 paper).
