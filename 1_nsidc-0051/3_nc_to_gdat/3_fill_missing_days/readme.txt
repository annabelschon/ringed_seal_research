The .f script creates several versions of the filled data and saves them in different files for comparison or
further analysis. 

- `conc_1` is used for filling missing data with the **previous valid day**.
- `conc_2` is used for **simple linear interpolation**.
- `conc_3` is used for **complex linear interpolation**.
- `conc_4` is used for **nearest neighbor interpolation**.

The final .gdat files are stored as different versions (v1-v4) in
'/data3/annabel/seals/1_nsidc_0051/3_gdat_final/'. There is a big chunk of data missing in 1987 November -
1988 December. 
