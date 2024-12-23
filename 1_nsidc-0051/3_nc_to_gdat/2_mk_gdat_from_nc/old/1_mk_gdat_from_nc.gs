 'reinit'

* Define the directory path and file name where you want the
*   output file written to.
  fname = '/data3/annabel/nsidc_0051/2_gdat_orig/ice_conc.gdat'

* Open the .ctl file.
 'open ../1_looking_at_nc_files/ice_conc_nc_files.ctl'

* Define the undef value you want to use in the outputs.
 'set undef -9999.0'

* Define the dimension environment.
 'set x 1 304'
 'set y 1 448'
  tt_max = 366

* Force grads to write the data to a .gdat file instead of
*   plotting it on the screen
 'set gxout fwrite'
 'set fwrite 'fname

* Loop through the .nc files, writting the data to a single .gdat
*   file containing the entire sea ice concentration archive.
  tt = 1
  while ( tt <= tt_max )
   'set t 'tt
    say 'time 'tt
   'd n07_icecon'
   'c'
    tt = tt + 1
  endwhile

* Close the .gdat file.
 'disable fwrite'

