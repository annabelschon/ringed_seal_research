 'reinit'

* Define the directory path and file name where you want the
*   output file written to.
  fname = '/data3/annabel/seals/1_nsidc_0051/2_gdat_orig/ice_conc.gdat'

* Open the .ctl file.
 'open ../1_looking_at_nc_files/1_ice_conc_nc_files.ctl'

* Define the undef value you want to use in the outputs.
 'set undef -9999.0'

* Define the dimension environment.
 'set x 1 304'
 'set y 1 448'
* tt_max = 16162

* Force grads to write the data to a .gdat file instead of
*   plotting it on the screen
 'set gxout fwrite'
 'set fwrite 'fname

* Loop through the .nc files, writting the data to a single .gdat
*   file containing the entire sea ice concentration archive.
*   Deal with the fact that there are 5 diffent variable names in
*   the .nc files, that change at different times.
  var_groups = 5

  var_names = 'n07_icecon f08_icecon f11_icecon f13_icecon f17_icecon'

* Or you could do it this way.
* var_names.1 = 'n07_icecon'
* var_names.2 = 'f08_icecon'
* var_names.3 = 'f11_icecon'
* var_names.4 = 'f13_icecon'
* var_names.5 = 'f17_icecon'

  fname = '../1_looking_at_nc_files/conc_times.dat'

  vv = 1
  while ( vv <= var_groups )

    return = read(fname)
    record = sublin(return,2)
    tt_srt = subwrd(record,1)
    tt_end = subwrd(record,2)

    varname = subwrd(var_names,vv)

    say
    say vv'  'tt_srt'  'tt_end'  'varname
    say vv'  'tt_srt'  'tt_end'  'varname
    say vv'  'tt_srt'  'tt_end'  'varname
    say

    tt = tt_srt
    while ( tt <= tt_end )
     'set t 'tt

      say 'time 'tt' 'varname
*      d varname'
      'd 'varname

     'c'
*   pull dummy
      tt = tt + 1
*     tt = tt + 1000
    endwhile

    vv = vv + 1
  endwhile

* Close the .gdat file.
 'disable fwrite'

