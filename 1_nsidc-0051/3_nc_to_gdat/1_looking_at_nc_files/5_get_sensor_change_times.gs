 'reinit'

 'open 1_ice_conc_nc_files.ctl'

 'set time 01JAN1980 20AUG1987'
 'q dims'
  line = sublin(result,5)
  tt1_srt = subwrd(line,11)
  tt1_end = subwrd(line,13)

 'set time 21AUG1987 18DEC1991'
 'q dims'
  line = sublin(result,5)
  tt2_srt = subwrd(line,11)
  tt2_end = subwrd(line,13)

 'set time 19DEC1991 29SEP1995'
 'q dims'
  line = sublin(result,5)
  tt3_srt = subwrd(line,11)
  tt3_end = subwrd(line,13)

 'set time 30SEP1995 31DEC2007'
 'q dims'
  line = sublin(result,5)
  tt4_srt = subwrd(line,11)
  tt4_end = subwrd(line,13)

 'set time 01JAN2008 31MAR2024'
 'q dims'
  line = sublin(result,5)
  tt5_srt = subwrd(line,11)
  tt5_end = subwrd(line,13)

* Save the information to a file.
  rc = write(conc_times.dat,tt1_srt' 'tt1_end)
  rc = write(conc_times.dat,tt2_srt' 'tt2_end)
  rc = write(conc_times.dat,tt3_srt' 'tt3_end)
  rc = write(conc_times.dat,tt4_srt' 'tt4_end)
  rc = write(conc_times.dat,tt5_srt' 'tt5_end)

  rc = close (conc_times.dat)

