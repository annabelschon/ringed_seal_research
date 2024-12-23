 'reinit'

 'open 1_ice_conc_nc_files.ctl'
 'set mpdraw off'
 'set grads off'

 'set xlab %g'
 'set ylab %g'

 'set t 2'

 'set gxout grfill'
*'d n07_icecon'
 'd 0.004*n07_icecon'
 'cbarn'

 'set gxout fgrid'
 'set fgvals 254 15 253 0 252 1 251 3'
 'd n07_icecon'

