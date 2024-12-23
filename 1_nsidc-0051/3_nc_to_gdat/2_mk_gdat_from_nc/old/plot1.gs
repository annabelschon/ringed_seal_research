 'reinit'

 'open ice_conc.ctl'
 'set mpdraw off'
 'set grads off'

 'set xlab %g'
 'set ylab %g'

 'set gxout grfill'
*'d n07_icecon'
 'd 0.004*conc'
 'cbarn'

 'set gxout fgrid'
 'set fgvals 254 15 253 0 252 1 251 3'
 'd conc'

