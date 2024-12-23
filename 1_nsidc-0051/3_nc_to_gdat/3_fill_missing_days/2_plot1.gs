 'reinit'

*'open ice_conc_v0.ctl'
 'open ice_conc_v1.ctl'

 'set mpdraw off'
 'set grads off'

 'set xlab %g'
 'set ylab %g'

 'set t 1'

 'set gxout grfill'
 'd 0.004*conc'
 'cbarn'

 'set gxout fgrid'
 'set fgvals 254 15 253 0 252 1 251 3'
 'd conc'

