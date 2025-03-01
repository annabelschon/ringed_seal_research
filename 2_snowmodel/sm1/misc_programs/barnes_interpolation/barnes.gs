 'reinit'
 'set display color white'
 'c'

 'open obs_gridded.ctl'

 'set mpdraw off'

 'set parea 1.0 7.5 2.0 8.5'

 'set vpage 0.0 4.25 5.5 11.0'
 'set grads off'
 'set mproj scaled'
 'set xlab %gkm'
 'set ylab %gkm'
 'set gxout grfill'
 'set clevs 0.2 0.25 0.3 0.35 0.4 0.45 0.5'
 'd obs(t=10)'
 'draw title beta = 0.1'
 'cbarn'

 'set vpage 4.25 8.5 5.5 11.0'
 'set grads off'
 'set mproj scaled'
 'set xlab %gkm'
 'set ylab %gkm'
 'set gxout grfill'
 'set clevs 0.2 0.25 0.3 0.35 0.4 0.45 0.5'
 'd obs(t=7)'
 'draw title beta = 0.4'
 'cbarn'

 'set vpage 0.0 4.25 1.0 6.5'
 'set grads off'
 'set mproj scaled'
 'set xlab %gkm'
 'set ylab %gkm'
 'set gxout grfill'
 'set clevs 0.2 0.25 0.3 0.35 0.4 0.45 0.5'
 'd obs(t=4)'
 'draw title beta = 0.7'
 'cbarn'

 'set vpage 4.25 8.5 1.0 6.5'
 'set grads off'
 'set mproj scaled'
 'set xlab %gkm'
 'set ylab %gkm'
 'set gxout grfill'
 'set clevs 0.2 0.25 0.3 0.35 0.4 0.45 0.5'
 'd obs(t=1)'
 'draw title beta = 1.0'
 'cbarn'

*'gprint two_dim'

************************************

  pull dummy

 'reinit'
 'set display color white'
 'c'

 'open obs_gridded.ctl'

 'set grads off'
 'set mpdraw off'
 'set mproj scaled'
 'set xlab %gkm'

 'set parea 1.0 8.0 2.0 9.0'

 'set y 501'
 'd obs(t=10)'
 'd obs(t=7)'
 'd obs(t=4)'
 'd obs(t=1)'

 'draw title beta = 0.1, 0.4, 0.7, and 1.0\(at y = 501, or 15 km)'

*'gprint crossection'

************************************

