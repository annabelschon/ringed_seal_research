 'reinit'
 'open large_curvature_wt.ctl'

 'set grads off'
 'set mpdraw off'

 'set xlab %g'
 'set ylab %g'
 'set gxout grfill'
 'd curve'
 'cbarn'
 'draw title curvature'

 'set gxout contour'
 'set cint 200'
 'set clab off'
 'd topo'

 pull dummy
 'c'

 'set xlab %g'
 'set ylab %g'
 'set gxout grfill'
 'd curvewt'
 'cbarn'
 'draw title curvewt'
 'set gxout contour'
 'set cint 200'
 'set clab off'
 'd topo'

