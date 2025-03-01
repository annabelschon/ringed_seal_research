 'reinit'
 'set display color white'
 'c'

 'open ../../ctl_files/wo_assim/swed.ctl'
 'open ../../topo_vege/NoAm_30m/topo_vege.ctl'

 'set grads off'
 'set mproj scaled'
 'set mpdraw off'
 'set parea 1.0 10.0 1.5 7.65'

 'set xlopts 1 5 0.16'
 'set ylopts 1 5 0.16'
 'set xlab %g'
 'set ylab %g'
 'set xlint 5'
 'set ylint 5'

 'set gxout shaded'
 'set time 1apr2018'
 'd 100*swed'
 'cbarn 1 0 5.5 0.8'

 'set gxout contour'
 'd topo.2(t=1)'

 'draw title SWE (cm), 1 April 2018\(black contours = topo)'

 'gprint snow1'

