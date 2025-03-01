 'reinit'
 'set display color white'
 'c'

 'open ../cap_info.ctl'
 'open ../../../../topo_vege/NoAm_30m/topo_vege.ctl'

 'set mproj scaled'
 'set mpdraw off'
 'set grads off'

* nx=2649, ny=2528, ratio=1.05, 0.95
 'set parea 1.25 9.11 0.5 8.0'

*'set xlopts 1 5 0.18'
*'set ylopts 1 5 0.18'

*'set xlab %gkm'
*'set ylab %gkm'

 'set xlab off'
 'set ylab off'

*'set xlint 50'
*'set ylint 50'

 'set rgb 44 220 220 220'

 'set gxout grfill'
 'd cap_smth'
 'cbarn 1.0 1 9.6 4.25'

* light blue (water areas)
 'set rgb 32 202 225 255'
* grey
 'set rgb 44 220 220 220'

 'set gxout fgrid'
 'set fgvals 24 44'
*'set fgvals 24 32'
 'd vege.2(t=1)'

 'gprint_png fig_files/cap_smth'

