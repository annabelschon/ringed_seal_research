'reinit'
'open 2_ice_conc_extracted_domain.ctl'

'set display color white'
'c'

'set mpdraw off'
'set grads off'

'set gxout grfill'
*left = 0.25
*right = 0.5
*top = 1.0
*bottom = 0.5

*'set_parea_pl 'left' 'right' 'top' 'bottom
*'set dbuff on'

'set xlab %g'
'set ylab %g'

* Select a time step for the spatial plot
'set t 4000' 

'q time'
 ret=sublin(result,1)
 date=subwrd(ret,3)
 dy=substr(date,4,2)
 mo=substr(date,6,3)
 yr=substr(date,9,4)

'draw title Ice Concentration (0-1)\'dy' 'mo' 'y

* Scale ice concentration data 
'd conc'

* color bar
'cbarn'

* specify that output should be displayed as a filled grid of values rather than as
* the default contour or shaded plot
'set gxout fgrid'
'set fgvals 254 15 253 0 252 1 251 3'
'd conc'
