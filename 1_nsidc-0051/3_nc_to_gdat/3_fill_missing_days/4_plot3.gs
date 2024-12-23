 'reinit'

 'open ice_conc_v3.ctl'

 'set mproj scaled'
 'set mpdraw off'

  left = 0.25
  right = 0.5
  top = 1.0
  bottom = 0.5
 'set_parea_pl 'left' 'right' 'top' 'bottom

 'set dbuff on'

 tt = 1

  while ( tt <= 16162 )

   'set grads off'
   'set xlab off'
   'set ylab off'

   'set t 'tt

   'set gxout grfill'
   'd 0.004*conc'
   'cbarn'

   'set gxout fgrid'
   'set fgvals 254 15 253 0 252 1 251 3'
   'd conc'

   'q time'
    ret=sublin(result,1)
    date=subwrd(ret,3)
    dy=substr(date,4,2)
    mo=substr(date,6,3)
    yr=substr(date,9,4)

   'draw title Ice Concentration (0-1)\'dy' 'mo' 'yr

*   'printim frame_'tt'.png png'  * Save each frame as an image

   'swap'

*   pull dummy
*  'c'

*   tt = tt + 1
*   tt = tt + 30
    tt = tt + 90

  endwhile

