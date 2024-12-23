 'reinit'

 'open glen_monthly.ctl'
 'open ice_conc_mask.ctl'

 'set mproj scaled'
 'set mpdraw off'

  left = 0.25
  right = 0.5
  top = 1.0
  bottom = 0.5
 'set_parea_pl 'left' 'right' 'top' 'bottom

 'set rgb 21 220 220 220'
 'set rgb 22 100 100 100'

 'set dbuff on'

 tt = 1

  while ( tt <= 516)

   'set grads off'
   'set xlab off'
   'set ylab off'

   'set t 'tt

   'set gxout grfill'
   'set clevs 0 10 20 30 40 50 60 70 80 90 100'
   'set ccols 1 14 4 11 5 13 10 7 12 8 2 6'
   'd monthly_conc.1'
   'cbarn'
 
   'set gxout fgrid'
   'set fgvals 1.0 22 2.0 0'
   'd mask.2(t=1)'
   'cbarn'

   'q time'
    ret=sublin(result,1)
    date=subwrd(ret,3)
*   dy=substr(date,4,2)
    mo=substr(date,6,3)
    yr=substr(date,9,4)

   'draw title Ice Concentration (0 - 100 %)\ 'mo' 'yr

*   'printim frame_'tt'.png png'  * Save each frame as an image

   'swap'

*    pull dummy
*  'c'

*   tt = tt + 1
*    tt = tt + 10
     tt = tt + 3 
*   tt = tt + 180

  endwhile

