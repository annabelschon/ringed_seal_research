'reinit'

'open bsfx.ctl'
'open /home/aschoen/seals/1_nsidc-0051/5_daily_to_yearly/1_mk_yearly_data/ice_conc_mask.ctl'

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
  
  while ( tt <= 500)

   'set grads off'
   'set xlab off'
   'set ylab off'

   'set t 'tt

   'set gxout grfill'
   'set fgvals 1.0 1.0 2.0 0'
   'd mask.2(t=1)'

   'set gxout grfill'
*  'set clevs 0.125 0.25 0.5 0.75 1 1.25 1.5 1.75 2'
*  'set ccols 1 14 4 11 5 13 10 7 12'
   'd bsfx.1'
   'cbarn'

   'q time'
   ret=sublin(result,1)
   date=subwrd(ret,3)
   mo=substr(date,6,3)
   yr=substr(date,9,4)

   'draw title Blowing Snow Flux (kg/m*s)\ 'date

   'swap'

        tt = tt + 8

    endwhile
