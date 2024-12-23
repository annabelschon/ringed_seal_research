 'reinit'

 'open averages_2D.ctl'

 'set gxout grfill'
 'set mpdraw off'
 'set mproj scaled'

  left = 0.5
  right = 0.5
  top = 0.75
  bottom = 0.5
 'set_parea_pl 'left' 'right' 'top' 'bottom

 tt = 1
 while ( tt <=12 )

   'set t 'tt
    say tt

   'set grads off'
   'set xlab %g'
   'set ylab %g'

   'd conc_month'
   'cbarn'

   'q time'
    ret=sublin(result,1)
    date=subwrd(ret,3)
*   hr=substr(date,1,2)
*   dy=substr(date,4,2)
    mo=substr(date,6,3)
*   yr=substr(date,9,4)

   'draw title 43-Year Average Sea Ice Concentration (%)   'mo
   'printim avg_sea_ice_'mo'.png png white'

    pull dummy
   'c'

   tt = tt + 1
 endwhile

