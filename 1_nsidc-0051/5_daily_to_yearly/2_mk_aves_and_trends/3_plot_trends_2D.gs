 'reinit'

 'open trends_2D.ctl'

 'set gxout grfill'
 'set mpdraw off'
 'set mproj scaled'

  left = 0.5
  right = 0.5
  top = 0.75
  bottom = 0.5
 'set_parea_pl 'left' 'right' 'top' 'bottom

* BLUE shades
 'set rgb 16   0   0 255'
 'set rgb 17  55  55 255'
 'set rgb 18 110 110 255'
 'set rgb 19 165 165 255'
 'set rgb 20 220 220 255'
* RED shades
 'set rgb 21 255 220 220'
 'set rgb 22 255 165 165'
 'set rgb 23 255 110 110'
 'set rgb 24 255  55  55'
 'set rgb 25 255   0   0'

 tt = 1
 while ( tt <=12 )

   'set t 'tt
    say tt

   'set grads off'
   'set xlab %g'
   'set ylab %g'

* Note that this asssumes clevs has 4 negative values, one 0,
*   and 4 positive values.
   'set clevs -20 -15 -10 -5 0 5 10 15 20'
   'set ccols 16 17 18 19 20 21 22 23 24 25'

*  'd conc_month'
   'd 10*conc_month'
   'cbarn'

   'q time'
    ret=sublin(result,1)
    date=subwrd(ret,3)
*   hr=substr(date,1,2)
*   dy=substr(date,4,2)
    mo=substr(date,6,3)
*   yr=substr(date,9,4)

*  'draw title 43-Year Sea Ice Concentration Trends (%/year)   'mo
   'draw title 43-Year Sea Ice Concentration Trends (%/decade)   'mo
   'printim avg_sea_ice_change_'mo'.png png white'

    pull dummy
   'c'

   tt = tt + 1
 endwhile

