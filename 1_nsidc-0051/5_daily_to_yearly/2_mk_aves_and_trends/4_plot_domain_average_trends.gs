 'reinit'

 'open domain_average_trends.ctl'

 'set t 1 last'

 'set x 1'
 yy = 1
 while ( yy <=12 )

   'set y 'yy
    say yy

   'set grads off'

   'set vrange 0 40'

   'set cmark 0'
   'd conc_month(x=1)'
   'set cmark 0'
   'd conc_month(x=2)'

   'draw title Domain-Average Sea Ice Concentration (%)'
   'printim domain_sea_ice_average_trends'yy'.png png white'

*    pull dummy

    yy = yy + 1
 endwhile

