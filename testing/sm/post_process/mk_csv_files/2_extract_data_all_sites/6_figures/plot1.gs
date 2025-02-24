 'reinit'
 'open ../5_extract/point_variables.ctl'

 'set t 1 4748'
 'set vrange 0.0 2.0' 
* Olnes pts:
  point = 1
  while (point <= 30)
* DiFolco pts:
* point = 31
* while (point <= 37)
* FTE pts:
* point = 38
* while (point <= 73)

   'set x 'point
   'set cmark 0'
   'd snod'
   'draw title Olnes plots'
*  'draw title DiFolco plots'
*  'draw title FTE  plots'

    say point
    pull dummy
*  'c'

    point = point + 1
*   point = point + 3

  endwhile

