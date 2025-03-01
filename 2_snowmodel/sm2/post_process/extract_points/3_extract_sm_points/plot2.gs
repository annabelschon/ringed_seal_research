 'reinit'
 'open point_variables.ctl'
 'open point_variables_aves.ctl'

*'set t 1 14610'
 'set t 1 1095'
 
  point = 1
  while (point <= 9)

   'set x 'point
   'set cmark 0'
*  'd tair.1'
   'd snod.1'
   'set cmark 0'
*  'd tair.2'
   'd snod.2'

    pull dummy
   'c'

    point = point + 1
  endwhile

