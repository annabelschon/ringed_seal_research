 'reinit'

 'open ../ctl_files/wo_assim/multilayer_temp.ctl'

 'set grads off'
 'set datawarn off'

 'set x 20'
 'set y 15'
 'set z 0.5 16.5'

 'set ylint 1'

 'set vrange -25 1'

 t=1
 while (t<=365)

   'set grads off'

   'set t 't

   'set cmark 0'
   'set ccolor 1'
   'd tempz'

   'draw title Snow Temperature (`3.`0`nC)'

*   pull dummy

    t = t + 3
 endwhile

