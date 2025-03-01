 'reinit'

 'open ../ctl_files/wo_assim/multilayer_snod.ctl'
 'open ../ctl_files/wo_assim/multilayer_2Dxy.ctl'
*'open ../ctl_files/wo_assim/multilayer_temp.ctl'

 'set grads off'
 'set datawarn off'

 'set x 20'
 'set y 15'
 'set z 1'
*'set t 1 365'
 'set time 12Z01SEP2017 12Z16JUL2018'

 max_layers = 25

 'set ylint 10'

 'set vrange 0 130'

 iter = 1
 while (iter <= max_layers+1)
   'set xlab off'
   'set ylab off'
   'set ccolor 1'
   'set cstyle 1'
   'set cthick 6'
   'set digsiz 0.001'
   'set cmark 2'
   'set cstyle 0'
   'set ccolor 1'

    if (iter = 1)
     'define layer=100*snodz.1(z='iter')'
    else
     'define layer=layer+100*snodz.1(z='iter')'
    endif

   'd layer'

    iter = iter + 1
 endwhile

 'set xlab on'
 'set ylab on'
 'set cstyle 1'
 'set ccolor 1'
 'set lwid 13 3.0'
 'set cthick 13'
 'd 100*snod.2'

 'frame'

 'draw ylab Snow Depth (cm)'

************************************
************************************

