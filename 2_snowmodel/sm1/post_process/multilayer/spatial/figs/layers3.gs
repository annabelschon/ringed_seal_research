 'reinit' 
 'set display color white'
 'c'

 'open ../multilayer.ctl'
 'open ../../../../snowmodel1/ctl_files/snowpack.ctl'

 'set grads off'
 'set datawarn off'

 'set vpage 0 8.5 5.5 11'
 'set parea 1 7.5 1.25 5.25'
 'set grads off'
 'set mproj scaled'

 'set xlopts 1 5 0.15'
 'set ylopts 1 5 0.15'
 'set ylint 10'

 'set vrange 0 80'

 'set x 48'
 'set y 50'
 'set z 1'
 'set t 1 2184'

 max_layers = 25

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
     'define layer=100*snodml.1(z='iter')'
    else
     'define layer=layer+100*snodml.1(z='iter')'
    endif

   'd layer'

    iter = iter + 1
 endwhile

  pull dummy

 'set xlab on'
 'set ylab on'
 'set cstyle 1'
 'set ccolor 1'
 'set lwid 13 3.0'
 'set cthick 13'
 'd 100*snodtop.1'

 'set cstyle 1'
 'set ccolor 2'
 'set lwid 13 1.0'
 'set cthick 13'
 'd 100*snod.2'

 'frame'

 'set string 1 c 6 90'
 'set strsiz 0.20'
 'draw string 0.28 3.25 Snow Depth (cm)'

 'set string 1 c 6 0'

 'frame'

*'gprint layers'

************************************
************************************

