 'reinit'
 'set display color white'
 'c'

 'open ../density_hires_pts.ctl'
 'open ../snowlayers_pts.ctl'
 'open ../../../obs/2_mk_grads/obs.ctl'

 'set grads off'
 'set datawarn off'
 'set mpdraw off'
 'set mproj scaled'

 'set parea 1.0 7.5 4 9'

 'set xlopts 1 5 0.14'
 'set ylopts 1 5 0.14'

  max_layers = 25

 'set x 1'
 'set y 1'
*'set z 1 70'
 'set z 1 140'
 'set t 0 2184'

 'set xlab off'
 'set ylab off'
*'set vrange 0 70'
 'set vrange 0 140'
 'set gxout shaded'
 'd rosnow'
 'cbarn 1.0 0.0 4.25 3.2'

 'set dfile 2'
*'set vrange 0 70'
 'set vrange 0 140'
 'set z 1'

 layer = 1
 while (layer <= max_layers+1)
   'set ccolor 1'
   'set cstyle 0'
   'set cthick 6'
   'set digsiz 0.0002'
   'set cmark 2'
   'd 100*layers.2(z='layer')'
    layer = layer + 1
 endwhile

 'set xlab on'
 'set ylab on'

 'set z 1'
 'set ccolor 1'
 'set cstyle 1'
 'set cthick 10'
 'set cmark 0'
 'd 100*snod.2'

 'set ccolor 2'
 'set cstyle 1'
 'set cthick 10'
 'set cmark 6'
 'set digsiz 0.2'
 'd 100*snod.3'

