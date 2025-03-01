 'reinit'
 'set display color white'
 'c'

 'open ../data/snod_den_assim.ctl'
 'open ../../observations/2_mk_grads/obs.ctl'

 'set grads off'
 'set mpdraw off'
 'set mproj scaled'

 'set parea 1.0 7.5 4 9'

 'set xlopts 1 5 0.14'
 'set ylopts 1 5 0.14'

 'set t 1 273'
 'set x 48'
 'set y 50'

 'set vrange 0 140'

 'set ccolor 1'
 'set cstyle 1'
 'set cthick 10'
 'set cmark 0'
 'd 100*snod.1'

 'set dfile 2'

 'set t 1 2184'
 'set x 1'
 'set y 1'

 'set ccolor 2'
 'set cstyle 1'
 'set cthick 10'
 'set cmark 6'
 'set digsiz 0.2'
 'd 100*snod.2'

