 'reinit'
 'set display color white'
 'c'

 'open ../../../../../topo_vege/NoAm_30m/topo_vege_km.ctl'
 'open ../4_spherical_model/semivario_spherical_model.ctl'

 'set vpage 0 8.5 7.33 11.0'

 'set parea 2.5 7.0 0.8 3.5'
 'set grads off'
 'set xlab on'
 'set ylab on'
 'set xlab %g'

 'set xlopts 1 5 0.15'
 'set ylopts 1 5 0.15'
 'set xlab %g'
 'set ylab %g'

 'set x 751'
 'set y 1 2528'

 'set vrange 0 2300'
 'set ylint 500'

 'set ccolor 1'
 'set cstyle 1'
 'set cmark 0'
 'set cthick 8'
 'd topo'

  frame

 'set string 1 c 6 0'
 'set strsiz 0.20'
 'draw string 4.75 0.3 distance (km)'

 'set string 1 c 6 90'
 'set strsiz 0.20'
 'draw string 1.5 2.15 topography (m)'

  abcd(a,'br')

************************************

 'set vpage 0 8.5 3.67 7.33'

 'set dfile 2'
 'set x 1'
 'set y 1'
 'set t 1'

 'set grads off'
*'set xlab on'
*'set ylab on'

 'set xlopts 1 5 0.15'
 'set ylopts 1 5 0.15'
 'set xlab %g'
 'set ylab %g'

 'set lon 0 15'
 'set vrange 0 27000'
 'set xlint 2'
 'set ylint 5000'

 'set ccolor 1'
 'set cstyle 1'
 'set cmark 0'
 'set cthick 8'
 'd spheric'

 'set ccolor 2'
 'set cstyle 1'
 'set cmark 3'
 'set digsiz 0.16'
 'set cthick 8'
 'd semivar'

  frame

* plot a vertical line at the model lag distance = 11.5 km.
  lag = 11.5
  sill = 23569.953
 'q w2xy 'lag' 'sill
  x1=subwrd(result,3)
  y1=subwrd(result,6)
  say x1' 'y1
*'set line 4 1 10'
*'draw mark 2 'x1' 'y1' 0.20'

  sill = 0.0
 'q w2xy 'lag' 'sill
  x2=subwrd(result,3)
  y2=subwrd(result,6)
  say x2' 'y2
 'set line 1 1 8'
 'draw line 'x1' 'y1' 'x1' 'y2

 'set string 1 c 6 90'
 'set strsiz 0.20'
  xx=x1-0.22
  yy=(y1+y2)/2
 'draw string 'xx-0.35' 'yy' range ='
 'draw string 'xx' 'yy' 11.5 km'


 'set string 1 c 6 0'
 'set strsiz 0.20'
 'draw string 4.75 0.3 distance (km)'

 'set string 1 c 6 90'
 'set strsiz 0.20'
 'draw string 1.5 2.15 semivariance (`3c`0)'

  abcd(b,'br')

************************************

 'gprint fig_files/lag'

************************************
************************************

************************************
************************************
************************************
 function abcd(a,pos)
  'query gxinfo'
   rec3 = sublin(result,3)
   rec4 = sublin(result,4)
   xlo  = subwrd(rec3,4)
   xhi =  subwrd(rec3,6)
   ylo =  subwrd(rec4,4)
   yhi =  subwrd(rec4,6)

   delta = 0.20
  'set string 1 c 6 0'
  'set strsiz 0.20'
  'set line 1 1 6'

  if (pos='tr')
    x1 = xhi - 2*delta
    y1 = yhi - 2*delta
    x2 = xhi
    y2 = yhi
    xpos = xhi - delta
    ypos = yhi - delta
  endif

  if (pos='br')
    x1 = xhi - 2*delta
    y1 = ylo
    x2 = xhi
    y2 = ylo + 2*delta
    xpos = xhi - delta
    ypos = ylo + delta
  endif

  if (pos='tl')
    x1 = xlo
    y1 = yhi - 2*delta
    x2 = xlo + 2*delta
    y2 = yhi
    xpos = xlo + delta
    ypos = yhi - delta
  endif

  if (pos='bl')
    x1 = xlo
    y1 = ylo
    x2 = xlo + 2*delta
    y2 = ylo + 2*delta
    xpos = xlo + delta
    ypos = ylo + delta
  endif

  'set line 0'
  'draw recf 'x1' 'y1' 'x2' 'y2
  'set line 1 1'
  'draw rec 'x1' 'y1' 'x2' 'y2
  'draw string 'xpos' 'ypos' 'a

 return
************************************

