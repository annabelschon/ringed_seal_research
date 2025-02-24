 'reinit'
 'set display color white'
 'c'

 'open ../cap_info.ctl'
 'open ../../../../topo_vege/NoAm_30m/topo_vege.ctl'

 'set mproj scaled'
 'set mpdraw off'
 'set grads off'

* nx=2649, ny=2528, ratio=1.05, 0.95
 'set parea 1.25 9.11 0.5 8.0'

*'set xlopts 1 5 0.18'
*'set ylopts 1 5 0.18'

*'set xlab %gkm'
*'set ylab %gkm'

 'set xlab off'
 'set ylab off'

*'set xlint 50'
*'set ylint 50'

 'set rgb 44 220 220 220'

 'set gxout fgrid'
 'set fgvals 0 1 1 15 2 0'
 'd cap'

* light blue (water areas)
 'set rgb 32 202 225 255'
* grey
 'set rgb 44 220 220 220'

 'set gxout fgrid'
*'set fgvals 24 44'
 'set fgvals 24 32'
 'd vege.2(t=1)'

  legend()

 'gprint_png fig_files/cap'

******************************
******************************

  function legend()

  'query gxinfo'
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xlo  = subwrd(rec3,4)
  xhi =  subwrd(rec3,6)
  ylo =  subwrd(rec4,4)
  yhi =  subwrd(rec4,6)

* say xlo
* say xhi
* say ylo
* say yhi

  xlo=xhi-2.5
  ylo=yhi-1.2

  xr=xhi
  xl=xlo

  yt=yhi
  yb=ylo

  xsiz=xr-xl
  ysiz=yt-yb

  dy=ysiz/6

  xs1=xl+0.1
  xs3=xr-0.4

  ys1=yt-ysiz*1./6.
  ys2=yt-ysiz*2./6.
  ys3=yt-ysiz*3./6.
  ys4=yt-ysiz*4./6.
  ys5=yt-ysiz*5./6.

* draw the box.
  'set line 0 1 4'
  'draw recf 'xl' 'yb' 'xr' 'yt
  'set line 1 1 4'
  'draw rec 'xl' 'yb' 'xr' 'yt
* 'draw line 'xlo' 'ys2' 'xhi' 'ys2
* 'draw line 'xlo' 'ys4' 'xhi' 'ys4

  'set strsiz 0.14'
  'set string 1 l 4 0'

  'draw string 'xs1' 'ys1' No CAP'
  'draw string 'xs1' 'ys3' Possible CAP'
  'draw string 'xs1' 'ys5' Likely CAP'

  xs3l=xs3-0.20
  xs3r=xs3+0.20

  'set line 1 1 4'
  'draw recf 'xs3l' 'ys1-0.1' 'xs3r' 'ys1+0.1

  'set line 15 1 4'
  'draw recf 'xs3l' 'ys3-0.1' 'xs3r' 'ys3+0.1

  'set line 1 1 4'
  'draw rec 'xs3l' 'ys5-0.1' 'xs3r' 'ys5+0.1

  return

******************************
******************************

