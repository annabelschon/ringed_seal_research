 'reinit'
 'set display color white'
 'c'

 'open ../../1_topo_vege/5_mk_final_topo_vege/seals_topo_vege_500m_km.ctl'
*'open ../../1_topo_vege/6_mk_glac_front_mask/glacier_front_mask_500m_km.ctl'
 'open ../../1_topo_vege/6_mk_glac_front_mask/glacier_front_mask_500m_6km_km.ctl'

 'open ../../../2020_work/1_topo_vege/6_final_topo_vege/seals_topo_vege_100m_km.ctl'
 'open ../../../2020_work/1_topo_vege/8_mk_glac_front_mask/seals_gfront_mask_3km_km.ctl'

 'set grads off'
 'set mproj scaled'
 'set mpdraw off'

* nx=690, ny=990, ratio=0.70, 1.43
*'set parea 0.75 7.75 0.5 10.54'
*'set parea 2.0 6.5 4.0 10.4'
*'set parea 1.75 6.75 3.35 10.5'
 'set parea 1.75 6.75 3.65 10.8'

*'set xlab %gkm'
*'set ylab %gkm'
*'set xlint 50'
*'set ylint 50'
*'set xlopts 1 5 0.15'
*'set ylopts 1 5 0.15'
 'set xlab off'
 'set ylab off'

* blues
 'set rgb 21 102 178 255'
 'set rgb 22 153 204 255'
 'set rgb 23 204 229 255'

* browns
*'set rgb 31 101  67  33'
*'set rgb 31 139  69  19'
 'set rgb 31 205 133  63'
*'set rgb 31 210 180 140'

 'set gxout fgrid'
 'set fgvals 18 31 20 0 24 23'
 'd vege.1(t=1)'

 'set gxout fgrid'
*'set fgvals 1 21'
*'set fgvals 1 2'
 'set fgvals 1 4'
 'd glfm.2(t=1)'

  fname = pts.dat
  dx = 500.0
  dy = 500.0
  xmn = 398250.0
  ymn = 8490250.0

*'set clip 1.75 6.75 3.65 10.8'
*'set clip 1.75 3.00 3.65 5.0'

* points(fname,dx,dy,xmn,ymn)

* Draw a subdomain rectancle.
 'set line 1 1 5'
  x1 = 2.03
  y1 = 7.44
  x2 = 2.58
  y2 = 7.80

 'draw rec 'x1' 'y1' 'x2' 'y2

* Draw a length scale arrow.
 'set line 1 1 5'
  x1 = 4.7
  y1 = 4.4
  x2 = 6.05
  y2 = 4.4
  xm = (x1+x2)/2
 'draw line 'x1' 'y2' 'x2' 'y2 
 'set string 1 c 5'
 'set strsiz 0.16'
 'draw string 'x1+0.03' 'y2+0.013' <'
 'draw string 'x2-0.03' 'y2+0.013' >'
 'draw string 'xm' 'y2-0.2' 100 km'

 'abcd a 'br''

***********************************
***********************************

 'set dfile 3'

*'set x 1 375'
*'set y 1 275'
 'set x 1 375'
 'set y 30 265'

* nx=375, ny=235, ratio=1.60 or 0.63
*'set parea 2.0 6.5 0.5 3.3'
*'set parea 1.75 6.75 0.5 3.65'
 'set parea 1.75 6.75 0.2 3.35'

*'set xlab on'
*'set ylab on'
*'set xlab %gkm'
*'set ylab %gkm'
*'set xlint 2'
*'set ylint 2'
*'set xlopts 1 5 0.15'
*'set ylopts 1 5 0.15'
 'set xlab off'
 'set ylab off'

 'set gxout fgrid'
 'set fgvals 14 31 15 31 17 31 18 31 19 31 20 0 24 23 25 31 26 31'
 'd veg.3(t=1)'

 'set gxout fgrid'
 'set fgvals 1 4'
 'd gmsk.4(t=1)'

* points(fname,dx,dy,xmn,ymn)

* Draw a length scale arrow.
 'set line 1 1 5'
  x1 = 2.32
  y1 = 3.0
  x2 = 3.06
  y2 = 3.0
  xm = (x1+x2)/2
 'draw line 'x1' 'y2' 'x2' 'y2 
 'set string 1 c 5'
 'set strsiz 0.16'
 'draw string 'x1+0.03' 'y2+0.013' <'
 'draw string 'x2-0.03' 'y2+0.013' >'
 'draw string 'xm' 'y2-0.2' 6 km'

* Plot the snow depth observation site.
 'draw mark 2 4.98 1.82 0.35'

 'abcd b 'br''

 'gprint fig_files/domain'

************************************
************************************
************************************
************************************

  function points(fname,dx,dy,xmn,ymn)
while (1)
  ret = read(fname)
  rc = sublin(ret,1)
  if (rc>0) 
    if (rc!=2) 
      say 'File I/O Error'
      return
    endif
    break
  endif
  rec = sublin(ret,2)
  xstn = subwrd(rec,4)
  ystn = subwrd(rec,5)

* convert these 'meters' coords to be compatible with
*   the .ctl file (0.0 origin and km)
  xx=(xstn-xmn)/1000
  yy=(ystn-ymn)/1000

* say xx
* say yy

  'q ll2xy 'xx' 'yy
  x1=subwrd(result,1)
  y1=subwrd(result,2)

  'set lwid 20 1.0'
  'set line 1 1 20'

  'draw mark 3 'x1' 'y1' 0.05'
* 'draw mark 3 'x1' 'y1' 0.12'
endwhile

  rc=close(fname)

  return

************************************
************************************

