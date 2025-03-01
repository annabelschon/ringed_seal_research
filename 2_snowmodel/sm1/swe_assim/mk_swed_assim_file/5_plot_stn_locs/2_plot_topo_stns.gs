 'reinit'
 'open ../../../topo_vege/NoAm_30m/topo_vege.ctl'

 'set mpdraw off'
 'set mproj scaled'
 'set grads off'

* define the SnowModel domain
  fname=SM_info.dat

  ret = read(fname)
  rec = sublin(ret,2)
  nx = subwrd(rec,1)

  ret = read(fname)
  rec = sublin(ret,2)
  ny = subwrd(rec,1)

  say nx
  say ny

  set_parea(nx,ny)

 'set xlab off'
 'set ylab off'

 'set gxout grfill'
 'd topo'

 'set gxout fgrid'
 'set fgvals 24 15'
 'd vege'

 'set lwid 21 1.0'
 'set lwid 22 2.0'
 'set lwid 25 5.0'

 fname = plot_info.dat

 return = read(fname)

 return = read(fname)
 record = sublin(return,2)
 nstns = subwrd(record,1)

 return = read(fname)
 return = read(fname)

 return = read(fname)
 record = sublin(return,2)
 nsnotel = subwrd(record,1)

 say 'nstns = 'nstns
 say 'nsnotel = 'nsnotel

  fname = ij_coords.dat

  xx = 1
  while (xx <= nstns)

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
    n = subwrd(rec,1)
    i = subwrd(rec,2)
    j = subwrd(rec,3)
    id = subwrd(rec,4)
*   say n' 'i' 'j' 'id

*   pull dummy

    if (i != -9999)
     'q gr2xy 'i' 'j
      x = subwrd(result,3)
      y = subwrd(result,6)

*     say n' 'x' 'y

     'set line 0 1 25'

      if (xx <= nsnotel)
       'draw mark 6 'x' 'y' 0.4'
      else
       'draw mark 4 'x' 'y' 0.4'
      endif

     'set strsiz 0.2'
     'set string 1 r 6'
     'draw string 'x-0.3' 'y' 'n
     'set string 1 l 6'
     'draw string 'x+0.3' 'y' 'id
    endif
    say n' 'i' 'j' 'id
    xx = xx + 1
  endwhile

*'gprint_png fig_files/station_map'

************************************
************************************
************************************
************************************

     function set_parea(nx,ny)

* Adjust parea to make the aspect ratio correct.
*   This should work regardles of whether you
*   are plotting in landscape or portrait.
*    'set mproj scaled'
*     set_parea(nx,ny)

     min_boundary = 0.5
    'query gxinfo'
     rec2 = sublin(result,2)
     gxlolim = 0.0 + min_boundary
     gxhilim = subwrd(rec2,4) - min_boundary
     gylolim = 0.0 + min_boundary
     gyhilim = subwrd(rec2,6) - min_boundary

     gxmax = gxhilim - gxlolim
     gymax = gyhilim - gylolim
     gxcent = (gxhilim + gxlolim) / 2
     gycent = (gyhilim + gylolim) / 2

     gxlolim = gxcent - gxmax / 2
     gxhilim = gxcent + gxmax / 2
     gylolim = gycent - gymax / 2
     gyhilim = gycent + gymax / 2

     eps = 0.0001
     ratio = nx / ny + eps

     if (ratio>1)
* x is the biggest.
       xlo = gxlolim
       xhi = gxhilim
       ylo = gycent - gxmax / ratio / 2
       yhi = gycent + gxmax / ratio / 2
       if (ylo<gylolim)
         xlo = gxcent - gymax * ratio / 2
         xhi = gxcent + gymax * ratio / 2
         ylo = gylolim
         yhi = gyhilim
       endif
     else
* y is the biggest.
       xlo = gxcent - gymax * ratio / 2
       xhi = gxcent + gymax * ratio / 2
       ylo = gylolim
       yhi = gyhilim
     endif

    'set parea 'xlo' 'xhi' 'ylo' 'yhi

     return

************************************
************************************

