 'reinit'
 'set display color white'
 'c'

 'open ../3_gdal_mk_polygon_mask/mask_meters.ctl'

  left = 0.25
  right = 0.25
  top = 0.25
  bottom = 0.25
 'set_parea_pl 'left' 'right' 'top' 'bottom

 'set mpdraw off'
 'set grads off'

 'set xlab off'
 'set ylab off'

 'set gxout grfill'

 'd mask'

pp = 1
  while ( pp <= 18 )

    say pp

    fname = "pdata/poly"pp".dat"
    say fname

    polygon(fname)

    pp = pp + 1

  endwhile

 'gxprint LME_masked.png png'
*'gprint_png fig_files/mask_poly'

************************************
************************************
************************************
************************************

  function polygon(fname)

  ret = read(fname)
  rec = sublin(ret,2)
  x1 = subwrd(rec,1)
  y1 = subwrd(rec,2)

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
* say rec

  x2 = subwrd(rec,1)
  y2 = subwrd(rec,2)

* say x2' 'y2

  'q w2xy 'x1' 'y1
  xx1=subwrd(result,3)
  yy1=subwrd(result,6)

  'q w2xy 'x2' 'y2
  xx2=subwrd(result,3)
  yy2=subwrd(result,6)

  'set line 1 1 4'

  'draw line 'xx1' 'yy1' 'xx2' 'yy2

  x1 = x2
  y1 = y2

endwhile

  rc=close(fname)

  return

************************************
************************************

