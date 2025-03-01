 'reinit'
*'set display color white'
*'c'

 'open ../../ctl_files/wo_assim/snod_km.ctl'
 'open ../../topo_vege/NoAm_30m/topo_vege_km.ctl'

 'set mproj scaled'
 'set mpdraw off'

* nx=205, ny=336, ratio=1.64, 0.61
 'set parea 1.25 7.05 0.75 10.25'

 'set xlopts 1 5 0.18'
 'set ylopts 1 5 0.18'

* This creates a small, spatially varying field that can
*   be added to snod so grads generates color shades when
*   the spatial distribution is constant (like when snod=0
*   everywhere).
 'define small=lon/10000000'

 'set dbuff on'

 t=1

 count = 1000000
 count=count+1

 while (t<=44560)
*while (t<=365)

   'set grads off'
   'set xlab %gkm'
   'set ylab %gkm'
   'set xlint 50'
   'set ylint 50'
   'set gxout grfill'

   'set t 't

   'set clevs 0 5 10 25 50 75 100 125 150 200 250'
   'set ccols 15 9 14 4 11 5 13 10 7 12 8 2'

* Because "small" is always a positive number (like between
*   0 and 10^-6), subtract 1 mm (or something) so the snow
*   depth goes to zero in the plot when all of the snow melts.
*  'd snod'
*  'd 100*snod+small-0.001'

* Here I have subtracted 5 cm of snow to get rid of the
*   flashy thin snow in the movie.  This is not realy the
*   right way to do this (I should just set anything less
*   than 5 cm to zero as a postprocessing step, or something).
   'd 100*snod+small-0.001-5.0'

   'cbarn 1.3 1 7.5 5.5'

   'set gxout fgrid'
*  'set fgvals 24 0'
   'set fgvals 19 1'
   'd vege.2(t=1)'

*  'set string 1 c 6'
*  'set strsiz 0.15'
*  'draw string 8.15 2.60 cm'

   'q time'
    ret=sublin(result,1)
    date=subwrd(ret,3)
    dy=substr(date,4,2)
    mo=substr(date,6,3)
    yr=substr(date,9,4)

   'draw title Snow Depth (cm)    'dy' 'mo' 'yr

* print 5 copies of each figure; I am just using this to slow
* down the movie.
*   loop = 1
*   while (loop<=5)
      cnt = substr(count,2,6)
      figname = 'figs_png/movie_'%cnt''
      say figname
     'gprint_png 'figname
*     loop = loop + 1
*     count = count + 1
*   endwhile

   'swap'
    t = t + 1
    count = count + 1
 endwhile

************************************

