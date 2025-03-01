 'reinit'
*'set display color white'
*'c'

* These are (i,j) .ctl files.
*'open ../../ctl_files/wo_assim/snod.ctl'
*'open ../../topo_vege/NoAm_30m/topo_vege.ctl'

 'open /data1/working/robins/sm_3/outputs/snod.ctl'
 'open /data1/working/robins/topo_veg/9_final/topo_veg_robins_5km_ij.ctl'

 'set mproj scaled'
 'set mpdraw off'

* There are ways to automatially find and set these parea
*   values.
* nx=660, ny=400, ratio=1.65, ratio=0.61.
  'set parea 0.5 10.5 1.25 7.35'

* This creates a small, spatially varying field that can
*   be added to snod so grads generates color shades when
*   the spatial distribution is constant (like when snod=0
*   everywhere).
 'define small=lon/10000000'

* This is using GrADS ability to make a figure and then plot it
*   on the screen in its final form (as opposed to watching it
*   draw on the screen).
 'set dbuff on'

 tt = 1

 count = 1000000
 count = count + 1

 while ( tt <= 365 )

   'set grads off'

   'set xlab off'
   'set ylab off'

* If you want m or km axis labels, you will have to use different
*   .ctl files.
*  'set xlopts 1 6 0.18'
*  'set ylopts 1 6 0.18'
*  'set xlab %gkm'
*  'set ylab %gkm'
*  'set xlint 50'
*  'set ylint 50'

   'set gxout grfill'

   'set t 'tt

* This is what makes all the figures have the same color range.
   'set clevs 0 5 10 25 50 75 100 125 150 200 250'
   'set ccols 15 9 14 4 11 5 13 10 7 12 8 2'

* Because "small" is always a positive number (like between
*   0 and 10^-6), subtract 1 mm (or something) so the snow
*   depth goes to zero in the plot when all of the snow melts.
*  'd snod'
*  'd 100*snod+small-0.001'

* Here I have subtracted 5 cm of snow to get rid of the
*   flashy thin snow in the movie.
   'd 100*snod+small-0.001-5.0'

* In this order: scale factor; 0=horizontal, 1=vertical; x; y.
   'cbarn 1.3 0 5.5 0.7'

* This is setting things like glaciers and lakes and oceans to a
*   constant color.
   'set gxout fgrid'
*  'set fgvals 24 0'
   'set fgvals 19 1'
   'd vege.2(t=1)'

* This is putting a "cm" label on the plots (like next to the
*   color bar).  I am also putting it in the title below.
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

* Identify the GPS file and plot the GPS points over the snow data.
    fname = '../1_mk_gps_data/3_mk_daily_gps_files/data/gps_'%count'.dat'
    say fname
    points(fname)

* Create a file name and print the figure to the file.  Note that
*   what this is doing is getting rid of the leading "1", so you
*   have 000001 instead of 1000001.  This is important because
*   when you go to make the movie, it is looking for file names
*   that start at 1 and increment from there.
    cnt = substr(count,2,6)
    figname = 'fig_files/movie_'%cnt''
    say figname
   'gprint_png 'figname

* Update the displayed figure.
   'swap'

* You can use "wait" to slow down how fast this is plotted on
*   the screen.  This function just kills a little time.
*   wait(30)

    tt = tt + 1
    count = count + 1
 endwhile

************************************
************************************

  function points(fname)

 'set line 1 1 6'

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
    xx = subwrd(rec,1)
    yy = subwrd(rec,2)
*   say xx' 'yy

* Convert the grid (i,j) coordinates to (x,y) on the page.
   'q gr2xy 'xx' 'yy
    x1=subwrd(result,3)
    y1=subwrd(result,6)

*  'set line 1'
   'set line 0'
   'draw mark 2 'x1' 'y1' 0.25'
  endwhile

  rc=close(fname)

  return

************************************
************************************

* The number in wait(#) defines the number of times the nc
*   loop is run.  For example, if you set nc to a number that
*   corresponds to 0.1 sec on your machine, then wait(30) will
*   give you a 3 sec pause.

* wait(30)

  function wait(n)

  tt=0
  nc=100
  while (tt<n)
    t=0
    while (t<nc)
      t=t+1
      xx=3.14*tt
    endwhile
    tt= tt+1
  endwhile

  return

************************************
************************************

