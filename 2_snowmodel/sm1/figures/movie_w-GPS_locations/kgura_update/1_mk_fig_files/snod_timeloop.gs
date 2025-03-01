 'reinit'

* These are (i,j) .ctl files.
 'open ../../ctl_files/wo_assim/snod.ctl'
 'open ../../topo_vege/NoAm_30m/topo_vege.ctl'
 
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
 count = 100000

 count = count + 1

*If daily time step and one year:
 while ( tt <= 365 )
*If 3hourly time step and one year 
*while ( tt <= 2928)

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
* Or, can specify the levels and colors above.

* For testing, begin with a later time step:
*  if (tt >= 250)

* Because "small" is always a positive number (like between
*   0 and 10^-6), subtract 1 mm (or something) so the snow
*   depth goes to zero in the plot when all of the snow melts.
*  'd snod'
*  'd 100*snod+small-0.001'

* Here I have subtracted 5 cm of snow to get rid of the
*   flashy thin snow in the movie.
   'd 100*snod+small-0.001-5.0'
*End test: 
*  endif

* In this order: scale factor; 0=horizontal, 1=vertical; x; y.
   'cbarn 1.2 0 5.5 0.7'

* Display topography.
   'set gxout contour'
   'set clab off'
   'set cint 300'
   'set cthick 1'
   'set ccolor 42'
   'd topo.2(t=1)'

* This is putting a "cm" label on the plots (like next to the
*   color bar).  I am also putting it in the title below.
*  'set string 1 c 6'
*  'set strsiz 0.15'
*  'draw string 8.15 2.60 cm'

   'q time'
    ret=sublin(result,1)
    date=subwrd(ret,3)
*   hr=substr(date,1,2)
    dy=substr(date,4,2)
    mo=substr(date,6,3)
    yr=substr(date,9,4)

   'set string 1 c 10 0'
   'set strsiz .18'
   'draw string 5.5 7.8 SnowModel Simulations    'dy' 'mo' 'yr 

*Other options for labeling:
*  'draw title Snow Depth (cm)    'dy' 'mo' 'yr
*  'draw title Snow Depth (cm)    'hr':00 'dy' 'mo' 'yr

   'set string 1 bc 10 0'
   'set strsiz .15'
   'draw string 5.5 0.15 Snow Depth (cm)'


*    'abcd  'br''

    scalebar ()

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

************************************
************************************
  function scalebar ()

* This is how many km you want in your scale bar.

  scale = 20

* dxpos is the x distance from the lower-right corner, where you
*   want the right end of the scale bar to be.'
* dypos is the y distance from the lower-right corner, where you
*   want the center of the scale bar to be.'
  dxpos = -0.5
  dypos = 0.5
      
* dx is the x-length of the arrow barbs.
  dx = 0.35
          
* Get the info required to create the scale bar.
  
*fname should be path to your ...OUTPUTS.dat file from topo
*  processing:

  fname='../../../topo_vege/NoAm_30m/SM_domain_config_info_OUTPUTS.dat'
  ret = read(fname)
  rec = sublin(ret,2)
  nx = subwrd(rec,3)
                 
  ret = read(fname)
  ret = read(fname)
  rec = sublin(ret,2)
  deltax = subwrd(rec,3)
                      
* say nx
* say deltax
                         
* If you are going to read this file again, you have to close it.
*   Alternatively, you could get this info before calling the time
*   loop and pass it in.  That way you only have to read this
*   information once.
  cf = close(fname)

* Get the plot size in inches on the page.  This could also be
*   done once, outside any time loop.  You would have to make the
*   variables global, like this: _xlo, _xhi, etc.  i
  'query gxinfo'
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xlo  = subwrd(rec3,4) 
  xhi =  subwrd(rec3,6)
  ylo =  subwrd(rec4,4)
* yhi =  subwrd(rec4,6)
            
* say xlo
* say xhi
* say ylo
* say yhi
                 
* Calculate the scale bar length.
  xdist_km = nx * deltax / 1000
  barlength = (scale / xdist_km) * (xhi - xlo)
                     
* Calculate the scale line and arrow coordinates.  This is the
*   naming convention.
*     o       o       y1
*  o             o    y2
*     o       o       y3
*  x1 x2      x3 x4
                           
*     o         o
*       dy            dy
*  o                o
*       dy            dy
*     o         o
*   dx           dx

  dy = dx / 2
    
  x4 = xhi + dxpos
  y2 = ylo + dypos
    
  x1 = x4 - barlength
    
  x2 = x1 + dx
  x3 = x4 - dx
  y1 = y2 + dy
  y3 = y2 - dy
   
* Draw the scale line and end arrows.
  'set line 0 1 12'
  'draw line 'x1' 'y2' 'x4' 'y2
   
*  'draw line 'x1' 'y2' 'x2' 'y1
*  'draw line 'x1' 'y2' 'x2' 'y3
*  'draw line 'x4' 'y2' 'x3' 'y1
*  'draw line 'x4' 'y2' 'x3' 'y3
   
* Draw the km label.
* 'set strsiz 0.20'
  'set strsiz 0.20'
* 'set string 0 c 6'
  'set string 0 c 12'
  xc = (x4 - x1)/2 + x1
  yc = y2 - 0.2
  'draw string 'xc' 'yc' 'scale' km'
   
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

