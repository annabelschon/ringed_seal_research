 'reinit'
 'set display color white'
 'c'

 'open ../../4_daily_to_yearly/4_mk_aves_and_trends_zoomin/dom_ave_trends_4points.ctl'

* years of interest.
*   2003 2011 2014 2016

* everything is done here using inches on the page.

*****************************************

* FIGURE 1

  year = 2011

  'set time 00Z01JAN'year
  'd 100*snod_1ap'
  snod = subwrd(result,4)

  'd bsfx_s1a'
  area = subwrd(result,4)

  z1 = 10.1926
  z2 = 20.3851

* bottom of figure
  y0 = 5.3
  plot(snod,z1,z2,area,year,y0)

*****************************************

* FIGURE 2

  year = 2014

  'set time 00Z01JAN'year
  'd 100*snod_1ap'
  snod = subwrd(result,4)

  'd bsfx_s1a'
  area = subwrd(result,4)

  z1 = 4.3732
  z2 = 8.7464

* bottom of figure
  y0 = 3.1
  plot(snod,z1,z2,area,year,y0)

*****************************************

* FIGURE 3

  year = 2003

  'set time 00Z01JAN'year
  'd 100*snod_1ap'
  snod = subwrd(result,4)

  'd bsfx_s1a'
  area = subwrd(result,4)

  z1 = 1.7285
  z2 = 3.4571

* bottom of figure
* y0 = 1.6
  y0 = 1.65
  plot(snod,z1,z2,area,year,y0)

*****************************************

* FIGURE 4

  year = 2016

  'set time 00Z01JAN'year
  'd 100*snod_1ap'
  snod = subwrd(result,4)

  'd bsfx_s1a'
  area = subwrd(result,4)

  z1 = 0.6577
  z2 = 1.3155

* bottom of figure
  y0 = 0.4
  plot(snod,z1,z2,area,year,y0)

*****************************************
*****************************************

 'gprint fig_files/schematic'

*****************************************
*****************************************

 function plot(snod,z1,z2,area,year,y0)

* x1, x2 = x limits
  x1 = 1.0
  x2 = 10.0

* xcent = the center of the ice block
  xcent = 2.0

  scale = 0.25

  snod1 = scale*snod/100

* 2016 needs to be made a little deeper to show up on the plot.
  if (year = 2016)
    snod1=3*snod1
  endif

  z1 = scale*z1
  z2 = scale*z2

* xstart = left edge of ice block
  xstart = xcent - z1/2

* the snow depth
 'set rgb 21 161 202 241'
*'set line 4 1 1'
 'set line 21 1 1'
 'draw recf 'x1' 'y0' 'x2' 'snod1+y0

* ice block
 'set line 15 1 1'
  xl1 = x1 + xstart
  xr1 = xl1 + z1
  yt1 = y0 + z1
 'draw recf 'xl1' 'y0' 'xr1' 'yt1

* snow wedge
*'set line 4 1 1'
 'set line 21 1 1'
  xl=xr1
  xr2=xl+z2
  yt=y0+z1
  if (year != 2016)
   'draw polyf 'xl' 'y0' 'xl' 'yt' 'xr2' 'y0' 'xl' 'y0
  endif

* the sea-ice-surface line
 'set line 15 1 1'
 'draw recf 'x1' 'y0-0.2' 'x2' 'y0

* numbers
* snod = math_format('%5.1f',snod)
* area = math_format('%5.1f',area)
  snod = math_format('%.1f',snod)
  area = math_format('%.1f',area)
  'set string 1 bl 6 0'
  'set strsiz 0.2'
  'draw string 4.9 'yt+0.25' snow depth = 'snod' cm'
* 'draw string 4.9 'yt-0.1'  lair density = 'area' lairs ha`a-1`n'
  'draw string 4.9 'yt-0.1'  lair density = 'area' lairs hm`a-2`n'

* year
  'set string 1 c 6 0'
  xc = (xl1+xr1)/2
  'draw string 'xc' 'yt1+0.25' 'year

* plot a 50 cm line for reference
  den_diam = scale*0.5

* this draws a cross 50 cm across, that I can use to scale the
*   mark size
*'set line 1 1 1'
*'draw line 'x2-0.5' 'y0' 'x2-0.5' 'den_diam+y0
*'draw line 'x2-den_diam/2-0.5' 'y0+den_diam/2' 'x2+den_diam/2-0.5' 'y0+den_diam/2

 'set line 1 1 3'
  xcent = x1+0.35
  ycent = den_diam/2+y0
 'draw mark 2 'xcent' 'ycent' '0.15

* xcent = x2-0.35
*'draw mark 2 'xcent' 'ycent' '0.15

  xcent = xr1+0.12
 'draw mark 2 'xcent' 'ycent' '0.15

 if (year=2003)
   xcent = xr1+0.18
   ycent = den_diam/2+y0+0.18
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.48
   ycent = den_diam/2+y0+0.04
  'draw mark 2 'xcent' 'ycent' '0.15
 endif

 if (year=2014)
   xcent = xr1+0.18
   ycent = den_diam/2+y0+0.18
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.48
   ycent = den_diam/2+y0+0.04
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+1.48
   ycent = den_diam/2+y0+0.14
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.9
   ycent = den_diam/2+y0+0.34
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.6
   ycent = den_diam/2+y0+0.6
  'draw mark 2 'xcent' 'ycent' '0.15
 endif

 if (year=2011)
   xcent = xr1+0.18
   ycent = den_diam/2+y0+0.18
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.48
   ycent = den_diam/2+y0+0.04
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+1.48
   ycent = den_diam/2+y0+0.14
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.9
   ycent = den_diam/2+y0+0.34
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.6
   ycent = den_diam/2+y0+0.6
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+2.8
   ycent = den_diam/2+y0+0.19
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+2.48
   ycent = den_diam/2+y0+1.14
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.5
   ycent = den_diam/2+y0+1.34
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+2.6
   ycent = den_diam/2+y0+0.9
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+4.25
   ycent = den_diam/2+y0+0.05
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+2.9
   ycent = den_diam/2+y0+0.4
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.2
   ycent = den_diam/2+y0+1.4
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.3
   ycent = den_diam/2+y0+1.9
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.5
   ycent = den_diam/2+y0+1.8
  'draw mark 2 'xcent' 'ycent' '0.15

   xcent = xr1+0.6
   ycent = den_diam/2+y0+2.05
  'draw mark 2 'xcent' 'ycent' '0.15
 endif

  return

*****************************************
*****************************************

**************************************************
**************************************************

 function ellipse(x0,y0,xscale,yscale,thick)

* my goal here is to draw an ellipse that corresponds to the
* aspect ratio of the figure.

 aa=xscale
 bb=yscale

 a = 1
 deg = 0
 DELTA = 5
 D2R   = 3.141592654 / 180.0

 while (deg <= 360)
  radius.a = aa*bb/math_sqrt(bb*math_cos(deg*D2R)*bb*math_cos(deg*D2R)+aa*math_sin(deg*D2R)*aa*math_sin(deg*D2R))
  x.a = x0 + radius.a * math_cos(deg*D2R)
  y.a = y0 + radius.a * math_sin(deg*D2R)

* say radius.a
* say deg' 'a' 'x.a' 'y.a

  deg = deg + DELTA
  a = a + 1
 endwhile
 count = a - 1

'set line 4 1 'thick
 a = 1
 while (a < count)
   b = a + 1
   'draw line 'x.a' 'y.a' 'x.b' 'y.b
   a = a + 1
 endwhile

 return

**************************************************
**************************************************

