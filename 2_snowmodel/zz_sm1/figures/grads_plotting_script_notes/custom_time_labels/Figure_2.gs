 'reinit'
 'set display color white'
 'c'

 'open ../6_postprocess/swed_obs_mod_stn.ctl'

  ncols = 5
  nrows = 4

* Call the panel function to create the panel parea dimensions.
  mk_panels(ncols,nrows)

  panel = 1
  panel_total = ncols * nrows

* Loop through each panel and draw a plot.

* while (panel <= panel_total)
*    _parea.panel
*   'set t 'panel
*   'd tair'
*    panel = panel + 1
* endwhile

*yr = 1980
 yr = 2001
 while (yr <= 2020)

    say yr
    year = yr - 1980 + 1
    say year

*   panel = yr - 1979
    panel = yr - 2000
    _parea.panel
    say panel

   'set datawarn off'

 'set grads off'
 'set mproj scaled'
 'set mpdraw off'
 'set gxout shaded'
 'set xlab off'
 'set ylab off'

   'set lwid 22 2.0'

************************

   'set vrange -0.2 15'
*  'set ylint 3'

   'set t 1 107'

* this is the snotel station of interest.
   'set x 'year
   'set y 1'

   'set cmark 0'
   'set cstyle 1'
   'set cthick 22'
   'set ccolor 3'
   'd 100*obs.1'

   'set cmark 0'
   'set cstyle 1'
   'set cthick 22'
*  'set ccolor 2'
   'set ccolor 9'
   'd 100*mod.1'

* Draw the year label.
   'q gxinfo'
    rec = sublin(result,3)
    xmn = subwrd(rec,4)
    xmx = subwrd(rec,6)
    rec = sublin(result,4)
    ymn = subwrd(rec,4)
    ymx = subwrd(rec,6)

    xcent=(xmn+xmx)/2
   'set string 1 c 6 0'
   'set strsiz 0.15'
   'draw string 'xcent' 'ymx-0.2' 'yr

    yr = yr + 1

 endwhile

   'set string 1 c 6 90'
   'set strsiz 0.18'
    yy = (11.0 - _bottom)/2 + _bottom
*  'draw string 0.14 8.5 snow depth range, 0-50 cm'
   'draw string 0.14 'yy' Snow depth range, 0-50 cm'

   'set string 1 c 6 0'
   'set strsiz 0.20'
    yy = _bottom - 0.15
*  'draw string 4.33 5.8 date span, 1 Sep - 16 Dec'
   'draw string 4.33 'yy' Date span, 1 September - 16 December'

   'abcd A 'br''

* LOWER LEFT PANEL.
* LOWER LEFT PANEL.
* LOWER LEFT PANEL.

* SNOTEL
 'open ../7_figures/yearly_variables_startyear.ctl'

* SnowModel
 'open ../6_postprocess/sonset_sm_startyear.ctl'

 'set parea 1.2 4.2 1.0 4.0'

 'set xlab on'
 'set ylab on'

 'set dfile 2'

* Build an array of values to be plotted.

 'set t 1 41'

 xmin = 245
 xmax = 325

 ymin = xmin
 ymax = xmax

     'set vrange 'xmin' 'xmax
     'set vrange2 'ymin' 'ymax

*    'set xlint 15'
*    'set ylint 15'

     'set x 1'
     'set y 1'
     'set gxout scatter'

     'set xlopts 1 6 0.16'
     'set ylopts 1 6 0.16'

* DOY.
     'define obs=243+snow_onset_dos.2'
     'define mod=243+snow_onset_dos.3'

     'set digsiz 0.2'

     'set xlab off'
     'set ylab off'

     'd obs;mod'
     'frame'

*************************************

* draw the 1:1 line
*   note that this requires the x and y axis ranges
*   to be the same
 'q gxinfo'
  rec = sublin(result,3)
  xmn = subwrd(rec,4)
  xmx = subwrd(rec,6)
  rec = sublin(result,4)
  ymn = subwrd(rec,4)
  ymx = subwrd(rec,6)
 'draw line 'xmn' 'ymn' 'xmx' 'ymx

*************************************

   timelabsx(xmin,xmax)
   timelabsy(ymin,ymax)

*************************************

  'set string 1 c 6 90'
  'set strsiz 0.18'
   x1 = xmn-1.05
   x2 = x1+0.3
   y1 = (ymn+ymx)/2
* 'draw string 'x1' 'y1' SnowModel Snow Onset Date (DOY)'
  'draw string 'x1' 'y1' SnowModel Snow Onset Date'
* 'draw string 'x2' 'y1' Date (DOY)'

  'set string 1 c 6 0'
  'set strsiz 0.18'
   x1 = (xmn+xmx)/2
   y1 = ymn-0.5
   y2 = y1-0.3
* 'draw string 'x1' 'y1' Observed Snow Onset Date (DOY)'
  'draw string 'x1' 'y2' Observed Snow Onset Date'
* 'draw string 'x1' 'y2' Date (DOY)'

*************************************

* calculate r^2: obs comes first in the r_squared call, then mod
 'set string 1 l 6 0'
 'set strsiz 0.18'
  x1 = xmn+0.2
  y1 = ymx-0.55
  flag = 2
  r_squared('obs','mod',x1,y1,flag)

*************************************

   'abcd B 'br''

*************************************
*************************************
*************************************

* LOWER RIGHT PANEL.
* LOWER RIGHT PANEL.
* LOWER RIGHT PANEL.

* SNOTEL
*'open ../7_figures/yearly_variables_startyear.ctl'

* SnowModel
*'open ../6_postprocess/sonset_sm_startyear.ctl'

 'open ../6_postprocess/swed_obs_mod_stn_ave.ctl'

*'set parea 5.3 8.3 1.0 4.0'
 'set parea 5.35 8.35 1.0 4.0'

 'set xlab on'
 'set ylab on'

 'set dfile 4'

* Build an array of values to be plotted.

     'set time 01SEP1980 16DEC1980'

     'set vrange 0 20'
     'set vrange2 0 20'
     'set xlint 4'
     'set ylint 4'

     'set x 1'
     'set y 1'
     'set gxout scatter'

     'set xlopts 1 6 0.16'
     'set ylopts 1 6 0.16'

     'set grads off'

* swed.
* use a constant density to convert to depth.
     'define obs=3*100*obs.4'
     'define mod=3*100*mod.4'

     'set ccolor 1'
     'set cmark 2'
*    'set digsiz 0.2'
     'set digsiz 0.05'

     'd obs;mod'
     'frame'

*************************************

* draw the 1:1 line
*   note that this requires the x and y axis ranges
*   to be the same
 'q gxinfo'
  rec = sublin(result,3)
  xmn = subwrd(rec,4)
  xmx = subwrd(rec,6)
  rec = sublin(result,4)
  ymn = subwrd(rec,4)
  ymx = subwrd(rec,6)
 'draw line 'xmn' 'ymn' 'xmx' 'ymx

*************************************

  'set string 1 c 6 90'
  'set strsiz 0.18'
*  x1 = xmn-1.05
   x1 = xmn-0.85
   x2 = x1+0.3
   y1 = (ymn+ymx)/2
* 'draw string 'x1' 'y1' SnowModel Snow Onset Date (DOY)'
  'draw string 'x1' 'y1' SnowModel Snow'
  'draw string 'x2' 'y1' Depth (cm)'

  'set string 1 c 6 0'
  'set strsiz 0.18'
   x1 = (xmn+xmx)/2
   y1 = ymn-0.5
   y2 = y1-0.3
* 'draw string 'x1' 'y1' Observed Snow Onset Date (DOY)'
  'draw string 'x1' 'y1' Observed Snow'
  'draw string 'x1' 'y2' Depth (cm)'

*************************************

* calculate r^2: obs comes first in the r_squared call, then mod
 'set string 1 l 6 0'
 'set strsiz 0.18'
  x1 = xmn+0.2
  y1 = ymx-0.55
  flag = 1
  r_squared('obs','mod',x1,y1,flag)

*************************************

   'abcd C 'br''

*************************************
*************************************
*************************************

* Print the figure.
  'gprint fig_files/Figure_2'

*******************************************************************
*******************************************************************

  function mk_panels(ncols,nrows)

* Define the width of the blank areas between the panels.
  space = 0.1

* Define any extra bottom, top, and left-side space.
* bottom = 6.0
* _bottom = 4.3
* _bottom = 4.7
  _bottom = 5.0
  _top = 0.0
  _left = 0.25

* Get the real page dimensions.
  'query gxinfo'
  rec2  = sublin(result,2)
  xsize = subwrd(rec2,4)
  _ysize = subwrd(rec2,6)

* These need to be adjusted further if you want the aspect ratio
*   to be correct.
  xdist = xsize - (ncols + 1) * space - _left
  ydist = _ysize - (nrows + 1) * space - _bottom - _top

  _xcenter = (xsize - _left) / 2 + _left

* Calculate plotting coordinates of each panel.
  width  = xdist / ncols
  height = ydist / nrows
  row = 1
  col = 1
  panel = 1

* row = 1 is on the top, row = nrows is on the bottom.
  while (row <= nrows)
    yhi = _ysize - space - ((height + space) * (row - 1)) - _top
    if (row = nrows)
      ylo = space + _bottom
    else
      ylo = yhi - height
    endif

    _ylab.row = (ylo + yhi) / 2

    while (col <= ncols)  
      xlo = space + (width + space) * (col - 1) + _left
      xhi = xlo + width

      _parea.panel = 'set parea 'xlo'  'xhi'  'ylo'  'yhi
      _xlab.col = (xlo + xhi) / 2

      panel = panel + 1
      col = col + 1
    endwhile
    col = 1
    row = row + 1
  endwhile

  return

*******************************************************************
*******************************************************************
****************************************************
****************************************************
****************************************************

 function r_squared(obs,mod,x,y,flag)

* NOTE: this works for time-series data or 2D arrays.
*   it also works if there are missing data in either
*   the obs or mod array, or both.

* 'set gxout scatter'
* 'd obs;mod'
* 'set string 1 l 3.5'
*  x1 = 4.7
*  y1 = 1.3
*  r_squared('obs','mod',x1,y1)

* obs == x, mod == y

 'set gxout stat'

* check to see if there are any missing data.  this is not
*   really used in any way.

 'd 'obs
  rec = sublin(result,7)
  n_undef_obs = subwrd(rec,4)

 'd 'mod
  rec = sublin(result,7)
  n_undef_mod = subwrd(rec,4)

  say
  say 'n_undef_obs = 'n_undef_obs
  say 'n_undef_mod = 'n_undef_mod
  say

* make sure the undef values are not included in the calculations.
*   the normal calculations don't include undef values, and the
*   maskout function makes sure the undef values in the other
*   variable are not included either.

*'d 'obs
 'd maskout('obs','mod')'
  rec = sublin(result,10)
  sumx = subwrd(rec,2)

*'d 'mod
 'd maskout('mod','obs')'
  rec = sublin(result,10)
  sumy = subwrd(rec,2)

*'d 'obs'*'obs
 'd maskout('obs'*'obs','mod')'
  rec = sublin(result,10)
  sumxx = subwrd(rec,2)

*'d 'mod'*'mod
 'd maskout('mod'*'mod','obs')'
  rec = sublin(result,10)
  sumyy = subwrd(rec,2)

 'd 'obs'*'mod
  rec = sublin(result,10)
  sumxy = subwrd(rec,2)

  rec = sublin(result,7)
  npts = subwrd(rec,8)

  say 'sumx  = 'sumx
  say 'sumy  = 'sumy
  say 'sumxy = 'sumxy
  say 'sumxx = 'sumxx
  say 'sumyy = 'sumyy
  say 'npts  = 'npts
  say

  top = npts*sumxy - sumx*sumy
  bot = (npts*sumxx - sumx*sumx) * (npts*sumyy - sumy*sumy)

  r2 = top*top/bot

  say 'r^2  = ' r2

  fmt = '%-6.2f'
  r2 = math_format(fmt,r2)
  say fmt' of 'r2' = 'r2
  say

*'set strsiz 0.18'
  if (flag = 1)
    'draw string 'x' 'y+0.35' n='npts' days'
  endif

  if (flag = 2)
    'draw string 'x' 'y+0.35' n='npts' years'
  endif

*'draw string 'x' 'y' r`a2`n='r2

  return

************************************
************************************

*************************************************
*************************************************

  function timelabsx(xmin,xmax)

* Open file and do the plot, making sure
*   to plot no labels on the x axis

* Define the values that are to be plotted
nlabels = 5

xlab.1 = '16Sep'
xlab.2 = '1Oct'
xlab.3 = '16Oct'
xlab.4 = '1Nov'
xlab.5 = '16Nov'

xval.1 = '260'
xval.2 = '275'
xval.3 = '290'
xval.4 = '306'
xval.5 = '321'

* Get the plot-size info
'q gxinfo'
rec = sublin(result,3)
xmn = subwrd(rec,4)
xmx = subwrd(rec,6)
rec = sublin(result,4)
ymn = subwrd(rec,4)
ymx = subwrd(rec,6)
 
 xdiff = (xmx - xmn) / (xmax - xmin)

 i = 1
*'set string 1 tc 6'
 'set string 1 tc 6 45'
 'set strsiz 0.18'

while ( i<=nlabels )

* This worked when I only plotted this figure, but didn't
*   in this 3-panel plot (I got a segmentation fault that
*   I couldn't track down)
* 'q gr2xy '%(xval.i)%' '0
* 'q gr2xy '%(xval.i)%' 'xmin
*  x = subwrd(result,3)
*  y = ymn

* So I did this instead; also see the xdiff calulation above
   x = xmn + (xval.i - xmin) * xdiff
   y = ymn

* Draw the date labels
* 'draw string 'x' 'y-0.10' 'xlab.i
  'draw string 'x-0.4' 'y-0.30' 'xlab.i

* Draw the grid tick marks
  'set line 1 1 6'
  'draw line 'x' 'y' 'x' '%(y-0.06)

* Draw the grid lines
  'set line 15 5 4'
  'draw line 'x' 'ymn' 'x' 'ymx
  i = i + 1
endwhile

return

*************************************************
*************************************************

  function timelabsy(ymin,ymax)

* Open file and do the plot, making sure
*   to plot no labels on the x axis

* Define the values that are to be plotted
nlabels = 5

ylab.1 = '16Sep'
ylab.2 = '1Oct'
ylab.3 = '16Oct'
ylab.4 = '1Nov'
ylab.5 = '16Nov'

yval.1 = '260'
yval.2 = '275'
yval.3 = '290'
yval.4 = '306'
yval.5 = '321'

* Get the plot-size info
'q gxinfo'
rec = sublin(result,3)
xmn = subwrd(rec,4)
xmx = subwrd(rec,6)
rec = sublin(result,4)
ymn = subwrd(rec,4)
ymx = subwrd(rec,6)

 ydiff = (ymx - ymn) / (ymax - ymin)

 i = 1
*'set string 1 tc 6 90'
 'set string 1 tc 6 45'
 'set strsiz 0.18'

while ( i<=nlabels )
* See my notes on this in "function timelabsy"
* 'q gr2xy '0' '%(yval.i)
*  x = xmn
*  y = subwrd(result,6)

   x = xmn
   y = ymn + (yval.i - ymin) * ydiff

* Draw the date labels
* 'draw string 'x-0.30' 'y' 'ylab.i
  'draw string 'x-0.50' 'y-0.1' 'ylab.i

* Draw the grid tick marks
  'set line 1 1 6'
  'draw line '%(x-0.06)' 'y' 'x' 'y

* Draw the grid lines
  'set line 15 5 4'
  'draw line 'xmn' 'y' 'xmx' 'y
  i = i + 1
endwhile

return

*************************************************
*************************************************

