 'reinit'
 'set display color white'
 'c'

 'open snod_obs_mod.ctl'

 'set parea 1.0 8.0 2.0 9.0'

* Build an array of values to be plotted.

 'set t 1 400'

     'set vrange 0.0 1.8'
     'set vrange2 0.0 1.8'

     'set x 1'
     'set y 1'
     'set gxout scatter'

     'set xlopts 1 5 0.16'
     'set ylopts 1 5 0.16'

     'set grads off'

*    'set clip 1.0 8.0 2.0 9.0'

*    'define obs=100.0*snod_obs.1'
*    'define mod=100.0*snod_mod.1'

*    'define obs=100*snod_obs.1'
*    'define mod=100*snod_mod.1'

     'define obs=snod_obs.1'
     'define mod=snod_mod.1'

     'set digsiz 0.2'

     'd obs;mod'

     'set clip 0.0 8.5 0.0 11.0'

* plot the axis labels.
     'set digsiz 0'
     'd obs;mod'

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

  'set string 1 c 4 90'
  'set strsiz 0.22'
   x1 = xmn-0.7
   y1 = (ymn+ymx)/2
  'draw string 'x1' 'y1' SnowModel Snow Depth (m)'

  'set string 1 c 4 0'
  'set strsiz 0.22'
   x1 = (xmn+xmx)/2
   y1 = ymn-0.5
  'draw string 'x1' 'y1' Observed Snow Depth (m)'

*************************************

* calculate r^2: obs comes first in the r_squared call, then mod
 'set string 1 l 4 0'
 'set strsiz 0.22'
  x1 = xmn+0.2
  y1 = ymx-0.7
  r_squared('obs','mod',x1,y1)

*************************************

*'draw title GMU 15A\(DOS = Day Of Simulation = day since 1 Sep)\'

 'gprint_png fig_files/scatter_snod_obs_mod'

****************************************************
****************************************************
****************************************************

 function r_squared(obs,mod,x,y)

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

 'set strsiz 0.22'
 'draw string 'x' 'y+0.45' n='npts
 'draw string 'x' 'y' r`a2`n='r2

  return

************************************
************************************


