 'reinit'

 'open /u2/liston/climate/csmp31/sfcday/zsfcday.89018912.g2.ctl'

 'set grads off'
 'set gxout shaded'

 'set y 10'
 'set t 1 365'

 'set ylab off'

 'd sctemp-273'

   timelaby()

*************************************************
*************************************************

  function timelaby()

* Open file and do the plot, making sure
*   to plot no labels on the y axis

* Get the y-axis grid (time) limits

'q dims'
 rec = sublin(result,5)
 gtmin = subwrd(rec,11)
 gtmax = subwrd(rec,13)

* Define the times that are to be plotted

nlabels = 12

ylab.1 = 'JAN'
ylab.2 = 'FEB'
ylab.3 = 'MAR'
ylab.4 = 'APR'
ylab.5 = 'MAY'
ylab.6 = 'JUN'
ylab.7 = 'JUL'
ylab.8 = 'AUG'
ylab.9 = 'SEP'
ylab.10 = 'OCT'
ylab.11 = 'NOV'
ylab.12 = 'DEC'

* Build the "times" array
 i = 1
 while (i<=nlabels)
  'set time 'ylab.i
  'q dims'
   rec = sublin(result,5)
   times.i = subwrd(rec,9)
   say times.i'  'ylab.i
   i = i + 1
 endwhile

* Get the plot-size info

'q gxinfo'
rec = sublin(result,3)
xmn = subwrd(rec,4)
xmx = subwrd(rec,6)
rec = sublin(result,4)
ymn = subwrd(rec,4)
ymx = subwrd(rec,6)

 i = 1
 'set string 1 c 4'
 'set strsiz 0.1 0.12'

while (i<=nlabels)
  'q gr2xy '0' '%(times.i-gtmin+1)
   x = xmn
   y = subwrd(result,6)

* Draw the date labels
  'draw string 'x-0.30' 'y' 'ylab.i

* Draw the tick marks
  'set line 1 1 1'
  'draw line '%(x-0.06)' 'y' 'x' 'y

* Draw the grid lines
  'set line 15 5 1'
  'draw line 'xmn' 'y' 'xmx' 'y
  i = i + 1
endwhile

*************************************************
*************************************************

