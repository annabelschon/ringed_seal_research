  'reinit'

  'open /home/liston/modis/scov.w.ctl'
  'set grads off'

  'set t 15 160'
  'set xlab off'
  'd scov'

   timelabs()

*************************************************
*************************************************

  function timelabs()

* Open file and do the plot, making sure
*   to plot no labels on the x axis

* Get the x-axis grid (time) limits

'q dims'
 rec = sublin(result,5)
 gtmin = subwrd(rec,11)
 gtmax = subwrd(rec,13)

* Define the times that are to be plotted

nlabels = 5
year = '1997'
xlab.1 = '1FEB'
xlab.2 = '1MAR'
xlab.3 = '1APR'
xlab.4 = '1MAY'
xlab.5 = '1JUN'

* Build the "times" array
 i = 1
 while (i<nlabels+1)
  'set time 'xlab.i
  'q dims'
   rec = sublin(result,5)
   times.i = subwrd(rec,9)
   say times.i
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
 'set string 1 tc 4'
 'set strsiz 0.1 0.12'

while (i<nlabels+1)
  'q gr2xy '%(times.i-gtmin+1)%' '0
   x = subwrd(result,3)
   y = ymn

* Draw the date labels

  'draw string 'x' 'y-0.10' 'xlab.i

* Draw the grid lines and tick marks

  'set line 1 1 1'
  'draw line 'x' 'y' 'x' '%(y-0.06)
  'set line 15 5 1'
  'draw line 'x' 'ymn' 'x' 'ymx
  i = i + 1
endwhile

* Draw the year label

 'q gr2xy '%(times.1-gtmin+1)%' '0
  x = subwrd(result,3)
  y = subwrd(result,6)
 'draw string 'x' 'ymn-0.30' 'year

*************************************************
*************************************************

