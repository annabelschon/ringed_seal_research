 'reinit'
*'set display color white'
*'c'

 'open snotel_snocourse_sm_domain.ctl'

 fname = plot_info.dat

 return = read(fname)
 record = sublin(return,2)
 tmax = subwrd(record,1)

 return = read(fname)
 return = read(fname)
 return = read(fname)

 return = read(fname)
 record = sublin(return,2)
 nsnotel = subwrd(record,1)

 say 'ndays = 'tmax
 say 'nsnotel = 'nsnotel

 'set t 1 'tmax
 'set x 1'

 xx = 1
 while (xx <= nsnotel)

 'set x 'xx

 'set parea 1.0 10.0 1.75 7.25'
 'set grads off'
 'set datawarn off'
 'set vrange -40 40'

 'set xlopts 1 5 0.16'
 'set ylopts 1 5 0.16'

   if (xx = 1)
    'set xlab on'
    'set ylab on'
   else
    'set xlab off'
    'set ylab off'
   endif

 'set cmark 0'
 'd tair'

 'set line 0'
 'draw recf 3.5 7.3 7.5 8.1'
 'set string 1 c 6'
 'set strsiz 0.25'
 'draw string 5.5 7.7 station = 'xx

* pull dummy
*'c'

  xx = xx + 1

 endwhile

