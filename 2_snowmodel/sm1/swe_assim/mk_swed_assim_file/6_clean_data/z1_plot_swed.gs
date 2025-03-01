 'reinit'
*'set display color white'
*'c'

 'open snotel_snocrs_swed.ctl'

 fname = plot_info.dat

 return = read(fname)
 record = sublin(return,2)
 tmax = subwrd(record,1)

 return = read(fname)
 record = sublin(return,2)
 nstns = subwrd(record,1)

 return = read(fname)
 return = read(fname)

 return = read(fname)
 record = sublin(return,2)
 nsnotel = subwrd(record,1)

 say 'ndays = 'tmax
 say 'nstns = 'nstns
 say 'nsnotel = 'nsnotel

 'set t 1 'tmax
 'set x 1'

 xx = 1
 while (xx <= nstns)

 'set x 'xx

 'set parea 1.0 10.0 1.75 7.25'
 'set grads off'
 'set datawarn off'

   if (xx = 1)
    'set xlab on'
    'set ylab on'
   else
    'set xlab off'
    'set ylab off'
   endif

 'set xlopts 1 5 0.16'
 'set ylopts 1 5 0.16'

 'set vrange -20 200'

    if (xx <= nsnotel)
     'set cmark 0'
    else
     'set lwid 13 2.0'
     'set cthick 13'
     'set cmark 6'
     'set digsiz 0.6'
    endif

 'd 100*swed'

 'set line 0'
 'draw recf 3.5 7.3 7.5 8.1'
 'set string 1 c 6'
 'set strsiz 0.25'
 'draw string 5.5 7.7 station = 'xx

* pull dummy
*'c'

  xx = xx + 1

 endwhile

