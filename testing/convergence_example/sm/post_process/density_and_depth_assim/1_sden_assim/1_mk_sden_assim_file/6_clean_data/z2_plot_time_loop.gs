 'reinit'
*'set display color white'
*'c'

 'open snotel_snocrs_sden.ctl'

 fname = plot_info.dat

 return = read(fname)

 return = read(fname)
 record = sublin(return,2)
 nstns = subwrd(record,1)

 return = read(fname)
 record = sublin(return,2)
 year_start = subwrd(record,1)

 return = read(fname)
 record = sublin(return,2)
 year_end = subwrd(record,1)

 return = read(fname)
 record = sublin(return,2)
 nsnotel = subwrd(record,1)

 say 'nstns = 'nstns
 say 'year_start = 'year_start
 say 'year_end = 'year_end
 say 'nsnotel = 'nsnotel

 yr = year_start
 yr_end = year_end - 1

 while (yr <= yr_end)

 'set time 1sep'yr' 31aug'yr+1

   xx = 1
   while (xx <= nstns)

   'set x 'xx
    say  xx

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

   'set vrange 0 600'

    if (xx <= nsnotel)
     'set cmark 0'
    else
     'set lwid 13 2.0'
     'set cthick 13'
     'set cmark 6'
     'set digsiz 0.6'
    endif

   'd sden'

    xx = xx + 1

   endwhile

  if (yr != yr_end)
    say ""
    say " hit `enter` for next plot"
    pull dummy
   'c'
  endif

  yr = yr + 1

 endwhile

