 'reinit'
*'set display color white'
*'c'

 'open ../../ctl_files/wo_assim/snod.ctl'

 'set mproj scaled'
 'set mpdraw off'
 'set grads off'

 'set xlopts 1 5 0.16'
 'set ylopts 1 5 0.16'

 'set dbuff on'

 t=1
 while (t<=365)

   'set grads off'
   'set xlab %g'
   'set ylab %g'
*  'set xlint 50'
*  'set ylint 50'
   'set gxout grfill'

   'set t 't

*  'set clevs 0 10 25 50 75 100 200 300 400 500'

   'd 100*snod'
   'cbarn'

   'q time'
    ret=sublin(result,1)
    date=subwrd(ret,3)
    hr=substr(date,1,2)
    dy=substr(date,4,2)
    mo=substr(date,6,3)
    yr=substr(date,9,4)

   'draw title Snow Depth (cm)   'dy' 'mo' 'yr

*   pull dummy

   'swap'
    t = t + 1
 endwhile

************************************

