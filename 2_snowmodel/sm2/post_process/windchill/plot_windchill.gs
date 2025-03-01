 'reinit'
 'set display color white'
 'c'

 'open windchill.ctl'
 'open ../../ctl_files/wo_assim/tair.ctl'
*********************************
*First plot wind chill
 'set vpage 0 5.5 0 8.5'
 'set parea 0.5 5.0 1.81 6.69'
 
 'set mpdraw off'
 'set mproj scaled'
 'set grads off'
 'set annot 1 4'
 'set xlab off'
 'set ylab off'

 'set gxout grfill'
 'color -50 50 -div 10 -kind darkblue->white->red'

*'set time 03Z01SEP2000'
*'set time 03Z01DEC2000'
 'set time 03Z01MAR2001'
*'set time 03Z01JUN2001'

 'd chill.1'

***********************************
*Now plot air temperature
 'set vpage 5.5 11 0 8.5'
 'set parea 6.0 10.5 1.81 6.69'
 
 'set mpdraw off'
 'set mproj scaled'
 'set grads off'
 'set annot 1 4'
 'set xlab off'
 'set ylab off'

 'set gxout grfill'
 'color -50 50 -div 10 -kind darkblue->white->red'

*'set time 03Z01SEP2000'
*'set time 03Z01DEC2000'
 'set time 03Z01MAR2001'
*'set time 03Z01JUN2001'

 'd tair.2'

