 'reinit'
 'open yearly_variables.ctl'
 'set mpdraw off'
 'set mproj scaled'
 'set gxout grfill'

 tt = 1

 while (tt <= 43)

   'set t 'tt

*  'd conc_a1a'
*  'd tair_a1a'
*  'd 100*smlt_s1a'
*  'd spre_sum'
*  'd snod_1ap'
*  'd 100*swed_1ap'
    'd bsfx_s1a'

*  'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
*  'd 1-smlt_s1a/swed_1ap'

   'cbarn'

   'q time'
   ret=sublin(result,1)
   date=subwrd(ret,3)
   yr=substr(date,9,4)
  
   'draw title 'yr
   'printim frame_'tt'.png png'

    pull dummy
    'c'

    tt = tt + 1
 endwhile

