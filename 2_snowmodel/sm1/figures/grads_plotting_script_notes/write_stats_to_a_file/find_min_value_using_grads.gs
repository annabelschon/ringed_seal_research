 'reinit'

 'open /home/kgura/skyler/ctl_files/wo_assim/cldf.ctl'

 'set gxout stat'

  final_min_value = 10000000000.0

  tt = 1

  while (tt <= 1461)

   'set t 'tt

   'd cldf'
    line = sublin(result,8)
    min_value = subwrd(line,4)

    say min_value

    if (min_value < final_min_value)
      final_min_value = min_value
    endif

    tt = tt + 1

  endwhile

  fmv = math_format('%4.2f',final_min_value)

  say
  say 'final_min_value = 'final_min_value
  say
  say 'final_min_value = 'fmv
  say

