 'reinit'

 'open ../ctl_files/wo_assim/multilayer_temp.ctl'
 'open ../ctl_files/wo_assim/multilayer_cond.ctl'
 'open ../ctl_files/wo_assim/multilayer_diam.ctl'
 'open ../ctl_files/wo_assim/multilayer_flux.ctl'
 'open ../ctl_files/wo_assim/multilayer_sden.ctl'

 'set mpdraw off'
 'set gxout grfill'

 'set x 20'
 'set y 15'
 'set z 0.5 26.5'
 'set t 1 365'
 'set ylint 1'

 'd tempz.1'
 'cbarn'
 'draw title temp'

  pull dummy
 'c'

 'd condz.2'
 'cbarn'
 'draw title cond'

  pull dummy
 'c'

 'd 1000*diamz.3'
 'cbarn'
 'draw title diam'

  pull dummy
 'c'
 'd fluxz.4'
 'cbarn'
 'draw title flux'

  pull dummy
 'c'
 'd sdenz.5'
 'cbarn'
 'draw title sden'

