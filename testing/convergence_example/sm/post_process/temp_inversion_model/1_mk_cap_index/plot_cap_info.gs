 'reinit'
 'set display color white'
 'c'

 'open cap_info.ctl'
 'set mpdraw off'
 'set grads off'

 'set gxout grfill'

 'set xlab off'
 'set ylab off'

 'd topo'
 'cbarn'
 'draw title topo'
  pull dummy
 'c'

 'd slope'
 'cbarn'
 'draw title slope'
  pull dummy
 'c'

 'd curve'
 'cbarn'
 'draw title curvature'
  pull dummy
 'c'

 'd frac'
 'cbarn'
 'draw title fraction'
  pull dummy
 'c'

 'set gxout fgrid'
 'set fgvals 0 1 1 15 2 0'
 'd cap'
 'draw title cap'
  pull dummy
 'c'

 'set gxout grfill'
 'd cap_smth'
 'cbarn'
 'draw title cap'

