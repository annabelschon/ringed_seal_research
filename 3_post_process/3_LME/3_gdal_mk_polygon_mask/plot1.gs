 'reinit'

 'open mask.ctl'

 'set mpdraw off'
*'set gxout grfill'
 'set gxout fgrid'

 'set fgvals 1 2 2 3 3 4'

 'd mask'
*'cbarn'

  pull dummy
  'c'

 'set x 180 220'
 'set y 90 150'

 'set fgvals 1 2 2 3 3 4'
 'd mask'

 'set gxout grid'
 'd mask'

 'gxprint LME_zoomed_grid.png png'
