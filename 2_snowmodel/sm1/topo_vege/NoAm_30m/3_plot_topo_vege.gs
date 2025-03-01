 'reinit'
 'set display color white'
 'c'

 'open topo_vege.ctl'

  left = 0.75
  right = 0.50
  top = 0.50
  bottom = 0.75
 'set_parea_pl 'left' 'right' 'top' 'bottom

 'set mpdraw off'
 'set grads off'

 'set xlab %g'
 'set ylab %g'

* These colors come from the NALCMS color plot (these are the
* colors they use).  When they didn't have a color for the
* SnowModel class, I set the color to a grey shade.
 'set rgb 211 0 61 0'
 'set rgb 212 148 156 112'
 'set rgb 221 0 99 0'
 'set rgb 222 30 171 5'
 'set rgb 223 20 140 61'
 'set rgb 23 92 117 43'
*'set rgb 24 40 40 40'
 'set rgb 24 0 0 0'
*'set rgb 25 60 60 60'
 'set rgb 25 0 0 0'
 'set rgb 26 179 138 51'
*'set rgb 27 80 80 80'
 'set rgb 27 0 0 0'
 'set rgb 28 179 158 43'
*'set rgb 29 100 100 100'
 'set rgb 29 0 0 0'
 'set rgb 30 156 117 84'
*'set rgb 31 120 120 120'
 'set rgb 31 0 0 0'
 'set rgb 321 232 220 94'
 'set rgb 322 225 207 94'
*'set rgb 33 140 140 140'
 'set rgb 33 0 0 0'
 'set rgb 34 186 212 143'
*'set rgb 35 160 160 160'
 'set rgb 35 0 0 0'
*'set rgb 36 180 180 180'
 'set rgb 36 0 0 0'
 'set rgb 37 107 163 138'
 'set rgb 381 64 138 112'
 'set rgb 382 168 171 174'
 'set rgb 39 76 112 163'
 'set rgb 40 255 250 255'
 'set rgb 41 220 33 38'
 'set rgb 42 230 174 102'
*'set rgb 43 200 200 200'
 'set rgb 43 0 0 0'
 'set rgb 44 220 220 220'

 'set gxout fgrid'

 'set fgvals 1 211   2 223  3 23  4 24  5 25  6 26  7 27  8 28  9 29  10 30  11 31  12 321  13 33  14 34  15 35  16 36  17 37  18 382  19 39  20 40  21  41  22 42  23 43  24 44'

 'd vege'

 'set gxout contour'
 'd topo'

