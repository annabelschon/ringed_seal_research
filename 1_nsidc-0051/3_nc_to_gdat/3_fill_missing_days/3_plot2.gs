 'reinit'

 'open ice_conc_v0.ctl'
 'open ice_conc_v1.ctl'
 'open ice_conc_v2.ctl'
 'open ice_conc_v3.ctl'
 'open ice_conc_v4.ctl'

 'set mpdraw off'
 'set grads off'

 'set x 60'
 'set y 286'

*'set t 1 16162'

* Look at the daily gaps.

 'set t 1 36'

 'set ccolor 2'

*'set cmark 0'
 'd conc.2'

*'set cmark 0'
 'd conc.1'

*'set cmark 0'
 'd conc.3'

  pull dummy

*'set cmark 0'
 'd conc.4'

  pull dummy

*'set cmark 0'
 'd conc.5'


* Look at the big gap.

  pull dummy
 'c'

 'set time 01NOV1987 01FEB1988'

 'set ccolor 4'

*'set cmark 0'
 'd conc.2'

*'set cmark 0'
 'd conc.1'

  pull dummy

*'set cmark 0'
*'d conc.3'

  pull dummy

*'set cmark 0'
 'd conc.4'

  pull dummy

*'set cmark 0'
 'd conc.5'

