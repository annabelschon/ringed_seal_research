'reinit'
'open 3_ice_conc_extracted_domain.ctl'

'set mpdraw off'
'set grads off'

'set gxout line'

* Annabel, it looks like these middle positions in x and y are
*   putting you in the North Pole remote sensing hole.  So, I
*   defined the (i,j), or (x,y), position to be somewhere else.

* Middle of x range (1 to 304)
*'set lon 150'
* middle of y range (1 to 448)
*'set lat 224'

'set x 60'
'set y 286'

* label for time (x-axis)
*'set xlab "Time (days)"' 
*'set ylab "Ice Concentration"'

* show the time series of ice con at the specified point for all time steps
* covers all time steps from beginning to end
'set cmark 0'
'set t 1 15705'

'set vrange 0 1'

'd conc'

'draw ylab Ice Concentration (%)'

* Save the time series plot as an image
'printim time_series_all_years.png png white'

*'quit'

