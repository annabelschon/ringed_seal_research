 'reinit'
 'set display color white'
 'c'

 'open ../2_mk_aves_and_trends/averages_2D.ctl'
 'open ../2_mk_aves_and_trends/trends_2D.ctl'
 'open ../2_mk_aves_and_trends/domain_average_trends.ctl'

* The mask outline of Arctic landmass.
 'open ../1_mk_yearly_data/ice_conc_mask.ctl'

***********************************************************
***********************************************************

* The following lines are project- and plot-specific.
* You will also have to change the plotting and color
*   increments if you want something different than the
*   defaults.  Those are scattered throughout the script
*   below.

*  'open ../../../topo_vege/NoAm_30m/topo_vege.ctl'
    var_name = 'conc_month'
    a_lab = 'Average sea ice (%)'
    b_lab = 'Sea ice trend (% decade`a-1`n)'
    c_lab1 = 'Domain-average sea ice (%)'
    c_lab2 = '(%)'

* This is set up to do the plot in two steps:
*   1) set run=1, make the plot.
*   2) modify the parts below, set run=2, and run the
*      plotting script again.

*   run=1
    run=2

* Note that panel b clevs assumes 4 negative values, one 0,
*   and 4 positive values.
    panel_b_clevs = '-20 -15 -10 -5 0 5 10 15  20'
    panel_c_vrange = '5 45'
    panel_c_ylint = '5'

***********************************************************
***********************************************************

* Get domain size information.
   'q dims'
    line1 = sublin(result,2)
    line2 = sublin(result,3)
    nx = subwrd(line1,13)
    ny = subwrd(line2,13)
    ratio = nx/ny

* If ratio .ge. 1.0, STACKED.
*   xxxxxxxxxxx
*   xxxxxxxxxxx
*   xxxxxxxxxxx
*
*   xxxxxxxxxxx
*   xxxxxxxxxxx
*   xxxxxxxxxxx
*
*   xxxxxxxxxxx

* If ratio .lt. 1.0, SIDE-BY-SIDE.

*   xxxxx xxxxx
*   xxxxx xxxxx
*   xxxxx xxxxx
*   xxxxx xxxxx
*   xxxxx xxxxx
*   xxxxx xxxxx

*   xxxxxxxxxxx

* These are all going to be portrait orientation.
  vp_bot = 0.0
  vp_top = 11.0
  vp_left = 0.0
  vp_right = 8.5

* Ratio = 304/448= 0.678

  if (ratio>1.75)
    say
    say
    say 'THIS PLOTING SCRIPT WILL HAVE TO BE MODIFIED FOR'
    say 'nx/ny ASPECT RATIOS > 1.75: current ratio = 'ratio
    say
    say
  endif

  if (ratio<0.46)
    say
    say
    say 'THIS PLOTING SCRIPT WILL HAVE TO BE MODIFIED FOR'
    say 'nx/ny ASPECT RATIOS < 0.46: current ratio = 'ratio
    say
    say
  endif

  if (ratio>=1.0)

* STACKED.

* parea box coords.
    top_offset = 0.35
    dy12 = 4.0
    left_offset = 0.25
    pa_top1 = vp_top-top_offset
    pa_bot1 = pa_top1-dy12
    dpa_y = pa_top1-pa_bot1
    pa_left1 = vp_right/2-left_offset-0.5*dy12*ratio
    pa_right1 = vp_right/2-left_offset+0.5*dy12*ratio

    top_offset = 0.3
    pa_top2 = pa_bot1-top_offset
    pa_bot2 = pa_top2-dy12
    pa_left2 = pa_left1
    pa_right2 = pa_right1

    dy3 = 1.5
    pa_top3 = pa_bot2-top_offset
    pa_bot3 = pa_top3-dy3
    pa_left3 = pa_left2
    pa_right3 = pa_right2

* color bar position.
    cb_vert = 1
    cb_scale = 0.7
    cbx_offset = 0.3
    cbarnx1 = pa_right1+cbx_offset
    cbarny1 = (pa_top1-pa_bot1)/2+pa_bot1

    cbarnx2 = pa_right2+cbx_offset
    cbarny2 = (pa_top2-pa_bot2)/2+pa_bot2

* panel 'a' label position.
    lab_offset = 0.3
    labx1 = pa_left1-lab_offset
    laby1 = cbarny1

* panel 'b' label position.
    labx2 = pa_left2-lab_offset
    laby2 = cbarny2

* panel 'c' label position.
    lab_offset = 0.5
    labx3 = pa_right3+lab_offset
    laby3 = (pa_top3-pa_bot3)/2+pa_bot3

* trend label location.
    tlab_offset = 0.2
    trend_labx = (pa_right3-pa_left3)/2+pa_left3
    trend_laby = pa_top3-tlab_offset

  else

* SIDE-BY-SIDE.

* parea box coords.
    top_offset = 0.7
    right_shift = 0.5
    right_border = 0.4
    left_border = right_border+right_shift
    middle_space = 0.3
    dx12 = (vp_right-right_border-left_border-middle_space)/2

    pa_left1 = left_border
    pa_right1 = left_border+dx12
    pa_top1 = vp_top-top_offset
    pa_bot1 = pa_top1-dx12/ratio

    pa_left2 = pa_right1+middle_space
    pa_right2 = pa_left2+dx12
    pa_top2 = pa_top1
    pa_bot2 = pa_bot1

    top_offset = 0.8
    dy3 = 1.5
    pa_top3 = pa_bot2-top_offset
    pa_bot3 = pa_top3-dy3
    pa_left3 = pa_left1
    pa_right3 = pa_right2

* color bar position.
    cb_vert = 0
    cb_scale = 0.5
    cbx_offset = 0.3
    cbarnx1 = (pa_right1-pa_left1)/2+pa_left1
    cbarny1 = pa_bot1-cbx_offset

    cbarnx2 = (pa_right2-pa_left2)/2+pa_left2
    cbarny2 = pa_bot2-cbx_offset

* panel 'a' label position.
    lab_offset = 0.3
    labx1 = cbarnx1
    laby1 = pa_top1+lab_offset

* panel 'b' label position.
    labx2 = cbarnx2
    laby2 = pa_top1+lab_offset

* panel 'c' label position.
    lab_offset = 0.3
    labx3 = pa_left3-lab_offset
    laby3 = (pa_top3-pa_bot3)/2+pa_bot3

* trend label location.
    tlab_offset = 0.2
    trend_labx = (pa_right3-pa_left3)/2+pa_left3
    trend_laby = pa_top3-tlab_offset

  endif

***********************************************************
***********************************************************

 months = 'AUGUST SEPTEMBER OCTOBER NOVEMBER DECEMBER JANUARY FEBRUARY MARCH APRIL MAY JUNE JULY'

 tt = 1

 while ( tt <= 12 )

   'set dfile 1'
   'set t 'tt
   'set x 1 304'
   'set y 1 448'

    say tt
    
 month_name = subwrd(months, tt)

* Add the overhead title

 'set strsiz 0.15 0.30' 
 'set string 1 c 12  0'
  say 'month_name = 'month_name
*'draw string '4.25' '0.0' Month: 'month_name
  title_labx = ((vp_left+vp_right)/2)
  title_laby = (vp_bot + 2.2)
 'draw string 'title_labx' 'title_laby' 'month_name
*'draw string 'trend_labx' 'trend_laby' change decade`a-1`n = 'trend

* PANEL a.

 'set vpage 'vp_left' 'vp_right' 'vp_bot' 'vp_top
 'set parea 'pa_left1' 'pa_right1' 'pa_bot1' 'pa_top1

 'set mpdraw off'
 'set mproj scaled'
 'set grads off'
 'set xlab off'
 'set ylab off'
 'set gxout grfill'

*'set clevs 30 40 50 60 70 80 90 100 150 200 250 300'
 'd 'var_name'.1'
 'cbar_glen 'cb_scale' 'cb_vert' 'cbarnx1' 'cbarny1

*'set gxout contour'
*'set clab off'
*'set ccolor 0'
*'set cthick 1'
*'d topo.4(t=1)'

*'set strsiz 0.16'
  strysiz = 0.20
 'set strsiz 0.10 'strysiz
*'set strsiz 0.12 0.24'
  if (ratio>=1.0)
    'set string 1 c 5 90'
  else
    'set string 1 c 5 0'
  endif

 'draw string 'labx1' 'laby1' 'a_lab

* make the land grey
 'set rgb 61 150 150 150'
 'set gxout fgrid'
 'set fgvals 1 61'
 'd mask.4(t=1)'

 'abcd a 'tr''

***********************************************************
***********************************************************

* PANEL b.

 'set vpage 'vp_left' 'vp_right' 'vp_bot' 'vp_top
 'set parea 'pa_left2' 'pa_right2' 'pa_bot2' 'pa_top2

 'set mpdraw off'
 'set mproj scaled'
 'set grads off'
 'set xlab off'
 'set ylab off'
 'set gxout grfill'

* BLUE shades
 'set rgb 16   0   0 255'
 'set rgb 17  55  55 255'
 'set rgb 18 110 110 255'
 'set rgb 19 165 165 255'
 'set rgb 20 220 220 255'
* RED shades
 'set rgb 21 255 220 220'
 'set rgb 22 255 165 165'
 'set rgb 23 255 110 110'
 'set rgb 24 255  55  55'
 'set rgb 25 255   0   0'

  if (run=2)
   'set clevs 'panel_b_clevs
   'set ccols 16 17 18 19 20 21 22 23 24 25'
  endif

 'd 10*'var_name'.2'
 'cbar_glen 'cb_scale' 'cb_vert' 'cbarnx2' 'cbarny2

 'set gxout fgrid'
 'set fgvals 1 61'
 'd mask.4(t=1)'

  strsiz=0.20
 'set strsiz 0.10 'strsiz
  if (ratio>=1.0)
    'set string 1 c 5 90'
  else
    'set string 1 c 5 0'
  endif
 'draw string 'labx2' 'laby2' 'b_lab

* make the oceans grey and the glaciers white
*'set gxout fgrid'
*'set fgvals 24 61 20 0'
*'d vege.4(t=1)'

 'abcd b 'tr''

***********************************************************
***********************************************************

* PANEL c.

 'set vpage 'vp_left' 'vp_right' 'vp_bot' 'vp_top
 'set parea 'pa_left3' 'pa_right3' 'pa_bot3' 'pa_top3

 'set grads off'

 'set xlopts 1 4 0.12'

  if (run=2)
    'set vrange 'panel_c_vrange
    'set ylint 'panel_c_ylint
  endif

 'set ccolor 1'
 'set xlab on'
 'set ylab on'
 'set dfile 3'
 'q file'
  line=sublin(result,5)
  nt=subwrd(line,12)
 'set x 1'
*'set y 1'
 'set y 'tt
 'set t 1 'nt
  if (ratio>=1.0)
    'set ylpos 0 r'
  endif

 'd 'var_name'.3(x=1)'
 'set cmark 0'
 'set ccolor 1'
 'd 'var_name'.3(x=2)'
 
 strsiz=0.15
 'set strsiz 0.075 'strsiz
 'set string 1 c 5 90'
  if (ratio>=1.0)
    'draw string 'labx3' 'laby3' 'c_lab1
*   'draw string 'labx3' 'laby3
*   'draw string 'labx3+0.22' 'laby3' 'c_lab2
*   'draw string 'labx3+0.22' 'laby3
  else
    'draw string 'labx3-0.22' 'laby3' 'c_lab1    
*   'draw string 'labx3-0.22' 'laby3
*   'draw string 'labx3' 'laby3' 'c_lab2
*   'draw string 'labx3' 'laby3
  endif

* Extract the trend per decade and plot it on the graph.
 'set t 1'
 'd 10*'var_name'.3(x=3)'
  trend = subwrd(result,4)
  trend = math_format('%-7.2f',trend)
 'set strsiz 0.13'
 'set string 1 c 5 0'
 'draw string 'trend_labx' 'trend_laby' change decade`a-1`n = 'trend

 'abcd c 'tr''

 'gprint fig_files/'var_name''tt''
  pull dummy
  'c'

  tt = tt + 1

 endwhile

***********************************************************
***********************************************************

*'gprint fig_files/'var_name''

***********************************************************
***********************************************************

