 'reinit'
 'set display color white'
 'c'

***********************************************************
***********************************************************

* the following lines are project- and plot-specific

* the variable you want to plot (cond, diam, flux, sden, temp)
    var_name = 'diam'
* title on plot
    title = 'Grain Diameter (mm)'
* grid cell and time domain of interest
    time_span = '12Z01SEP2017 12Z16JUL2018'
    ii = 20
    jj = 15
* plotting area
    parea ='1.5 10.0 1.5 7.5'
* snow depth range
    vrange = '0 130'
* from the snowmodel.par file
    max_sm_layers = 25

***********************************************************
***********************************************************

 'open ../ctl_files/wo_assim/multilayer_snod.ctl'
 'open ../ctl_files/wo_assim/multilayer_2Dxy.ctl'
 'open ../ctl_files/wo_assim/multilayer_'var_name'.ctl'

 'set grads off'
 'set datawarn off'

* define the point and time of interest
 'set x 'ii
 'set y 'jj
 'set z 1 'max_sm_layers+1
 'set time 'time_span

* make an initial plot to define the color and plotting domain
 'set gxout grfill'
 'd 1000*'var_name'z.3'
 'cbarn'

* extract the color shades information
 'q shades'
  shdinfo=result
  line=sublin(result,1)
  nlevs=subwrd(line,5)

  line=sublin(result,2)
  clr.1=subwrd(line,1)
  lv2.1=subwrd(line,3)
  lv1.1=lv2.1-1000000

  line=sublin(result,nlevs+1)
  clr.nlevs=subwrd(line,1)
  lv1.nlevs=subwrd(line,2)
  lv2.nlevs=lv1.nlevs+1000000

  n=2
  while ( n <= nlevs-1 )
    line=sublin(result,n+1)
    clr.n=subwrd(line,1)
    lv1.n=subwrd(line,2)
    lv2.n=subwrd(line,3)
    n=n+1
  endwhile

  n=1
  while ( n <= nlevs )
*   say clr.n' 'lv1.n' 'lv2.n
    n=n+1
  endwhile

  cinc=(lv1.nlevs-lv2.1)/(nlevs-2)

  say
  say ' hit enter to continue '

  pull dummy
 'c'

* define the final plotting domain by plotting the snowpack depth
 'set grads off'
 'set parea 'parea
 'set vrange 'vrange
 'set z 1'
 'set cmark 0'
 'set ccolor 1'
 'd 100*snod.2'
 'draw ylab snow depth (cm)'
 'draw title 'title

* extract the time domain
 'q dims'
  line=sublin(result,5)
  t1=subwrd(line,11)
  t2=subwrd(line,13)

* extract the plotting domain
 'q gxinfo'
  line=sublin(result,3)
  x1=subwrd(line,4)
  x2=subwrd(line,6)

  xinc=(x2-x1)/(t2-t1)

* define the top and bottom of each layer
 'set z 1'
  max_layers = max_sm_layers
  i = 1
 'define botlayer=0*snodz.1(z=1)'
  while ( i <= max_layers+1 )
    if ( i = 1 )
      'define toplayer=100*snodz.1(z='i')'
    else
      'define toplayer=botlayer+100*snodz.1(z='i')'
    endif

* plot the color shade for each layer for each time step
    t=t1
    while ( t <= t2 )
     'set t 't
     'q dims'
      line=sublin(result,5)
      tt=subwrd(line,6)

* find the snow depth of the bottom and top of each layer
*   at this time
     'd botlayer'
      snowd1=subwrd(result,4)
     'd toplayer'
      snowd2=subwrd(result,4)

* find the coordinates of the rectangle that covers this
*   layer for this time
     'q w2xy 'tt' 'snowd1
      xlo=subwrd(result,3)-xinc/2
      ylo=subwrd(result,6)
     'q w2xy 'tt' 'snowd2
      xhi=subwrd(result,3)+xinc/2
      yhi=subwrd(result,6)

* find the value for this layer at this time
     'd 1000*'var_name'z.3(z='i')'
      var=subwrd(result,4)

* find the color associated with this value
      eps=0.0000001
      if ( var <= lv2.1 )
        ii=1
      else
        ii = math_int((var - lv2.1 - eps)/cinc + 2.0)
      endif
      if ( var > lv1.nlevs )
        ii=nlevs
      endif

* draw the rectangle with the appropriate color
     'set line 'clr.ii
      if ( ylo != yhi )
* this if statement prevents plotting a color when there is
*   no line above the bottom line
        if ( snowd2 != -9.99e+08 )
          'draw recf 'xlo' 'ylo' 'xhi' 'yhi
        endif
      endif

* increment the time loop
     t=t+1
    endwhile

* reinitialize things so you can plot the next layer
   'set z 1'
   'set time 'time_span

   'define botlayer=toplayer'
    i=i+1
  endwhile

* plot the snow layers over the color shades
 'set xlab off'
 'set ylab off'
 'set cthick 4'
 'set cstyle 0'
 'set digsiz 0.001'

* find the number of layers in this plot
 'set gxout stat'
 'd KK.2'
  line=sublin(result,8)
  nlayers=subwrd(line,5)
 'set gxout line'

 i=1
 while ( i <= nlayers )
   'set ccolor 1'
   'set cmark 2'

    if ( i = 1 )
     'define layer=100*snodz.1(z='i')'
    else
     'define layer=layer+100*snodz.1(z='i')'
    endif

   'd layer'

    i=i+1
 endwhile

* plot the snow surface with a solid line
 'set cstyle 1'
 'set ccolor 1'
 'set cmark 0'
 'd 100*snod.2'

* plot the colorbar
 'q gxinfo'
  line=sublin(result,3)
  x1=subwrd(line,4)
  x2=subwrd(line,6)
  line=sublin(result,4)
  y1=0.0
  y2=subwrd(line,4)

  sf=0.95
  vert=0
  xmid=x1+(x2-x1)/2
  ymid=(y2-y1)/2

  mkcbar(shdinfo,sf,vert,xmid,ymid)

* save the plot
 'gprint_png fig_files/layers_'var_name

*************************************************
*************************************************

  function mkcbar(shdinfo,sf,vert,xmid,ymid)

* these are Glen's edits of: /home/gliston/grads/lib/cbarn.gs

*	sf   - scale the whole bar 1.0 = original 0.5 half the size, etc.
*	vert - 0 FORCES a horizontal bar = 1 a vertical bar
*	xmid - the x position on the virtual page the center the bar
*	ymid - the x position on the virtual page the center the bar
*

*sf=subwrd(args,1)
*vert=subwrd(args,2)
*xmid=subwrd(args,3)
*ymid=subwrd(args,4)

if(sf='');sf=1.0;endif

*
*  Check shading information
*
* 'query shades'
* shdinfo = result
  if (subwrd(shdinfo,1)='None') 
    say 'Cannot plot color bar: No shading information'
    return
  endif

* 
*  Get plot size info
*
  'query gxinfo'
  rec2 = sublin(result,2)
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xsiz = subwrd(rec2,4)
  ysiz = subwrd(rec2,6)
  ylo = subwrd(rec4,4)
  xhi = subwrd(rec3,6)
  xd = xsiz - xhi

  ylolim=0.6*sf
  xdlim1=1.0*sf
  xdlim2=1.5*sf  
  barsf=0.8*sf
  yoffset=0.2*sf
  stroff=0.05*sf
  strxsiz=0.12*sf
  strysiz=0.13*sf
*
*  Decide if horizontal or vertical color bar
*  and set up constants.
*
  if (ylo<ylolim & xd<xdlim1) 
    say "Not enough room in plot for a colorbar"
    return
  endif
  cnum = subwrd(shdinfo,5)
*
*	logic for setting the bar orientation with user overides
*
  if (ylo<ylolim | xd>xdlim1)
    vchk = 1
    if(vert = 0) ; vchk = 0 ; endif
  else
    vchk = 0
    if(vert = 1) ; vchk = 1 ; endif
  endif
*
*	vertical bar
*

  if (vchk = 1 )

    if(xmid = '') ; xmid = xhi+xd/2 ; endif
    xwid = 0.2*sf
    ywid = 0.5*sf
    
    xl = xmid-xwid/2
    xr = xl + xwid
    if (ywid*cnum > ysiz*barsf) 
      ywid = ysiz*barsf/cnum
    endif
    if(ymid = '') ; ymid = ysiz/2 ; endif
    yb = ymid - ywid*cnum/2
    'set string 1 l 5'
    vert = 1

  else

*
*	horizontal bar
*

    ywid = 0.4
    xwid = 0.8

    if(ymid = '') ; ymid = ylo/2-ywid/2 ; endif
    yt = ymid + yoffset
    yb = ymid
    if(xmid = '') ; xmid = xsiz/2 ; endif
    if (xwid*cnum > xsiz*barsf)
      xwid = xsiz*barsf/cnum
    endif
    xl = xmid - xwid*cnum/2
    'set string 1 tc 5'
    vert = 0
  endif


*
*  Plot colorbar
*


  'set strsiz 'strxsiz' 'strysiz
  num = 0
  while (num<cnum) 
    rec = sublin(shdinfo,num+2)
    col = subwrd(rec,1)
    hi = subwrd(rec,3)
    if (vert) 
      yt = yb + ywid
    else 
      xr = xl + xwid
    endif

    if(num!=0 & num!= cnum-1)
    'set line 1 1 10'
    'draw rec 'xl' 'yb' 'xr' 'yt
    'set line 'col
    'draw recf 'xl' 'yb' 'xr' 'yt
    if (num<cnum-1)
      if (vert) 
        xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
        yp=yb-stroff
        'draw string 'xr' 'yp' 'hi
      endif
    endif
    endif

    if(num = 0 )

      if(vert = 1)

        xm=(xl+xr)*0.5
        'set line 1 1 10'
        'draw line 'xl' 'yt' 'xm' 'yb
        'draw line 'xm' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'yt

        'set line 'col
        'draw polyf 'xl' 'yt' 'xm' 'yb' 'xr' 'yt' 'xl' 'yt

      else

        ym=(yb+yt)*0.5
        'set line 1 1 10'
        'draw line 'xl' 'ym' 'xr' 'yb
        'draw line 'xr' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'ym

        'set line 'col
       'draw polyf 'xl' 'ym' 'xr' 'yb' 'xr' 'yt' 'xl' 'ym

      endif

    endif

    if (num<cnum-1)
      if (vert)
         xp=xr+stroff 
        'draw string 'xp' 'yt' 'hi
      else
         yp=yb-stroff
        'draw string 'xr' 'yp' 'hi
      endif
    endif

    if(num = cnum-1 )

      if( vert = 1)
        'set line 1 1 10'
        'draw line 'xl' 'yb' 'xm' 'yt
        'draw line 'xm' 'yt' 'xr' 'yb
        'draw line 'xr' 'yb' 'xl' 'yb

        'set line 'col
        'draw polyf 'xl' 'yb' 'xm' 'yt' 'xr' 'yb' 'xl' 'yb
      else

        'set line 1 1 10'
        'draw line 'xr' 'ym' 'xl' 'yb
        'draw line 'xl' 'yb' 'xl' 'yt
        'draw line 'xl' 'yt' 'xr' 'ym

        'set line 'col
        'draw polyf 'xr' 'ym' 'xl' 'yb' 'xl' 'yt' 'xr' 'ym
        

      endif

    endif

    if (num<cnum-1)
      if (vert) 
        xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
        yp=yb-stroff
       'draw string 'xr' 'yp' 'hi
      endif
    endif

    num = num + 1
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile
return

*************************************************
*************************************************

