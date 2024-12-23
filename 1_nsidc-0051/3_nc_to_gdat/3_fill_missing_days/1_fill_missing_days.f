c 1_fill_missing_days.f

c Sweep through the .gdat file looking for days with missing
c   values.  Then fill the missing days with valid data from the
c   previous day.

      implicit none

      integer nx,ny,ndays

      parameter (nx=304,ny=448)

c This came from: /annabel/seals/nsidc-0051/2_maxiter_offset/
c   01 Jan 1980 - 31 Mar 2024 = 16162.
      parameter (ndays=16162)

      real undef,wt
      integer i,j,nday,icount,nnday,nday_srt,nday_end,nt

      real conc_0(nx,ny,ndays)
      real conc_1(nx,ny,ndays)
      real conc_2(nx,ny,ndays)
      real conc_3(nx,ny,ndays)
      real conc_4(nx,ny,ndays)
      real conc_tmp(nx,ny,ndays)

      real xmissing_day(ndays)

c Variables and parameters associated with the linear interpolation
c   option.
      integer nearest_flag,ilo,mflag,k_srt,k_end,npts,inearest
      real t1,t2,y1,y2,tt,slope
      real tobs(ndays)

      character path1*(*)
      parameter (path1=
     &  '/data3/annabel/seals/1_nsidc_0051/2_gdat_orig/')

      character path2*(*)
      parameter (path2=
     &  '/data3/annabel/seals/1_nsidc_0051/3_gdat_final/')

c The undef value we are using.
      undef = -9999.0

c Read in the original data.
      open (21,file=path1//'ice_conc.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      print *
      print *, 'reading conc data'
      do nday=1,ndays
        if (mod(nday,1000).eq.0) print *,'nday =',nday
        read (21,rec=nday) ((conc_0(i,j,nday),i=1,nx),j=1,ny)
      enddo

c Save some copies of this for processing.  conc_3 and _4 will
c   filled later, so just put something in there that will pop
c   up if the filling does not go as envisioned.
      print *
      print *, 'copying conc data'
      do nday=1,ndays
        do j=1,ny
          do i=1,nx
            conc_1(i,j,nday) = conc_0(i,j,nday)
            conc_2(i,j,nday) = conc_0(i,j,nday)
            conc_3(i,j,nday) = - undef
            conc_4(i,j,nday) = - undef
          enddo
        enddo
      enddo

c Create a missing-day index.  Look in the lower left corner for
c   an undef value.  If you find one, set the "missing" flag to
c   undef, else 1.0.
      do nday=1,ndays
        if (conc_0(1,1,nday).eq.undef) then
          xmissing_day(nday) = undef
        else
          xmissing_day(nday) = 1.0
        endif
c       print *, nday,conc_0(1,1,nday),xmissing_day(nday)
      enddo

c The linear interpolation requires valid data at the start and
c   end of the data block.
c Check to see if on day = 1 the data are missing.
      if (conc_0(1,1,1).eq.undef) then
        print *, 'Day 1 has missing data.  You have to deal with'
        print *, 'that somehow (this code assumes you have good'
        print *, 'data on the first day).'
        stop
      endif
c Check to see if on day = ndays the data are missing.
      if (conc_0(1,1,ndays).eq.undef) then
        print *, 'The last day has missing data.  You have to fix'
        print *, 'with that somehow.'
        stop
      endif

c Count the number of missing days.
      icount = 0
      do nday=1,ndays
        if (xmissing_day(nday).eq.undef) then
          icount = icount + 1
        endif
      enddo
      print *, 'number of missing days =',icount

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c FILL MISSING DATA WITH PREVIOUS VALID VALUES.

c For days with undef values, fill them in using the distribution
c   from the previous day that had valid data.
      do nday=2,ndays
        if (xmissing_day(nday).eq.undef) then
c         print *,'processing missing day: nday=',nday
          do j=1,ny
            do i=1,nx
              conc_1(i,j,nday) = conc_1(i,j,nday-1)
            enddo
          enddo
        endif
      enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c FILL MISSING DATA USING simple LINEAR INTERPOLATION.

c This identifies the good-data start and ends of the data gap.
      nday_srt = 0
      nday_end = 0

      do nday=2,ndays

c Look for the start and end of a missing data period.
        if (xmissing_day(nday).eq.undef .and.
     &    xmissing_day(nday-1).ne.undef) nday_srt = nday - 1

        if (xmissing_day(nday).ne.undef .and.
     &    xmissing_day(nday-1).eq.undef) nday_end = nday

c When you find the start and end of this data gap, linearly
c   interpolate across the missing data.
        if (nday_srt.ne.0 .and. nday_end.ne.0) then

          do nnday=nday_srt+1,nday_end-1

            wt = real(nnday - nday_srt) / real(nday_end - nday_srt)
            print *, wt

            do j=1,ny
              do i=1,nx
                conc_2(i,j,nnday) =
     &    (1.0 - wt) * conc_2(i,j,nday_srt) + wt * conc_2(i,j,nday_end)
              enddo
            enddo

          enddo

c After this data gap is filled, reset the gap identifiers, and
c   look for the next gap that needs filling.
          nday_srt = 0
          nday_end = 0

        endif

      enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c FILL MISSING DATA USING complex LINEAR INTERPOLATION.

c I got subroutine dintrv(...)  from here:
c   http://jacobwilliams.github.io/finterp/sourcefile/
c     linear_interpolation_module.f90.html
c There are other multi-dimentional linear interpolators there
c   that use this code.
c This compiles with gfortran.

c You have two options here:
c   linear interploation (nearest_flag = 0)
c   nearest neighbor interpolation (nearest_flag = 1)
      nearest_flag = 0

c ilo is an initialization parameter which must be set to 1
c   the first time the array 'tobs' is processed by dintrv.
      ilo = 1

c This linear interpolator requires information defining when,
c   in time (or x, if you are interpolating over a spatial
c   domain), there are valid data to be interpolated between
c   (called observations).  You also have to know how many time
c   points (npts) are in this "valid times" array.  Build that
c   information here.
      npts = 0
      do nday=1,ndays
        if (conc_0(1,1,nday).ne.undef) then
          npts = npts + 1
          tobs(npts) = nday
        endif
      enddo
      print *
      print *, npts
      print *, npts
      print *, npts

c Also note that the linear interpolator assumes the incoming
c   "observations" don't have the missing/undef data layers
c   in the data array.  So, remove those here, creating a tmp
c   data cube that has tossed the undef time layers.
      nt = 0
      do nday=1,ndays
        if (conc_0(1,1,nday).ne.undef) then
          nt = nt + 1
          do j=1,ny
            do i=1,nx
              conc_tmp(i,j,nt) = conc_0(i,j,nday)
            enddo
          enddo
        endif
      enddo

c Loop through the time steps in the simulation.
      do nday=1,ndays

c tt is the time position you are interpolating to, as a real
c   number now instead of an integer.
        tt = real(nday)

c Find the obs position (in time) to the left (k_srt) and right
c   (k_end) of the time record of interest (tt).
        call dintrv(tobs,npts,tt,ilo,k_srt,k_end,mflag,
     &    nearest_flag,inearest,ndays)

c These are the times, in nday units, before and after the undef
c   area in the data.  So, conceptually, here t is like x, in
c   the line equation on an x-y plot (here we have a t-y plot).
        t1 = tobs(k_srt)
        t2 = tobs(k_end)

c This is what you would use if you just had a line of variables.
c       y1 = yobs(k_srt)
c       y2 = yobs(k_end)
c       slope = (y2 - y1) / (t2 - t1)
c       yy(nday) = slope * (tt - t1) + y1

c Do the linear interpolation across all the spatial grid cells.
        do j=1,ny
          do i=1,nx
c These are the two "y" values of the two points on the line.
            y1 = conc_tmp(i,j,k_srt)
            y2 = conc_tmp(i,j,k_end)

c Do the linear interplation calcuation in "y = slope*x + b" form.
            slope = (y2 - y1) / (t2 - t1)
            conc_3(i,j,nday) = slope * (tt - t1) + y1
          enddo
        enddo

      enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c FILL MISSING DATA USING NEAREST NEIGHBOR FILLING.

c Nearest neighbor interpolation (nearest_flag = 1)
      nearest_flag = 1

c ilo is an initialization parameter which must be set to 1
c   the first time the array 'tobs' is processed by dintrv.
      ilo = 1

c Loop through the time steps in the simulation.
      do nday=1,ndays

c tt is the time position you are interpolating to, as a real
c   number now instead of an integer.
        tt = real(nday)

c Find the obs position (in time) to the left (k_srt) and right
c   (k_end) of the time record of interest (tt).
        call dintrv(tobs,npts,tt,ilo,k_srt,k_end,mflag,
     &    nearest_flag,inearest,ndays)

c Do the data filling across all the spatial grid cells.
        do j=1,ny
          do i=1,nx
            conc_4(i,j,nday) = conc_tmp(i,j,inearest)
          enddo
        enddo

      enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Save the data.
      print *
      print *, 'saving conc data'
      print *

c Maybe this is what you want to use when you have decided on
c   your data-filling method.
c     open (31,file=path2//'ice_conc_1980-2024.gdat',

c Data filling option 1.
      open (31,file=path2//'ice_conc_v1.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      do nday=1,ndays
        if (mod(nday,3000).eq.0) print *,'nday =',nday
        write (31,rec=nday) ((conc_1(i,j,nday),i=1,nx),j=1,ny)
      enddo
      print *

c Data filling option 2.
      open (32,file=path2//'ice_conc_v2.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      do nday=1,ndays
        if (mod(nday,3000).eq.0) print *,'nday =',nday
        write (32,rec=nday) ((conc_2(i,j,nday),i=1,nx),j=1,ny)
      enddo
      print *

c Data filling option 3.
      open (33,file=path2//'ice_conc_v3.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      do nday=1,ndays
        if (mod(nday,3000).eq.0) print *,'nday =',nday
        write (33,rec=nday) ((conc_3(i,j,nday),i=1,nx),j=1,ny)
      enddo
      print *

c Data filling option 4.
      open (34,file=path2//'ice_conc_v4.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      do nday=1,ndays
        if (mod(nday,3000).eq.0) print *,'nday =',nday
        write (34,rec=nday) ((conc_4(i,j,nday),i=1,nx),j=1,ny)
      enddo
      print *

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine dintrv(xt,n,x,ilo,ileft,iright,mflag,
     &  nearest,inearest,nmax)

      implicit none

      integer nmax
c     real xt(n)   !! obs vector
      real xt(nmax)   !! obs vector
      real x       !! argument
      integer ilo  !! an initialization parameter which must be set
                   !! to 1 the first time the array `xt` is
                   !! processed by dintrv. `ilo` contains information for
                   !! efficient processing after the initial call and `ilo`
                   !! must not be changed by the user.  each dimension
                   !! requires a distinct `ilo` parameter.
      integer ileft    !! left index
      integer iright   !! right index
      integer mflag    !! signals when `x` lies out of bounds
      integer inearest !! nearest index

      integer ihi,istep,imid,n,nearest

      if (n==1) then
c this is only allowed for nearest interpolation
        if (nearest.eq.1) then
            inearest = 1
            return
        endif
      endif

      ihi = ilo + 1
      if ( ihi>=n ) then
          if ( x>=xt(n) ) then
              mflag = 1
              ileft = n-1
              iright= n
              if (nearest.eq.1) inearest = n
              return
          endif
          if ( n<=1 ) then
              mflag = -1
              ileft = 1
              iright= 2
              if (nearest.eq.1) inearest = 1
              return
          endif
          ilo = n - 1
          ihi = n
      endif

      if ( x>=xt(ihi) ) then

c now x >= xt(ilo). find upper bound
          istep = 1
          do
              ilo = ihi
              ihi = ilo + istep
              if ( ihi>=n ) then
                  if ( x>=xt(n) ) then
                      mflag = 1
                      ileft = n-1
                      iright= n
                      if (nearest.eq.1) inearest = n
                      return
                  endif
                  ihi = n
              elseif ( x>=xt(ihi) ) then
                  istep = istep*2
                  cycle
              endif
              exit
          enddo

      else

          if ( x>=xt(ilo) ) then
              mflag = 0
              ileft = ilo
              iright= ilo+1
              if (nearest.eq.1) then
                  if ( abs(x-xt(ileft)) <= abs(x-xt(iright)) ) then
                      inearest = ileft
                  else
                      inearest = iright
                  endif
              endif
              return
          endif
c now x <= xt(ihi). find lower bound
          istep = 1
          do
              ihi = ilo
              ilo = ihi - istep
              if ( ilo<=1 ) then
                  ilo = 1
                  if ( x<xt(1) ) then
                      mflag = -1
                      ileft = 1
                      iright= 2
                      if (nearest.eq.1) inearest = 1
                      return
                  endif
              elseif ( x<xt(ilo) ) then
                  istep = istep*2
                  cycle
              endif
              exit
          enddo

      endif

c now xt(ilo) <= x < xt(ihi). narrow the interval
      do
          imid = (ilo+ihi)/2
          if ( imid==ilo ) then
              mflag = 0
              ileft = ilo
              iright= ilo+1
              if (nearest.eq.1) then
                  if ( abs(x-xt(ileft)) <= abs(x-xt(iright)) ) then
                      inearest = ileft
                  else
                      inearest = iright
                  endif
              endif
              return
          endif
c note. it is assumed that imid = ilo in case ihi = ilo+1
          if ( x<xt(imid) ) then
              ihi = imid
          else
              ilo = imid
          endif
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

