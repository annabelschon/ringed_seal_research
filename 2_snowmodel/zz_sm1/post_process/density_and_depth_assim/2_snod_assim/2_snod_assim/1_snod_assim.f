c 1_snod_assim.f

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c subroutine DENSITY_ASSIM_USER does the snow density assimilation
c   after a SnowModel run has finished.

c Note that this is set up to do multi-year runs, but it only
c   assimilates one density observation date in each year.  For
c   an example of assimilating multiple density observation dates
c   in a year, see /hightsi/mosaic_ship/1_sm/19_density_assim/.

      implicit none

      integer nx,ny,nstns_max,nx_max,ny_max

      parameter (nx_max=10000,ny_max=10000)
      parameter (nstns_max=400)

      integer iyear_init,imonth_init,iday_init,nyears,ndays,max_iter
      real deltax,deltay,dt,undef,print_inc,beta
      double precision xmn,ymn

      character*120 input_path
      character*120 output_path
      integer trailing_blanks,i_len_input,i_len_output

c  Beta controls the interpolation distance weights.  Beta = 1.0
c    will give you a very smooth field, and correction factor
c    distributions that may not produce densities that exactly match
c    the observations.  Beta << 1.0 will give you correction factor
c    fields that go right through the data.  If you just have one
c    data point/area, beta is not used.
c     beta = 1.0
c     beta = 0.5
      beta = 0.25
c     beta = 0.1
c     beta = 1.0

c Read in the data input and output paths.
      open (71,file=
     &  '../../1_sden_assim/2_sden_assim/0_input_output_paths.dat')
      read (71,*)
      read (71,98) input_path
      read (71,98) output_path
   98 format (a120)
      i_len_input = 120 - trailing_blanks(input_path)
      i_len_output = 120 - trailing_blanks(output_path)

c Send this into the subroutine.
c     input_path(1:i_len_input)
c     output_path(1:i_len_output)

c Provide the required input information.  This information comes
c   out of the snowmodel.par file.
c     deltax = 250.0
c     deltay = 250.0
c     xmn = 156125.00
c     ymn = 1914125.00
c     dt = 10800.0
c     iyear_init = 2010
c     imonth_init = 9
c     iday_init = 1
c     undef = -9999.0
c     print_inc = 8.0

      open (61,file=
     &  '../../1_sden_assim/2_sden_assim/snowmodel_info.dat')

      read (61,*) nx
      read (61,*) ny
      read (61,*) deltax
      read (61,*) deltay
      read (61,*) xmn
      read (61,*) ymn
      read (61,*) dt
      read (61,*) iyear_init
      read (61,*) imonth_init
      read (61,*) iday_init
      read (61,*) max_iter
      read (61,*) undef
      read (61,*) print_inc

c Run the snow-density data assimilation routines.
      call DENSITY_ASSIM_USER(nx,ny,deltax,deltay,xmn,ymn,
     &  iyear_init,imonth_init,iday_init,print_inc,dt,
     &  undef,input_path(1:i_len_input),output_path(1:i_len_output),
     &  nstns_max,beta,nx_max,ny_max,nyears)

c Make the corresponding .ctl files.  This is assuming daily data
c   starting on 1 Sep, and once a year assimilations.
      ndays = max_iter / nint(print_inc)
      call make_snod_ctl_file (nx,ny,ndays,iyear_init,
     &  output_path(1:i_len_output))

      call make_snod_ratio_ctl_file (nx,ny,nyears,iyear_init+1,
     &  output_path(1:i_len_output))

      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine DENSITY_ASSIM_USER(nx,ny,deltax,deltay,xmn,ymn,
     &  iyear_init,imonth_init,iday_init,print_inc,dt,
     &  undef,input_path,output_path,
     &  nstns_max,beta,nx_max,ny_max,nyears)

c This program takes observations as discrete points, compares
c   those observations with a gridded model representation of those
c   observations, at the corresponding grid cells, computes a
c   difference between the observations and modeled values, fits
c   a gridded surface through those differences, and adds the
c   difference grid to the model grid.  Thus correcting the model
c   outputs with the observations.

c The input (density) data file is assumed to be in /snowmodel/data/
c   and it is called: snow_density_data.dat
c   So: /snowmodel/data/snow_density_data.dat

c A single data file describes the observation information that
c   will be used in the data assimilation.  This file contains the
c   following information in the following format.  The id can be
c   any number, x and y are easting and northing in m, and density
c   is in kg/m^3.

c   iyr imo idy (for this observation date)
c   number_of_stations_for_this_observation_date
c   id x y density
c   id x y density
c   id x y density

c For example:

c   2014 4 15
c   3
c   101 3456.7 23677.4 340.2
c   102 3556.3 25079.3 324.1
c   103 3106.2 29089.3 331.7

c The above is not exactly correct.  See /sm/code/dataassim_user.f
c   for additional details of the correct format of the density
c   observation input file.  For example, this file now requires
c   the number of simulation years to be included at the top of
c   the file.

c It also assumes the input and output files are daily files.

      implicit none

      integer nx,ny,k,iiyr,iimo,iidy,iyear_init,imonth_init,
     &  iday_init,id,i,j,n_den_pts,iobs_rec,nyear,nyears,
     &  ntimes,irec,irec_start,irec_end,nstns_max,nx_max,ny_max

      real deltax,deltay,undef,dt,print_inc,beta,var_model

      real snod_obs(nstns_max)
      real snod_obs2(nstns_max)
      integer ii,jj,n_pts

      real snod(nx_max,ny_max)
      real ratio_snod(nx_max,ny_max)

      double precision xmn,ymn
      double precision xstn(nstns_max),ystn(nstns_max)
      double precision xstn2(nstns_max),ystn2(nstns_max)

      character input_path*(*)
      character output_path*(*)

c Open the snod output file from the SnowModel run.
      open (41,file=input_path//'snod.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Open the output files.
      open (31,file=output_path//'snod_ratio.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      open (32,file=output_path//'snod_assim.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

c Open the snod observation data file.
      open (unit=21,file=
     &  '../1_mk_snod_assim_file/9_mk_snod_assim_file/snod_obs.dat')

c Read in the number of years this assimilation will process.
      read(21,*) nyears

c LOOP THROUGH THE YEARS IN THE SIMULATION.
      do nyear=1,nyears

c Read the data describing the time, location, and variable values
c   for each station, at this time step.
        read (21,*) ntimes
        if (ntimes.gt.1) then
          print *, 'ntimes cannot be > 1 in this implementation'
          stop
        endif
        read (21,*) iiyr,iimo,iidy
        read (21,*) n_den_pts

        if (n_den_pts.gt.nstns_max) then
          print *, 'n_den_pts in cannot be greater than nstns_max.'
          print *, 'You must increase nstns_max before you can'
          print *, 'continue.'
          stop
        endif

        do k=1,n_den_pts
          read (21,*) id,xstn(k),ystn(k),snod_obs(k)
        enddo

c Use the date stamp to calculate the write record for these
c   density observations.  Note that this one deals with 3-hourly
c   outputs.
        call get_obs_record(iday_init,imonth_init,iyear_init,
     &    iidy,iimo,iiyr,iobs_rec,print_inc,dt)

c Extract the modeled snod at that data write.
        read (41,rec=iobs_rec) ((snod(i,j),i=1,nx),j=1,ny)

c RUN A QUICK CHECK AND ELIMINATE ANY OBSERVED GRID POINTS THAT
c   HAVE ZERO (OR NEAR ZERO) MODELED VALUES.  THIS IS REQUIRED
c   BECAUSE IN WHAT FOLLOWS THERE IS A DIVIDE BY THE MODELED
c   VALUE.
      n_pts = 0
      do k=1,n_den_pts
        ii = 1 + nint((xstn(k) - xmn) / deltax)
        jj = 1 + nint((ystn(k) - ymn) / deltay)
        var_model = snod(ii,jj)
        if (var_model.gt.0.005) then
          n_pts = n_pts + 1
          xstn2(n_pts) = xstn(k)
          ystn2(n_pts) = ystn(k)
          snod_obs2(n_pts) = snod_obs(k)
        endif
      enddo

c Take the observed snow density, and the modeled snow density at
c   the same grid points and at the same times, and calculate the
c   differences between the two values.  Fit a correction surface
c   through these differences, covering the entire domain.  Then
c   adjust the original values by this offset (so the entire
c   surface is moved up or down depending on the calculated
c   differences.
        if (n_pts.gt.0) then
          call DENSITY_DATA_ASSIM(nx,ny,deltax,deltay,xmn,ymn,
     &      xstn2,ystn2,n_pts,snod_obs2,ratio_snod,snod,beta,
     &      nstns_max,nx_max,ny_max)
        else
          print *, 'You are in DENSITY ASSIM, and there are no'
          print *, 'density data available!'
          stop
        endif

c Save the density correction surface.
        write (31,rec=nyear) ((ratio_snod(i,j),i=1,nx),j=1,ny)

c Calculate the starting and ending records for this year.  Note
c   that this one assumed daily outputs.
        call get_irecs(iyear_init,imonth_init,iday_init,
     &    nyear,irec_start,irec_end)

c Use this ratio_snod array to correct all of the snow density
c   values in this years' SnowModel simulation.  Loop through
c   each record in this year.
        do irec=irec_start,irec_end

c Read in the original data arrays at this time step.
          read (41,rec=irec) ((snod(i,j),i=1,nx),j=1,ny)

c Perform the corrections.  Make sure no unreasonably small or
c   large densities have been produced.
          do j=1,ny
            do i=1,nx
              if (snod(i,j).ne.undef) then
                snod(i,j) = snod(i,j) * ratio_snod(i,j)
                snod(i,j) = max(0.0,snod(i,j))
              endif
            enddo
          enddo

c Save the data.
          write (32,rec=irec) ((snod(i,j),i=1,nx),j=1,ny)

        enddo

      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine DENSITY_DATA_ASSIM(nx,ny,deltax,deltay,xmn,ymn,
     &  xstn,ystn,nstns,var_obs,ratio_var_grid,var_grid,beta,
     &  nstns_max,nx_max,ny_max)

      implicit none

      integer nx,ny,ifill,iobsint,k,nstns,nstns_max,nx_max,ny_max

      real deltax,deltay,undef,dn,beta
      real var_grid(nx_max,ny_max),ratio_var_grid(nx_max,ny_max)
      real var_model(nstns_max),var_obs(nstns_max),
     &  ratio_var(nstns_max)

      double precision xmn,ymn
      double precision xstn(nstns_max),ystn(nstns_max)

      integer ii(nstns_max),jj(nstns_max)

c Convert the x and y locations to (ii,jj) locations.
      do k=1,nstns
        ii(k) = 1 + nint((xstn(k) - xmn) / deltax)
        jj(k) = 1 + nint((ystn(k) - ymn) / deltay)
      enddo

c Extract the modeled data at the appropriate grid cells.
      do k=1,nstns
        var_model(k) = var_grid(ii(k),jj(k))
      enddo

c Calculate the difference between the modeled variable and the
c   observation at each point/grid cell.
      do k=1,nstns

        ratio_var(k) = var_obs(k) / var_model(k)

c Impose some limits on the calculated corrections.
        ratio_var(k) = min(5.0,ratio_var(k))
        ratio_var(k) = max(0.25,ratio_var(k))

      enddo

c Save a text record of the density assimilation information.
      open (51,file='snod_data_output_notes.txt')
      write (51,*) '***********************************************'
      do k=1,nstns
        write (51,*) 'station =',k
        write (51,*) 'model =',var_model(k),'observations =',var_obs(k)
        write (51,99) ratio_var(k)
      write (51,*) '***********************************************'
      enddo
  99  format ('adjustment(fraction) =',f10.6)

c Now that I have the differences calculated at each observation
c   point, interpolate them over the simulation domain.  Use the
c   barnes oi scheme to create the distribution. If there is
c   only a single station, distribute those data uniformly over
c   the domain.  Make sure that ifill=1, and then undef is not
c   really used (so it does not have to be the same as defined in
c   the .par file).
      undef = -9999.0
      ifill = 1
      iobsint = 0

      if (nstns.ge.2) then
        call get_dn(nx,ny,deltax,deltay,nstns,dn,iobsint)
        dn = beta * dn
        call barnes_oi(nx,ny,deltax,deltay,xmn,ymn,nstns_max,
     &    nstns,xstn,ystn,ratio_var,dn,ratio_var_grid,undef,ifill,
     &    nx_max,ny_max)

      elseif (nstns.eq.1) then
        call single_stn(nx,ny,nstns,ratio_var,ratio_var_grid,nstns_max,
     &    nx_max,ny_max)
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_obs_record(iday_init,imonth_init,iyear_init,
     &  iidy,iimo,iiyr,iobs_rec_tmp,print_inc,dt)

      implicit none

      integer ioptn,iday_init,imonth_init,iyear_init,julian_start,
     &  iidy,iimo,iiyr,julian_end,iobs_rec_tmp,n_writes_per_day

      real print_inc,dt

c Find the Julian day at the start of the model run.
      ioptn = 3
      call calndr (ioptn,iday_init,imonth_init,iyear_init,julian_start)

c Find the Julian day for this data record.
      call calndr (ioptn,iidy,iimo,iiyr,julian_end)

c Calculate the day of simulation for this data record.  This is the
c   same as the output file data record.
      iobs_rec_tmp = julian_end - julian_start + 1

c Correct this for the case where sub-daily data writes were made.
      n_writes_per_day = nint((86400.0 / dt) / print_inc)
      iobs_rec_tmp = n_writes_per_day * iobs_rec_tmp

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_irecs(iyear_init,imonth_init,iday_init,
     &  nyear,irec_start,irec_end)

c Calculate the starting and ending record for this year.

      implicit none

      integer iyear_init,imonth_init,iday_init,nyear,iyr_beg,
     &  iyr_end,irec_start,ioptn,julian_start,julian_beg,
     &  julian_end,irec_end

c 'start' is the begining of the simulation.
c 'beg' is the begining of the simulation year.
c 'end' is the end of the simulation year.
      iyr_beg = iyear_init + nyear - 1
      iyr_end = iyear_init + nyear

      ioptn = 3
      call calndr (ioptn,iday_init,imonth_init,iyear_init,julian_start)
      call calndr (ioptn,iday_init,imonth_init,iyr_beg,julian_beg)
      call calndr (ioptn,iday_init,imonth_init,iyr_end,julian_end)

c The end day is one less than this start of the next year.
      julian_end = julian_end - 1

      irec_start = julian_beg - julian_start + 1
      irec_end = julian_end - julian_start + 1

      print *,nyear,iyr_beg,iyr_end,irec_start,irec_end

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_dn(nx,ny,deltax,deltay,nstns,dn,iobsint)

      implicit none

      integer nx,ny,nstns
      real deltax,deltay,dn
      real dn_max           ! the max obs spacing, dn_r
      real dn_min           ! dn_r, for large n
      integer iobsint       ! flag (=1) use dn value from .par file

c Calculate an appropriate filtered wavelength value.  First
c   calculate dn for the case of severely nonuniform data, and
c   then for the case where there is just about a station for
c   every grid cell.  Then assume that the average of these two
c   is a reasonable value to use in the interpolation.
        dn_max = sqrt(deltax*real(nx) * deltay*real(ny)) *
     &    ((1.0 + sqrt(real(nstns))) / (real(nstns) - 1.0))
        dn_min = sqrt((deltax*real(nx) * deltay*real(ny)) /
     &    real(nstns))

        if (iobsint.eq.1) then
c         dn = dn
        else
          dn = 0.5 * (dn_min + dn_max)
        endif

c       print *,'You are using an average obs spacing of',dn
c       print *,'  the program indicates a min, max range of',
c    &    dn_min,dn_max

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine barnes_oi(nx,ny,deltax,deltay,xmn,ymn,nstns_max,
     &  nstns,xstn,ystn,var,dn,grid,undef,ifill,
     &  nx_max,ny_max)

c This is an implementation of the Barnes objective analysis scheme
c   as described in:
c
c   Koch, S. E., M. DesJardins, and P. J. Kocin, 1983: An
c   interactive Barnes objective map analysis scheme for use with
c   satellite and conventional data. J. Climate and Applied
c   Meteorology, 22(9), 1487-1503.

      implicit none

      real gamma
      parameter (gamma=0.2)
      real pi

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn !center x coords of lower left grid cell
      double precision ymn !center y coords of lower left grid cell

      integer nstns_max,nx_max,ny_max
      integer nstns        ! number of input values, all good
      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords
      real var(nstns_max)  ! input values
      integer nflag        ! determines if output will be undef value
      real undef           ! undefined value

      real dn                  ! average observation spacing
      real grid(nx_max,ny_max) ! output values

      integer i,j      ! col, row counters
      integer mm,nn    ! station counters
      integer ifill    ! flag (=1) forces a value in every cell

      double precision xg,yg !temporary x and y coords of current cell
      real w1,w2       ! weights for Gauss-weighted average
      real wtot1,wtot2 ! sum of weights
      real ftot1,ftot2 ! accumulators for values, corrections
      real dsq         ! delx**2 + dely**2
      double precision xa,ya       ! x, y coords of current station
      double precision xb,yb       ! x, y coords of current station
      real dvar(nstns_max)   ! estimated error

      real xkappa_1    ! Gauss constant for first pass
      real xkappa_2    ! Gauss constant for second pass
      real rmax_1      ! maximum scanning radii, for first
      real rmax_2      ! and second passes
      real anum_1      ! numerator, beyond scanning radius,
      real anum_2      ! for first and second passes

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Compute the first and second pass values of the scaling parameter
c   and the maximum scanning radius used by the Barnes scheme.
c   Values above this maximum will use a 1/r**2 weighting.  Here I
c   have assumed a gamma value of 0.2.

c First-round values, Eqn (13).
      pi = 2.0 * acos(0.0)
      xkappa_1 = 5.052 * (2.0*dn/pi)**2

c Define the maximum scanning radius to have weight defined by
c   wt = 1.0 x 10**(-30) = exp(-rmax_1/xkappa_1)
c Also scale the 1/r**2 wts so that when dsq = rmax, the wts match.
      rmax_1 = xkappa_1 * 30.0 * log(10.0)
      anum_1 = 1.0e-30 * rmax_1

c Second-round values, Eqn (4).
      xkappa_2 = gamma * xkappa_1
      rmax_2 = rmax_1 * gamma
      anum_2 = 1.0e-30 * rmax_2

c Scan each input data point and construct estimated error, dvar, at
c   that point.
      do 222 nn=1,nstns

        xa = xstn(nn)
        ya = ystn(nn)
        wtot1 = 0.0
        ftot1 = 0.0

        do 111 mm=1,nstns

          xb = xstn(mm)
          yb = ystn(mm)
          dsq = real((xb - xa)**2 + (yb - ya)**2)

          if (dsq.le.rmax_1) then

            w1 = exp((- dsq)/xkappa_1)

          else

c Assume a 1/r**2 weight.
            w1 = anum_1/dsq

          endif

          wtot1 = wtot1 + w1
          ftot1 = ftot1 + w1 * var(mm)

  111   continue    ! end loop on sites m

        if (wtot1.eq.0.0) print *,'stn wt totals zero'

        dvar(nn) = var(nn) - ftot1/wtot1

  222 continue        ! end prediction loop on sites nn

c Grid-prediction loop.  Generate the estimate using first set of
c   weights, and correct using error estimates, dvar, and second
c   set of weights.

      do 666 j=1,ny
      do 555 i=1,nx

c xcoords of grid nodes at index i,j
c ycoords of grid nodes at index i,j
        xg = xmn + deltax * (real(i) - 1.0)
        yg = ymn + deltay * (real(j) - 1.0)

c Scan each input data point.
        ftot1 = 0.0
        wtot1 = 0.0
        ftot2 = 0.0
        wtot2 = 0.0
        nflag = 0

        do 333 nn=1,nstns
           
          xa = xstn(nn)
          ya = ystn(nn)
          dsq = real((xg - xa)**2 + (yg - ya)**2)

          if (dsq.le.rmax_2) then

            w1 = exp((- dsq)/xkappa_1)
            w2 = exp((- dsq)/xkappa_2)

          elseif (dsq.le.rmax_1) then

            w1 = exp((- dsq)/xkappa_1)
            w2 = anum_2/dsq

          else

c Assume a 1/r**2 weight.
            w1 = anum_1/dsq
            nflag = nflag + 1
c With anum_2/dsq.
            w2 = gamma * w1

          endif

          wtot1 = wtot1 + w1
          wtot2 = wtot2 + w2
          ftot1 = ftot1 + w1 * var(nn)
          ftot2 = ftot2 + w2 * dvar(nn)
           
  333   continue    ! end loop on data sites nn

        if (wtot1.eq.0.0 .or. wtot2.eq.0.0) print *,'wts total zero'

        if (ifill.eq.1) then
          grid(i,j) = ftot1/wtot1 + ftot2/wtot2
        else
          if (nflag.lt.nstns) then
            grid(i,j) = ftot1/wtot1 + ftot2/wtot2
          else
            grid(i,j) = undef
          endif
        endif

  555 continue         ! end loop on cols i
  666 continue         ! end loop on rows j

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine single_stn(nx,ny,nstns,var,grid,nstns_max,
     &  nx_max,ny_max)

      implicit none

      integer nstns_max,nx_max,ny_max
      integer nstns    ! number of input values, all good
      integer nx       ! number of x output values
      integer ny       ! number of y output values

      real var(nstns_max)      ! input values
      real grid(nx_max,ny_max) ! output values
      integer i,j              ! col, row counters

c Assign the station value to every grid cell.
      do j=1,ny
        do i=1,nx
          grid(i,j) = var(nstns)
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_snod_ctl_file (nx,ny,ndays,iyr_start,
     &  path)

      implicit none

      integer nx,ny,ndays,iyr_start,nvars,nvar

      parameter (nvars=1)

      character*60 var_string(nvars)
      character*(*) path

c Define the variable names that correspond to the .gdat file
c   writes.  I list them in a column here so it better corresponds
c   to the .gdat writes listed below; these must be listed in the
c   same order as the data writes, and they must be in single
c   quotes, and they cannot be more than 15 characters long.
      data var_string /
     &  'snod     0  0 snod depth (m)'/

      open (71,file='snod_assim.ctl')

      write (71,51) path
      write (71,52)
      write (71,53)
      write (71,54) nx
      write (71,55) ny

      write (71,56)
      write (71,57) ndays,iyr_start
      write (71,58) nvars

      do nvar=1,nvars
        write (71,100) var_string(nvar)
      enddo

      write (71,68)

      close (71)

   51 format ('DSET ',a,'snod_assim.gdat')
   52 format ('TITLE xxxxxxxxxxxxxxxxxxxxxxxxxx')
   53 format ('UNDEF -9999.0')

   54 format ('XDEF ',i8,' LINEAR  1.0  1.0')
   55 format ('YDEF ',i8,' LINEAR  1.0  1.0')
   56 format ('ZDEF        1 LINEAR  1.0  1.0')

   57 format ('TDEF ',i8,' LINEAR  01SEP',i4,' 1dy')

   58 format ('VARS     ',i4)

  100 format (a)

   68 format ('ENDVARS')

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_snod_ratio_ctl_file (nx,ny,nyears,iyr_start,
     &  path)

      implicit none

      integer nx,ny,nyears,iyr_start,nvars,nvar

      parameter (nvars=1)

      character*60 var_string(nvars)
      character*(*) path

c Define the variable names that correspond to the .gdat file
c   writes.  I list them in a column here so it better corresponds
c   to the .gdat writes listed below; these must be listed in the
c   same order as the data writes, and they must be in single
c   quotes, and they cannot be more than 15 characters long.
      data var_string /
     &  'snod_ratio     0  0 snod depth (m)'/

      open (71,file='snod_ratio.ctl')

      write (71,51) path
      write (71,52)
      write (71,53)
      write (71,54) nx
      write (71,55) ny

      write (71,56)
      write (71,57) nyears,iyr_start
      write (71,58) nvars

      do nvar=1,nvars
        write (71,100) var_string(nvar)
      enddo

      write (71,68)

      close (71)

   51 format ('DSET ',a,'snod_ratio.gdat')
   52 format ('TITLE xxxxxxxxxxxxxxxxxxxxxxxxxx')
   53 format ('UNDEF -9999.0')

   54 format ('XDEF ',i8,' LINEAR  1.0  1.0')
   55 format ('YDEF ',i8,' LINEAR  1.0  1.0')
   56 format ('ZDEF        1 LINEAR  1.0  1.0')

   57 format ('TDEF ',i8,' LINEAR  01JAN',i4,' 1yr')

   58 format ('VARS     ',i4)

  100 format (a)

   68 format ('ENDVARS')

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer function trailing_blanks(input_string)

      implicit none

      integer k
      character*(*) input_string

c Count the number of blanks following the last non-blank
c   character.
      trailing_blanks = 0
      do k=len(input_string),1,-1
        if (input_string(k:k).eq.' ') then
          trailing_blanks = trailing_blanks + 1
        else
          return
        endif
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c file = calendar.f  version 1.0
c
c Note from Glen: This is the version that should be used as
c   part of my computer programs!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c This program performs various date conversion calculations
c using subroutine calndr().
c On a 32-bit computer, it can handle any date between roughly
c 5 million BC and 5 million AD.  This limitation is due to
c the range of integers that can be expressed with 32 bits.
c The algorithm has no limitation.
c
c Using function idaywk(), the day of the week is computed
c along with the answer to the user's calendar calculation.
c
c External routines called:
c calndr  calendar conversions
c idaywk  day of the week determination
c
c Portability
c This routine is coded to Fortran 77 standards except that
c lower case is used.
c
c Copyright (C) 1999 Jon Ahlquist.
c Issued under the second GNU General Public License.
c See www.gnu.org for details.
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
c If you find any errors, please notify:
c Jon Ahlquist <ahlquist@met.fsu.edu>   
c Dept of Meteorology
c Florida State University
c Tallahassee, FL 32306-4520
c 15 March 1999.
c
c----------
c
c Declare variables.
c     implicit     none
c     integer      day, daynum, ioptn, julian, month, year
c     character*9  daynam(0:6)
c
c Declare the integer function used to compute the day of the week.
c     integer  idaywk
c
c Define the day names.
c     data  daynam /'Sunday',   'Monday', 'Tuesday', 'Wednesday',
c    &              'Thursday', 'Friday', 'Saturday'/
c
c Variables and their meanings
c day     day of the month.
c daynam  array of day names.  (daynam(0)='Sunday', daynam(1)='Monday',
c            ..., daynam(6)='Saturday')
c daynum  day number during the year.  (1 for 1 January, 2 for
c            2 January, 32 for 1 February, etc.)
c idaywk  integer function that returns an integer counter indicating
c            the day of the week, where 0 refers to Sunday, 1 to Monday,
c            up to 6 for Saturday.
c ioptn   option indicator where 0 < abs(ioptn) < 6.
c            See below and especially subroutine calndr for details.
c julian  Julian Day number.
c month   month counter (1=January, 2=February, ..., 12=December)
c year    year expressed with ALL digits.  DO NOT abbreviate years
c            by using only the last two digits.
c
c----------
c
      subroutine calndr (ioptn, iday, month, iyear, idayct)
c
c----------
c
c CALNDR = CALeNDaR conversions, version 1.0
c
c Input variable specifying the desired calendar conversion option.
      integer ioptn
c
c Input/Output variables (sometimes input, sometimes output,
c depending on the value of the desired option, ioptn.)
      integer  iday, month, iyear, idayct
c
c----------
c
c Subroutine calndr() performs calendar calculations using either
c the standard Gregorian calendar or the old Julian calendar.
c This subroutine extends the definitions of these calendar systems
c to any arbitrary year.  The algorithms in this subroutine
c will work with any date in the past or future,
c but overflows will occur if the numbers are sufficiently large.
c For a computer using a 32-bit integer, this routine can handle
c any date between roughly 5.8 million BC and 5.8 million AD
c without experiencing overflow during calculations.
c
c No external functions or subroutines are called.
c
c----------
c
c INPUT/OUTPUT ARGUMENTS FOR SUBROUTINE CALNDR()
c
c "ioptn" is the desired calendar conversion option explained below.
c Positive option values use the standard modern Gregorian calendar.
c Negative option values use the old Julian calendar which was the
c standard in Europe from its institution by Julius Caesar in 45 BC
c until at least 4 October 1582.  The Gregorian and Julian calendars
c are explained further below.
c
c (iday,month,iyear) is a calendar date where "iday" is the day of
c the month, "month" is 1 for January, 2 for February, etc.,
c and "iyear" is the year.  If the year is 1968 AD, enter iyear=1968,
c since iyear=68 would refer to 68 AD.
c For BC years, iyear should be negative, so 45 BC would be iyear=-45.
c By convention, there is no year 0 under the BC/AD year numbering
c scheme.  That is, years proceed as 2 BC, 1 BC, 1 AD, 2 AD, etc.,
c without including 0.  Subroutine calndr() will print an error message
c and stop if you specify iyear=0.
c
c "idayct" is a day count.  It is either the day number during the
c specified year or the Julian Day number, depending on the value
c of ioptn.  By day number during the specified year, we mean
c idayct=1 on 1 January, idayct=32 on 1 February, etc., to idayct=365
c or 366 on 31 December, depending on whether the specified year
c is a leap year.
c
c The values of input variables are not changed by this subroutine.
c
c
c ALLOWABLE VALUES FOR "IOPTN" and the conversions they invoke.
c Positive option values ( 1 to  5) use the standard Gregorian calendar.
c Negative option values (-1 to -5) use the old      Julian    calendar.
c
c Absolute
c  value
c of ioptn   Input variable(s)     Output variable(s)
c
c    1       iday,month,iyear      idayct
c Given a calendar date (iday,month,iyear), compute the day number
c (idayct) during the year, where 1 January is day number 1 and
c 31 December is day number 365 or 366, depending on whether it is
c a leap year.
c
c    2       idayct,iyear          iday,month
c Given the day number of the year (idayct) and the year (iyear),
c compute the day of the month (iday) and the month (month).
c
c    3       iday,month,iyear      idayct
c Given a calendar date (iday,month,iyear), compute the Julian Day
c number (idayct) that starts at noon of the calendar date specified.
c
c    4       idayct                iday,month,iyear
c Given the Julian Day number (idayct) that starts at noon,
c compute the corresponding calendar date (iday,month,iyear).
c
c    5       idayct                iday,month,iyear
c Given the Julian Day number (idayct) that starts at noon,
c compute the corresponding day number for the year (iday)
c and year (iyear).  On return from calndr(), "month" will always
c be set equal to 1 when ioptn=5.
c
c No inverse function is needed for ioptn=5 because it is
c available through option 3.  One simply calls calndr() with:
c ioptn = 3,
c iday  = day number of the year instead of day of the month,
c month = 1, and
c iyear = whatever the desired year is.
c
c----------
c
c EXAMPLES
c The first 6 examples are for the standard Gregorian calendar.
c All the examples deal with 15 October 1582, which was the first day
c of the Gregorian calendar.  15 October is the 288-th day of the year.
c Julian Day number 2299161 began at noon on 15 October 1582.
c
c Find the day number during the year on 15 October 1582
c     ioptn = 1
c     call calndr (ioptn, 15, 10, 1582,  idayct)
c calndr() should return idayct=288
c
c Find the day of the month and month for day 288 in year 1582.
c     ioptn = 2
c     call calndr (ioptn, iday, month, 1582, 288)
c calndr() should return iday=15 and month=10.
c
c Find the Julian Day number for 15 October 1582.
c     ioptn = 3
c     call calndr (ioptn, 15, 10, 1582, julian)
c calndr() should return julian=2299161
c
c Find the Julian Day number for day 288 during 1582 AD.
c When the input is day number of the year, one should specify month=1
c     ioptn = 3
c     call calndr (ioptn, 288, 1, 1582, julian)
c calndr() should return dayct=2299161
c
c Find the date for Julian Day number 2299161.
c     ioptn = 4
c     call calndr (ioptn, iday, month, iyear, 2299161)
c calndr() should return iday=15, month=10, and iyear=1582
c 
c Find the day number during the year (iday) and year
c for Julian Day number 2299161.
c     ioptn = 5
c     call calndr (ioptn, iday, month, iyear, 2299161)
c calndr() should return iday=288, month=1, iyear=1582
c
c Given 15 October 1582 under the Gregorian calendar,
c find the date (idayJ,imonthJ,iyearJ) under the Julian calendar.
c To do this, we call calndr() twice, using the Julian Day number
c as the intermediate value.
c     call calndr ( 3, 15,        10, 1582,    julian)
c     call calndr (-4, idayJ, monthJ, iyearJ,  julian)
c The first call to calndr() should return julian=2299161, and
c the second should return idayJ=5, monthJ=10, iyearJ=1582
c
c----------
c
c BASIC CALENDAR INFORMATION
c
c The Julian calendar was instituted by Julius Caesar in 45 BC.
c Every fourth year is a leap year in which February has 29 days.
c That is, the Julian calendar assumes that the year is exactly
c 365.25 days long.  Actually, the year is not quite this long.
c The modern Gregorian calendar remedies this by omitting leap years
c in years divisible by 100 except when the year is divisible by 400.
c Thus, 1700, 1800, and 1900 are leap years under the Julian calendar
c but not under the Gregorian calendar.  The years 1600 and 2000 are
c leap years under both the Julian and the Gregorian calendars.
c Other years divisible by 4 are leap years under both calendars,
c such as 1992, 1996, 2004, 2008, 2012, etc.  For BC years, we recall
c that year 0 was omitted, so 1 BC, 5 BC, 9 BC, 13 BC, etc., and 401 BC,
c 801 BC, 1201 BC, etc., are leap years under both calendars, while
c 101 BC, 201 BC, 301 BC, 501 BC, 601 BC, 701 BC, 901 BC, 1001 BC,
c 1101 BC, etc., are leap years under the Julian calendar but not
c the Gregorian calendar.
c
c The Gregorian calendar is named after Pope Gregory XIII.  He declared
c that the last day of the old Julian calendar would be Thursday,
c 4 October 1582 and that the following day, Friday, would be reckoned
c under the new calendar as 15 October 1582.  The jump of 10 days was
c included to make 21 March closer to the spring equinox.
c
c Only a few Catholic countries (Italy, Poland, Portugal, and Spain)
c switched to the Gregorian calendar on the day after 4 October 1582.
c It took other countries months to centuries to change to the
c Gregorian calendar.  For example, England's first day under the
c Gregorian calendar was 14 September 1752.  The same date applied to
c the entire British empire, including America.  Japan, Russia, and many
c eastern European countries did not change to the Gregorian calendar
c until the 20th century.  The last country to change was Turkey,
c which began using the Gregorian calendar on 1 January 1927.
c
c Therefore, between the years 1582 and 1926 AD, you must know
c the country in which an event was dated to interpret the date
c correctly.  In Sweden, there was even a year (1712) when February
c had 30 days.  Consult a book on calendars for more details
c about when various countries changed their calendars.
c
c DAY NUMBER DURING THE YEAR
c The day number during the year is simply a counter equal to 1 on
c 1 January, 32 on 1 February, etc., thorugh 365 or 366 on 31 December,
c depending on whether the year is a leap year.  Sometimes this is
c called the Julian Day, but that term is better reserved for the
c day counter explained below.
c
c JULIAN DAY NUMBER
c The Julian Day numbering system was designed by Joseph Scaliger
c in 1582 to remove ambiguity caused by varying calendar systems.
c The name "Julian Day" was chosen to honor Scaliger's father,
c Julius Caesar Scaliger (1484-1558), an Italian scholar and physician
c who lived in France.  Because Julian Day numbering was especially
c designed for astronomers, Julian Days begin at noon so that the day
c counter does not change in the middle of an astronmer's observing
c period.  Julian Day 0 began at noon on 1 January 4713 BC under the
c Julian calendar.  A modern reference point is that 23 May 1968
c (Gregorian calendar) was Julian Day 2,440,000.
c
c JULIAN DAY NUMBER EXAMPLES
c
c The table below shows a few Julian Day numbers and their corresponding
c dates, depending on which calendar is used.  A negative 'iyear' refers
c to BC (Before Christ).
c
c                     Julian Day under calendar:
c iday  month   iyear     Gregorian   Julian
c  24     11   -4714            0        -38
c   1      1   -4713           38          0
c   1      1       1      1721426    1721424
c   4     10    1582      2299150    2299160
c  15     10    1582      2299161    2299171
c   1      3    1600      2305508    2305518
c  23      5    1968      2440000    2440013
c   5      7    1998      2451000    2451013
c   1      3    2000      2451605    2451618
c   1      1    2001      2451911    2451924
c
c From this table, we can see that the 10 day difference between the
c two calendars in 1582 grew to 13 days by 1 March 1900, since 1900 was
c a leap year under the Julian calendar but not under the Gregorian
c calendar.  The gap will widen to 14 days after 1 March 2100 for the
c same reason.
c 
c----------
c
c PORTABILITY
c
c This subroutine is written in standard FORTRAN 77.
c It calls no external functions or subroutines and should run
c without problem on any computer having a 32-bit word or longer.
c 
c----------
c
c ALGORITHM
c
c The goal in coding calndr() was clear, clean code, not efficiency.
c Calendar calculations usually take a trivial fraction of the time
c in any program in which dates conversions are involved.
c Data analysis usually takes the most time.
c
c Standard algorithms are followed in this subroutine.  Internal to
c this subroutine, we use a year counter "jyear" such that
c  jyear=iyear   when iyear is positive
c       =iyear+1 when iyear is negative.
c Thus, jyear does not experience a 1 year jump like iyear does
c when going from BC to AD.  Specifically, jyear=0 when iyear=-1,
c i.e., when the year is 1 BC.
c
c For simplicity in dealing with February, inside this subroutine,
c we let the year begin on 1 March so that the adjustable month,
c February is the last month of the year.
c It is clear that the calendar used to work this way because the
c months September, October, November, and December refer to
c 7, 8, 9, and 10.  For consistency, jyear is incremented on 1 March
c rather than on 1 January.  Of course, everything is adjusted back to
c standard practice of years beginning on 1 January before answers
c are returned to the routine that calls calndr().
c
c Lastly, we use a trick to calculate the number of days from 1 March
c until the end of the month that precedes the specified month.
c That number of days is int(30.6001*(month+1))-122,
c where 30.6001 is used to avoid the possibility of round-off and
c truncation error.  For example, if 30.6 were used instead,
c 30.6*5 should be 153, but round-off error could make it 152.99999,
c which would then truncated to 152, causing an error of 1 day.
c
c Algorithm reference:
c Dershowitz, Nachum and Edward M. Reingold, 1990: Calendrical
c Calculations.  Software-Practice and Experience, vol. 20, number 9
c (September 1990), pp. 899-928.
c
c Copyright (C) 1999 Jon Ahlquist.
c Issued under the second GNU General Public License.
c See www.gnu.org for details.
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
c If you find any errors, please notify:
c Jon Ahlquist <ahlquist@met.fsu.edu>   
c Dept of Meteorology
c Florida State University
c Tallahassee, FL 32306-4520
c 15 March 1999.
c
c-----
c Declare internal variables.
      integer  jdref,  jmonth, jyear, leap,
     &         n1yr, n4yr, n100yr, n400yr,
     &         ndays, ndy400, ndy100, nyrs,
     &         yr400, yrref
c
c Explanation of all internal variables.
c jdref   Julian Day on which 1 March begins in the reference year.
c jmonth  Month counter which equals month+1 if month .gt. 2
c          or month+13 if month .le. 2.
c jyear   Year index,  jyear=iyear if iyear .gt. 0, jyear=iyear+1
c            if iyear .lt. 0.  Thus, jyear does not skip year 0
c            like iyear does between BC and AD years.
c leap    =1 if the year is a leap year, =0 if not.
c n1yr    Number of complete individual years between iyear and
c            the reference year after all 4, 100,
c            and 400 year periods have been removed.
c n4yr    Number of complete 4 year cycles between iyear and
c            the reference year after all 100 and 400 year periods
c            have been removed.
c n100yr  Number of complete 100 year periods between iyear and
c            the reference year after all 400 year periods
c            have been removed.
c n400yr  Number of complete 400 year periods between iyear and
c            the reference year.
c ndays   Number of days since 1 March during iyear.  (In intermediate
c            steps, it holds other day counts as well.)
c ndy400  Number of days in 400 years.  Under the Gregorian calendar,
c            this is 400*365 + 100 - 3 = 146097.  Under the Julian
c            calendar, this is 400*365 + 100 = 146100.
c ndy100  Number of days in 100 years,  Under the Gregorian calendar,
c            this is 100*365 + 24 = 36524.   Under the Julian calendar,
c            this is 100*365 + 25 = 36525.
c nyrs    Number of years from the beginning of yr400
c              to the beginning of jyear.  (Used for option +/-3).
c yr400   The largest multiple of 400 years that is .le. jyear.
c
c
c----------------------------------------------------------------
c Do preparation work.
c
c Look for out-of-range option values.
      if ((ioptn .eq. 0) .or. (abs(ioptn) .ge. 6)) then
         write(*,*)'For calndr(), you specified ioptn = ', ioptn
         write(*,*)
     &   'Allowable values are 1 to 5 for the Gregorian calendar'
         write(*,*)
     &   'and -1 to -5 for the Julian calendar.'
         stop
      endif
c
c Options 1-3 have "iyear" as an input value.
c Internally, we use variable "jyear" that does not have a jump
c from -1 (for 1 BC) to +1 (for 1 AD).
      if (abs(ioptn) .le. 3) then
         if (iyear .gt. 0) then
            jyear = iyear
         elseif (iyear .eq. 0) then
            write(*,*)
     &      'For calndr(), you specified the nonexistent year 0'
            stop
         else
            jyear = iyear + 1
         endif
c
c        Set "leap" equal to 0 if "jyear" is not a leap year
c        and equal to 1 if it is a leap year.
         leap = 0
         if ((jyear/4)*4 .eq. jyear) then
            leap = 1
         endif
         if ((ioptn .gt. 0)               .and.
     &       ((jyear/100)*100 .eq. jyear) .and.
     &       ((jyear/400)*400 .ne. jyear)      ) then
               leap = 0
         endif
      endif
c
c Options 3-5 involve Julian Day numbers, which need a reference year
c and the Julian Days that began at noon on 1 March of the reference
c year under the Gregorian and Julian calendars.  Any year for which
c "jyear" is divisible by 400 can be used as a reference year.
c We chose 1600 AD as the reference year because it is the closest
c multiple of 400 to the institution of the Gregorian calendar, making
c it relatively easy to compute the Julian Day for 1 March 1600
c given that, on 15 October 1582 under the Gregorian calendar,
c the Julian Day was 2299161.  Similarly, we need to do the same
c calculation for the Julian calendar.  We can compute this Julian
c Day knwoing that on 4 October 1582 under the Julian calendar,
c the Julian Day number was 2299160.  The details of these calculations
c is next. 
c    From 15 October until 1 March, the number of days is the remainder
c of October plus the days in November, December, January, and February:
c 17+30+31+31+28 = 137, so 1 March 1583 under the Gregorian calendar
c was Julian Day 2,299,298.  Because of the 10 day jump ahead at the
c switch from the Julian calendar to the Gregorian calendar, 1 March
c 1583 under the Julian calendar was Julian Day 2,299,308.  Making use
c of the rules for the two calendar systems, 1 March 1600 was Julian
c Day 2,299,298 + (1600-1583)*365 + 5 (due to leap years) =
c 2,305,508 under the Gregorian calendar and day 2,305,518 under the
c Julian calendar.
c    We also set the number of days in 400 years and 100 years.
c For reference, 400 years is 146097 days under the Gregorian calendar
c and 146100 days under the Julian calendar.  100 years is 36524 days
c under the Gregorian calendar and 36525 days under the Julian calendar.
      if (abs(ioptn) .ge. 3) then
c
c        Julian calendar values.
         yrref  =    1600
         jdref  = 2305518
c               = Julian Day reference value for the day that begins
c                 at noon on 1 March of the reference year "yrref".
         ndy400 = 400*365 + 100
         ndy100 = 100*365 +  25
c
c        Adjust for Gregorian calendar values.
         if (ioptn .gt. 0) then
            jdref  = jdref  - 10
            ndy400 = ndy400 -  3
            ndy100 = ndy100 -  1
         endif
      endif
c
c----------------------------------------------------------------
c OPTIONS -1 and +1:
c Given a calendar date (iday,month,iyear), compute the day number
c of the year (idayct), where 1 January is day number 1 and 31 December
c is day number 365 or 366, depending on whether it is a leap year.
      if (abs(ioptn) .eq. 1) then
c
c     Compute the day number during the year.
      if (month .le. 2) then
         idayct = iday + (month-1)*31
      else
         idayct = iday + int(30.6001 * (month+1)) - 63 + leap
      endif
c
c----------------------------------------------------------------
c OPTIONS -2 and +2:
c Given the day number of the year (idayct) and the year (iyear),
c compute the day of the month (iday) and the month (month).
      elseif (abs(ioptn) .eq. 2) then
c
      if (idayct .lt. 60+leap) then
         month  = (idayct-1)/31
         iday   = idayct - month*31
         month  = month + 1
      else
         ndays  = idayct - (60+leap)
c               = number of days past 1 March of the current year.
         jmonth = (10*(ndays+31))/306 + 3
c               = month counter, =4 for March, =5 for April, etc.
         iday   = (ndays+123) - int(30.6001*jmonth) 
         month  = jmonth - 1
      endif
c
c----------------------------------------------------------------
c OPTIONS -3 and +3:
c Given a calendar date (iday,month,iyear), compute the Julian Day
c number (idayct) that starts at noon.
      elseif (abs(ioptn) .eq. 3) then
c
c     Shift to a system where the year starts on 1 March, so January
c     and February belong to the preceding year.
c     Define jmonth=4 for March, =5 for April, ..., =15 for February.
      if (month .le. 2) then
        jyear  = jyear -  1
        jmonth = month + 13
      else
        jmonth = month +  1
      endif
c
c     Find the closest multiple of 400 years that is .le. jyear.
      yr400 = (jyear/400)*400
c           = multiple of 400 years at or less than jyear.
      if (jyear .lt. yr400) then
         yr400 = yr400 - 400
      endif
c
      n400yr = (yr400 - yrref)/400
c            = number of 400-year periods from yrref to yr400.
      nyrs   = jyear - yr400
c            = number of years from the beginning of yr400
c              to the beginning of jyear.
c
c     Compute the Julian Day number.
      idayct = iday + int(30.6001*jmonth) - 123 + 365*nyrs + nyrs/4
     &       + jdref + n400yr*ndy400
c
c     If we are using the Gregorian calendar, we must not count
c     every 100-th year as a leap year.  nyrs is less than 400 years,
c     so we do not need to consider the leap year that would occur if
c     nyrs were divisible by 400, i.e., we do not add nyrs/400.
      if (ioptn .gt. 0) then
         idayct = idayct - nyrs/100
      endif
c
c----------------------------------------------------------------
c OPTIONS -5, -4, +4, and +5:
c Given the Julian Day number (idayct) that starts at noon,
c compute the corresponding calendar date (iday,month,iyear)
c (abs(ioptn)=4) or day number during the year (abs(ioptn)=5).
      else
c
c     Create a new reference date which begins on the nearest
c     400-year cycle less than or equal to the Julian Day for 1 March
c     in the year in which the given Julian Day number (idayct) occurs.
      ndays  = idayct - jdref
      n400yr = ndays / ndy400
c            = integral number of 400-year periods separating
c              idayct and the reference date, jdref.
      jdref  = jdref + n400yr*ndy400
      if (jdref .gt. idayct) then
         n400yr = n400yr - 1
         jdref  = jdref  - ndy400
      endif
c
      ndays  = idayct - jdref
c            = number from the reference date to idayct.
c
      n100yr = min(ndays/ndy100, 3)
c            = number of complete 100-year periods
c              from the reference year to the current year.
c              The min() function is necessary to avoid n100yr=4
c              on 29 February of the last year in the 400-year cycle.
c
      ndays  = ndays - n100yr*ndy100
c            = remainder after removing an integral number of
c              100-year periods.
c
      n4yr   = ndays / 1461
c            = number of complete 4-year periods in the current century.
c              4 years consists of 4*365 + 1 = 1461 days.
c
      ndays  = ndays - n4yr*1461
c            = remainder after removing an integral number
c              of 4-year periods.
c
      n1yr   = min(ndays/365, 3)
c            = number of complete years since the last leap year.
c              The min() function is necessary to avoid n1yr=4
c              when the date is 29 February on a leap year,
c              in which case ndays=1460, and 1460/365 = 4.
c
      ndays  = ndays - 365*n1yr
c            = number of days so far in the current year,
c              where ndays=0 on 1 March.
c
      iyear  = n1yr + 4*n4yr + 100*n100yr + 400*n400yr + yrref 
c            = year, as counted in the standard way,
c              but relative to 1 March.
c
c At this point, we need to separate ioptn=abs(4), which seeks a
c calendar date, and ioptn=abs(5), which seeks the day number during
c the year.  First compute the calendar date if desired (abs(ioptn)=4).
      if (abs(ioptn) .eq. 4) then
         jmonth = (10*(ndays+31))/306 + 3
c               = offset month counter.  jmonth=4 for March, =13 for
c                 December, =14 for January, =15 for February.
         iday   = (ndays+123) - int(30.6001*jmonth)
c               = day of the month, starting with 1 on the first day
c                 of the month.
c
c        Now adjust for the fact that the year actually begins
c        on 1 January.
         if (jmonth .le. 13) then
            month = jmonth - 1
         else
            month = jmonth - 13
            iyear = iyear + 1
         endif
c
c This code handles abs(ioptn)=5, finding the day number during the year.
      else
c        ioptn=5 always returns month=1, which we set now.
         month = 1
c
c        We need to determine whether this is a leap year.
         leap = 0
         if ((jyear/4)*4 .eq. jyear) then
            leap = 1
         endif
         if ((ioptn .gt. 0)               .and.
     &       ((jyear/100)*100 .eq. jyear) .and.
     &       ((jyear/400)*400 .ne. jyear)      ) then
               leap = 0
         endif
c
c        Now find the day number "iday".
c        ndays is the number of days since the most recent 1 March,
c        so ndays=0 on 1 March.
         if (ndays .le.305) then
            iday  = ndays + 60 + leap
         else
            iday  = ndays - 305
            iyear = iyear + 1
         endif
      endif
c
c     Adjust the year if it is .le. 0, and hence BC (Before Christ).
      if (iyear .le. 0) then
         iyear = iyear - 1
      endif
c
c End the code for the last option, ioptn.
      endif
c
      return
      end


      integer function idaywk(jdayno)
c
c IDAYWK = compute the DAY of the WeeK given the Julian Day number,
c          version 1.0.
c
c Input variable
      integer  jdayno
c jdayno = Julian Day number starting at noon of the day in question.
c
c Output variable:
c idaywk = day of the week, where 0=Sunday, 1=Monday, ..., 6=Saturday.
c
c----------
c Compute the day of the week given the Julian Day number.
c You can find the Julian Day number given (day,month,year)
c using subroutine calndr.f.
c Example: For the first day of the Gregorian calendar,
c 15 October 1582, compute the Julian day number (option 3 of
c subroutine calndr) and compute the day of the week.
c     call calndr (3, 15, 10, 1582, jdayno) 
c     write(*,*) jdayno, idaywk(jdayno)
c The numbers printed should be 2299161 and 5,
c where 6 refers to Friday.
c
c Copyright (C) 1999 Jon Ahlquist.
c Issued under the second GNU General Public License.
c See www.gnu.org for details.
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
c If you find any errors, please notify:
c Jon Ahlquist <ahlquist@met.fsu.edu>   
c Dept of Meteorology
c Florida State University
c Tallahassee, FL 32306-4520
c 15 March 1999.
c
c-----
c Declare internal variable.
c jdSun is the Julian Day number starting at noon on any Sunday.
c I arbitrarily chose the first Sunday after Julian Day 1,
c which is Julian Day 6.
      integer  jdSun
      data     jdSun /6/
      idaywk = mod(jdayno-jdSun,7)
c If jdayno-jdSun < 0, then we are taking the modulus of a negative
c number. Fortran's built-in mod function returns a negative value
c when the argument is negative.  In that case, we adjust the result
c to a positive value.
      if (idaywk .lt. 0) idaywk = idaywk + 7
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

