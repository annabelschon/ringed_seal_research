c mk_yearly_variables.f

c This code takes daily SnowModel outputs and creates different
c   kinds of yearly outputs.

c It requires the user to do a few things:

c   1) Define each subroutine that you want to do any calculations
c      with.

c   2) Define the daily input array(s) required to do the
c      calcuations.

c   3) Define the output variables that you want to save.

c   4) Count the output variables that you want to save.

c   5) Put all of the above information in the right places.

c As part of this process it will save all of the yearly outputs
c   to a .gdat file and it will create an associated .ctl file.

      implicit none

      integer nx,ny,nyear,nyears,ndays,nvars,i,j,iyr_start,
     &  imo_start,idy_start,irec_start,nx_max,ny_max,iyr_end

      parameter (nx_max=10000,ny_max=10000)

      parameter (nvars=13)

      real swed_max(nx_max,ny_max)
      real swed_max_dos(nx_max,ny_max)
      real swed_max_doy(nx_max,ny_max)

      real snow_onset_dos(nx_max,ny_max)
      real snow_onset_doy(nx_max,ny_max)
      real snow_free_dos(nx_max,ny_max)
      real snow_free_doy(nx_max,ny_max)
      real core_snow_days(nx_max,ny_max)

      real snow_first_dos(nx_max,ny_max)
      real snow_first_doy(nx_max,ny_max)
      real snow_last_dos(nx_max,ny_max)
      real snow_last_doy(nx_max,ny_max)
      real total_snow_days(nx_max,ny_max)

      real undef,doy_start_of_sim,doy_31Dec,dt,print_inc

c Define the path where the SnowModel output data are located.
      character path*(*) 
      parameter (path=
     & '/data2/glen/nna_acoustics/2024_update/3hrly_outputs/dal/')

      character*20 var_string(nvars)

c Define the variable names that correspond to the .gdat file
c   writes.  I list them in a column here so it better corresponds
c   to the .gdat writes listed below; these must be listed in the
c   same order as the data writes, and they must be in single
c   quotes, and they cannot be more than 15 characters long.
      data var_string /
     &  'swed_max',
     &  'swed_max_dos',
     &  'swed_max_doy',

     &  'snow_onset_dos',
     &  'snow_onset_doy',
     &  'snow_free_dos',
     &  'snow_free_doy',
     &  'core_snow_days',

     &  'snow_first_dos',
     &  'snow_first_doy',
     &  'snow_last_dos',
     &  'snow_last_doy',
     &  'total_snow_days'/

      undef = -9999.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Read in nx and ny from the topo-vege analysis.
      open (41,file=
     &  '../../topo_vege/NoAm_30m/SM_domain_config_info_OUTPUTS.dat')

      read (41,93) nx
      read (41,93) ny
      print *
      print *, nx,ny

  93  format (9x,i8)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Output file.
      open (61,file='yearly_variables.gdat',
     &  form='unformatted',access='direct',recl=4*nvars*nx*ny,
     &  status='replace')

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Define the simulation start date and the number of simulation
c   years.
      open (33,file='../../met/merra2/'//
     &  '4_maxiter_offset/start_end_dates_maxiter_ioffset.dat')

      read (33,91) iyr_start
      read (33,91) imo_start
      read (33,91) idy_start
      read (33,*)
      read (33,91) iyr_end

      nyears = iyr_end - iyr_start

      print *
      print *, iyr_start,iyr_end,nyears
      print *

   91 format (14x,i10)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c If the SnowModel run output 3-hourly data, then daily data
c   must be produced for input into the following subroutines
c   (they assume daily input data).  Define the information
c   required to make daily data out of 3-hourly data.
      dt = 10800.0
      print_inc = 1.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Process one year at a time.
      print *
      do nyear=1,nyears

        print *,'simulation year =',nyear
        print *

c Calculate the starting record and number of records in this year.
        call get_irecs (iyr_start,imo_start,idy_start,nyear,
     &    irec_start,ndays,doy_start_of_sim,doy_31Dec)

c Define the subroutines, input file name (i), and output
c   variables (o).

c You will also need to count the number of output variables you
c   want to save, and fill in the 'nvars' parameter above.  Note
c   that you don't have to save all of the variables that are
c   produced by the subroutines (for example, all of them in the
c   'snowcover' subroutine).

        call snowcover (nx,ny,path,irec_start,ndays,undef,
     &    doy_start_of_sim,doy_31Dec,nx_max,ny_max,dt,print_inc,
     i    'snod.gdat',
     o    snow_onset_dos,snow_onset_doy,snow_free_dos,
     o    snow_free_doy,core_snow_days,snow_first_dos,
     o    snow_first_doy,snow_last_dos,snow_last_doy,
     o    total_snow_days)

c This must be called after 'snowcover' because we are using
c   the 'core' dos values to define the period to look in for
c   the max swe and date of that max swe value.
        call find_max_date_core (nx,ny,path,irec_start,ndays,
     &    undef,doy_start_of_sim,doy_31Dec,snow_onset_dos,
     &    snow_free_dos,nx_max,ny_max,dt,print_inc,
     i    'swed.gdat',
     o    swed_max,swed_max_dos,swed_max_doy)

c Check to see if the max_swe_date is in the core snow period.
        do j=1,ny
          do i=1,nx
            if (swed_max_dos(i,j).lt.snow_onset_dos(i,j)) then
              print *, 'swed_max_dos < snow_onset_dos',i,j
            elseif (swed_max_dos(i,j).gt.snow_free_dos(i,j)) then
              print *, 'swed_max_dos > snow_free_dos',i,j
            endif
          enddo
        enddo

c Save the data.
        write (61,rec=nyear)
     &    ((swed_max(i,j),i=1,nx),j=1,ny),
     &    ((swed_max_dos(i,j),i=1,nx),j=1,ny),
     &    ((swed_max_doy(i,j),i=1,nx),j=1,ny),

     &    ((snow_onset_dos(i,j),i=1,nx),j=1,ny),
     &    ((snow_onset_doy(i,j),i=1,nx),j=1,ny),
     &    ((snow_free_dos(i,j),i=1,nx),j=1,ny),
     &    ((snow_free_doy(i,j),i=1,nx),j=1,ny),
     &    ((core_snow_days(i,j),i=1,nx),j=1,ny),

     &    ((snow_first_dos(i,j),i=1,nx),j=1,ny),
     &    ((snow_first_doy(i,j),i=1,nx),j=1,ny),
     &    ((snow_last_dos(i,j),i=1,nx),j=1,ny),
     &    ((snow_last_doy(i,j),i=1,nx),j=1,ny),
     &    ((total_snow_days(i,j),i=1,nx),j=1,ny)

      enddo

c Make the grads .ctl file.  Here it just defines the grid in
c   (i,j) units.  The 'years' correspond to the date of the
c   ending simulation year (so if it started in Sep 2004, the
c   year will be 2005).
      call make_yearly_ctl_file (nx,ny,nyears,nvars,iyr_start,
     &  var_string)

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine snowcover (nx,ny,path,irec_start,ndays,undef,
     &  doy_start_of_sim,doy_31Dec,nx_max,ny_max,dt,print_inc,
     i  fname,
     o  snow_onset_dos,snow_onset_doy,snow_free_dos,
     o  snow_free_doy,core_snow_days,snow_first_dos,
     o  snow_first_doy,snow_last_dos,snow_last_doy,
     o  total_snow_days)

      implicit none

      integer nx,ny,i,j,ndays,nday,irec,irec_start,ndays_max,
     &  nx_max,ny_max,length_path,trailing_blanks

      parameter (ndays_max=366)

      real snod_yr(nx_max,ny_max,ndays_max)

      real snow_onset_dos(nx_max,ny_max)
      real snow_onset_doy(nx_max,ny_max)
      real snow_free_dos(nx_max,ny_max)
      real snow_free_doy(nx_max,ny_max)
      real core_snow_days(nx_max,ny_max)

      real snow_first_dos(nx_max,ny_max)
      real snow_first_doy(nx_max,ny_max)
      real snow_last_dos(nx_max,ny_max)
      real snow_last_doy(nx_max,ny_max)
      real total_snow_days(nx_max,ny_max)

      real first_flag(nx_max,ny_max)
      real undef_mask(nx_max,ny_max)

      integer istart(ndays_max)
      integer iend(ndays_max)
      integer idays(ndays_max)

      real undef,snod_threshold,doy_start_of_sim,doy_31Dec,delta_days,
     &  dt,print_inc

      integer icount,iicount,iiicount,idays_old,n_3hrs_in_day

      character*(*) path
      character*(*) fname
      character*120 path_fname

      n_3hrs_in_day = 8

c Define the snow depth threshold.
c     snod_threshold = 0.0
      snod_threshold = 0.005

c These calculations require you to read in a full years' worth
c   of data before doing the calculations.

c Open the input array.
      length_path = 120 - trailing_blanks(path)
      path_fname = path(1:length_path)//fname

c     print *, path_fname

      open (21,file=path_fname,
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Read in the data.  For the 3-hourly data outputs, irec is
c   reading the record at the end of each day; 8, 16, 24, etc.
c   This works for snod and swed, but not for things like tair
c   that would need to be averaged, or prec that would need to
c   be summed.
      do nday=1,ndays
        if (dt.eq.10800.0 .and. print_inc.eq.1) then
          irec = (irec_start + nday - 1) * n_3hrs_in_day
        else
          irec = irec_start + nday - 1
        endif
c       print *, nday,irec
        read (21,rec=irec) ((snod_yr(i,j,nday),i=1,nx),j=1,ny)
      enddo
      close (21)

c Assume that any undef snow depths are ocean (because that is
c   what I typically do with my terrestrial SnowModel simulations),
c   and use this information to create an undef_mask that can be
c   used to mask out the ocean areas (I could also get there by
c   reading in the vege data array).
      do j=1,ny
        do i=1,nx
          if (snod_yr(i,j,1).eq.undef) then
            undef_mask(i,j) = undef
          else
            undef_mask(i,j) = 0.0
          endif
        enddo
      enddo

c Initialize the arrays.
      do j=1,ny
        do i=1,nx
          snow_onset_dos(i,j) = undef
          snow_free_dos(i,j) = undef
          snow_first_dos(i,j) = undef
          snow_last_dos(i,j) = undef

          snow_onset_doy(i,j) = undef
          snow_free_doy(i,j) = undef
          snow_first_doy(i,j) = undef
          snow_last_doy(i,j) = undef

          first_flag(i,j) = 0.0
          total_snow_days(i,j) = 0.0
        enddo
      enddo

c Count the number of days with snow depth greater than the threshold.
      do j=1,ny
        do i=1,nx
          do nday=1,ndays
            if (snod_yr(i,j,nday).gt.snod_threshold) then
              total_snow_days(i,j) = total_snow_days(i,j) + 1.0
            endif
          enddo
          if (total_snow_days(i,j).eq.0.0 .and. undef_mask(i,j).eq.0.0)
     &      print *,'total_snow_days = 0.0',total_snow_days(i,j),i,j
        enddo
      enddo

c Sweep forward through the data year to find the first and last snow
c   occurances.
      do j=1,ny
        do i=1,nx
          do nday=1,ndays

            if (snod_yr(i,j,nday).gt.snod_threshold .and.
     &          first_flag(i,j).eq.0.0) then
              snow_first_dos(i,j) = real(nday)
              first_flag(i,j) = 1.0
            endif

            if (snod_yr(i,j,nday).gt.snod_threshold) then
              snow_last_dos(i,j) = real(nday)
            endif

          enddo
        enddo
      enddo

c Find the first and last dates of the largest continuous snow-covered
c   span, and the associated time span.
      do j=1,ny
        do i=1,nx

          if (undef_mask(i,j).ne.undef) then
            icount = 0

c First find all of the snow starts.

c   Deal with the case where there is snow depth above the threshold
c   on day one.
            if (snod_yr(i,j,1).gt.snod_threshold) then
              icount = icount + 1
              istart(icount) = 1
            endif

c Deal with the middle days of the year.
            do nday=1,ndays-1
              if (snod_yr(i,j,nday).le.snod_threshold .and.
     &            snod_yr(i,j,nday+1).gt.snod_threshold) then
                icount = icount + 1
                istart(icount) = nday + 1
              endif

c Now find all of the snow ends.
              if (snod_yr(i,j,nday).gt.snod_threshold .and.
     &            snod_yr(i,j,nday+1).le.snod_threshold) then
                iend(icount) = nday
              endif
            enddo

c Deal with the last day of the year.
            if (snod_yr(i,j,ndays).gt.snod_threshold) then
              iend(icount) = ndays
            endif

c This is the case where there was no snow during the entire
c   period in this grid cell.
            if (icount.eq.0) print *,'icount = 0 found',icount,i,j

            if (icount.ne.0) then
              do iicount=1,icount
                idays(iicount) = iend(iicount) - istart(iicount) + 1
              enddo

c Find the longest idays.
              idays_old = 0
              do iicount=1,icount
                if (idays(iicount).gt.idays_old) then
                  idays_old = idays(iicount)
                  iiicount = iicount
                endif
              enddo

              if (undef_mask(i,j).eq.undef) then
                snow_onset_dos(i,j) = undef
                snow_free_dos(i,j) = undef
              else
                snow_onset_dos(i,j) = real(istart(iiicount))
                snow_free_dos(i,j) = real(iend(iiicount))
              endif
            else
              snow_onset_dos(i,j) = undef
              snow_free_dos(i,j) = undef
            endif

          endif
        enddo
      enddo

c Calculate the number of days during this snow-covered block.
      do j=1,ny
        do i=1,nx
          core_snow_days(i,j) = 
     &      snow_free_dos(i,j) - snow_onset_dos(i,j) + 1.0
          if (total_snow_days(i,j).eq.0.0) core_snow_days(i,j) = 0.0
        enddo
      enddo

c Convert the day-of-simulation to day-of-year.
      delta_days = doy_31Dec - doy_start_of_sim + 1
      do j=1,ny
        do i=1,nx

          if (snow_onset_dos(i,j).le.delta_days) then
            snow_onset_doy(i,j) = snow_onset_dos(i,j) +
     &        doy_start_of_sim - 1.0
          else
            snow_onset_doy(i,j) = snow_onset_dos(i,j) - delta_days
          endif

          if (snow_free_dos(i,j).le.delta_days) then
            snow_free_doy(i,j) = snow_free_dos(i,j) +
     &        doy_start_of_sim - 1.0
          else
            snow_free_doy(i,j) = snow_free_dos(i,j) - delta_days
          endif

          if (snow_first_dos(i,j).le.delta_days) then
            snow_first_doy(i,j) = snow_first_dos(i,j) +
     &        doy_start_of_sim - 1.0
          else
            snow_first_doy(i,j) = snow_first_dos(i,j) - delta_days
          endif

          if (snow_last_dos(i,j).le.delta_days) then
            snow_last_doy(i,j) = snow_last_dos(i,j) +
     &        doy_start_of_sim - 1.0
          else
            snow_last_doy(i,j) = snow_last_dos(i,j) - delta_days
          endif

        enddo
      enddo

c Before saving the data, oceans to undefined.
      do j=1,ny
        do i=1,nx
          if (undef_mask(i,j).eq.undef) then
            snow_onset_dos(i,j) = undef
            snow_onset_doy(i,j) = undef
            snow_free_dos(i,j) = undef
            snow_free_doy(i,j) = undef
            core_snow_days(i,j) = undef

            snow_first_dos(i,j) = undef
            snow_first_doy(i,j) = undef
            snow_last_dos(i,j) = undef
            snow_last_doy(i,j) = undef
            total_snow_days(i,j) = undef
          endif
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine find_max_date_core (nx,ny,path,irec_start,ndays,
     &  undef,doy_start_of_sim,doy_31Dec,snow_onset_dos,
     &  snow_free_dos,nx_max,ny_max,dt,print_inc,
     i  fname,
     o  vmax,vdate_dos,vdate_doy)

      implicit none

      character*(*) path
      character*(*) fname
      character*120 path_fname

      integer nx,ny,i,j,nday,ndays,irec,irec_start,nx_max,ny_max,
     &  length_path,trailing_blanks,n_3hrs_in_day

      real var(nx_max,ny_max)
      real vmax(nx_max,ny_max)
      real vdate_dos(nx_max,ny_max)
      real vdate_doy(nx_max,ny_max)

      real snow_onset_dos(nx_max,ny_max)
      real snow_free_dos(nx_max,ny_max)

      real undef,doy_start_of_sim,doy_31Dec,delta_days,dt,print_inc

      n_3hrs_in_day = 8

c Open the input array.
      length_path = 120 - trailing_blanks(path)
      path_fname = path(1:length_path)//fname

c     print *, path_fname

      open (21,file=path_fname,
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Initialize the array with some negative number.
      do j=1,ny
        do i=1,nx
          vmax(i,j) = undef
          vdate_dos(i,j) = undef
          vdate_doy(i,j) = undef
        enddo
      enddo

c Read in the data.  For the 3-hourly data outputs, irec is
c   reading the record at the end of each day; 8, 16, 24, etc.
c   This works for snod and swed, but not for things like tair
c   that would need to be averaged, or prec that would need to
c   be summed.
      do nday=1,ndays
        if (dt.eq.10800.0 .and. print_inc.eq.1) then
          irec = (irec_start + nday - 1) * n_3hrs_in_day
        else
          irec = irec_start + nday - 1
        endif
c       print *, nday,irec
        read (21,rec=irec) ((var(i,j),i=1,nx),j=1,ny)

c Find the maximum value of this variable over the year.
        do j=1,ny
          do i=1,nx
            if (nday.ge.snow_onset_dos(i,j) .and.
     &        nday.le.snow_free_dos(i,j)) then
              if (var(i,j).ge.vmax(i,j)) then
                vmax(i,j) = var(i,j)
                vdate_dos(i,j) = real(nday)
              endif
            endif
          enddo
        enddo
      enddo

      close (21)

c Convert the day-of-simulation to day-of-year.
      delta_days = doy_31Dec - doy_start_of_sim + 1
      do j=1,ny
        do i=1,nx
          if (vdate_dos(i,j).le.delta_days) then
            vdate_doy(i,j) = vdate_dos(i,j) + doy_start_of_sim - 1.0
          else
            vdate_doy(i,j) = vdate_dos(i,j) - delta_days
          endif
        enddo
      enddo

c If there was an undef in the original array, keep it as undef.
c   Just look at the last 2D array to do this.
      do j=1,ny
        do i=1,nx
          if (var(i,j).eq.undef) then
            vmax(i,j) = undef
            vdate_dos(i,j) = undef
            vdate_doy(i,j) = undef
          endif
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_irecs (iyr_start,imo_start,idy_start,nyear,
     o  irec_start,ndays,doy_start_of_sim,doy_31Dec)

c Calculate the starting and ending record for this year.

      implicit none

      integer iyr_start,imo_start,idy_start,nyear,iyr_beg,iyr_end,
     &  irec_start,ndays,ioptn,julian_start,julian_beg,julian_end,
     &  irec_end,idoy_start_of_sim,idoy_31Dec,idy,imo

      real doy_start_of_sim,doy_31Dec

c 'start' is the begining of the simulation.
c 'beg' is the begining of the simulation year.
c 'end' is the end of the simulation year.
      iyr_beg = iyr_start + nyear - 1
      iyr_end = iyr_start + nyear

      ioptn = 3
      call calndr (ioptn,idy_start,imo_start,iyr_start,julian_start)
      call calndr (ioptn,idy_start,imo_start,iyr_beg,julian_beg)
      call calndr (ioptn,idy_start,imo_start,iyr_end,julian_end)

c The end day is one less than this start of the next year.
      julian_end = julian_end - 1

      irec_start = julian_beg - julian_start + 1
      irec_end = julian_end - julian_start + 1
      ndays = julian_end - julian_beg + 1

c     print *,nyear,iyr_beg,iyr_end,irec_start,irec_end,ndays

c Find the doy of the start of this simulation year, and the doy
c   on 31 December of the start of this simulation year.  This
c   information is used to convert from 'day of simulation' to
c   'day of year'.
      ioptn = 1
      call calndr (ioptn,idy_start,imo_start,iyr_beg,idoy_start_of_sim)
      idy = 31
      imo = 12
      call calndr (ioptn,idy,imo,iyr_beg,idoy_31Dec)

      doy_start_of_sim = real(idoy_start_of_sim)
      doy_31Dec = real(idoy_31Dec)

c     print *,nyear,iyr_beg,doy_start_of_sim,doy_31Dec

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_yearly_ctl_file (nx,ny,nyears,nvars,iyr_start,
     &  var_string)

      implicit none

      integer nx,ny,iyr_start,nyears,nvars,nvar

      character*20 var_string(nvars)

      open (71,file='yearly_variables.ctl')

      write (71,51)
      write (71,52)
      write (71,53)
      write (71,54) nx
      write (71,55) ny

      write (71,56)
      write (71,57) nyears,iyr_start+1
      write (71,58) nvars

      do nvar=1,nvars
        write (71,100) var_string(nvar)
      enddo

      write (71,68)

      close (71)

   51 format ('DSET ^yearly_variables.gdat')
   52 format ('TITLE yearly SnowModel variables')
   53 format ('UNDEF -9999.0')

   54 format ('XDEF ',i8,' LINEAR  1.0  1.0')
   55 format ('YDEF ',i8,' LINEAR  1.0  1.0')
   56 format ('ZDEF        1 LINEAR  1.0  1.0')

   57 format ('TDEF ',i8,' LINEAR  01JAN',i4,' 1yr')

   58 format ('VARS     ',i4)

  100 format (a,' 0  0  xxxxxxxxxx')

   68 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

