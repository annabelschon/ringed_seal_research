c dataassim_user.f

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c subroutine DATAASSIM_USER does the SWE assimilation.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine DATAASSIM_USER(nx,ny,icorr_factor_index,
     &  corr_factor,max_iter,deltax,deltay,xmn,ymn,nobs_dates,
     &  print_inc,iday_init,imonth_init,iyear_init,dt,
     &  output_path_wo_assim,xhour_init)

c Perform the required correction (precipitation and melt) factor
c   calculations.  To run the data assimilation routines requires
c   additional model inputs to be added here (inputs in addition
c   to those in the snowmodel.par file).  Then you must recompile
c   the code before running it.

c This program works when you have data at one or many points
c   (many individual grid cells), for one or more times.  And for
c   data (e.g., average swe) over areas of grid cells; there can be
c   many of these areas, at many different times.

c The input (swe) data file is assumed to be in the directory:
c   /snowmodel/swe_assim/.  See below for the details of how this
c   file is to be configured.

c The data assimilation produces a couple of extra files the user
c   can look at to study what the model did with the input data
c   file (fname_sweobs) that was provided.  First, there is a text
c   file written to /snowmodel/swe_assim/ that provides a summary
c   of the calculations that were done (see unit=77 in the code
c   below for what is written to this file).  Second, there is a
c   copy of the precipitation correction surfaces that were
c   generated.  This is also placed in /snowmodel/swe_assim/, the
c   file is a GrADS file (called corr_factor.gdat), and there is
c   also a corr_factor.ctl there that can be modified to fit the
c   current simulation.  The data layers in corr_factor.gdat arec
c   structured as follows:
c   The number of layers equals the total number of observation
c   times that were assimilated, plus the number of  years in the
c   assimilation.  The order is: the correction surface (cf) for
c   the time between the start of the simulation and observation
c   time 1 in year 1, the cf for the time between obs time 1 in
c   year 1 and obs time 2 in year 1, etc., then a cf==1.0 for the
c   time between the last obs time and the end of year 1 (or the
c   end of the simulation for a 1-year run).  Then this order
c   repeats for the rest of the years of the simulation.  In the
c   GrADS control file (corr_factor.ctl) these layers are assumed
c   to correspond to different times in the data file (although
c   the actual time increment defined in the .ctl file is not
c   really relevant: for example, t=1 corresponds to the cf for
c   obs 1, t=2 is for obs 2, t=3 is for the time between the last
c   obs time and the end of year 1, t=4 is for the obs 1 in year
c   2, etc.).

      implicit none

      include 'snowmodel.inc'

      real deltax,deltay,beta,areas_flag,print_inc,dt,xhour_init
      real sprec_ratio(max_obs_dates),smelt_ratio(max_obs_dates)
      real corr_factor(nx_max,ny_max,max_obs_dates)
      real areas_mask(nx_max,ny_max)

      double precision xmn,ymn
      double precision xstn(nx_max*ny_max),ystn(nx_max*ny_max)

      integer icorr_factor_index(max_time_steps)
      integer iobs_rec(max_obs_dates)
      integer nobs_dates,nx,ny,max_iter,local_assim_flag,iday_init,
     &  imonth_init,iyear_init,nyear,nyears,nobs_total,nobs_total_cfi

      character*89 fname_swed,fname_sspr,fname_ssmt
      character*80 fname_sweobs
      character*80 fname_sweobs_barnes_mask

      character*80 output_path_wo_assim
      integer trailing_blanks,i_len_wo
      integer i,j

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c BEGIN USER EDIT SECTION.
c BEGIN USER EDIT SECTION.

c Define how many years there are in this simulation.  Note that
c   these are SnowModel simulation years.  So, for example, year
c   1 might be 1 Sep 2007 - 31 Aug 2008, and year 2 might be
c   1 Sep 2008 - 31 Aug 2009, etc.
c NOTE: this is now put in the first line of the swe assimilation
c   input file (in 'fname_sweobs' below).
c     nyears = 20
c     nyears = 1

c A single data file describes the observation information that
c   will be used in the data assimilation.  This file contains the
c   following information in the following format.  The id can be
c   any number, x and y are easting and northing in m, and swe is
c   in m).

c   total_number_of_observation_dates_for_this_year
c   iyr imo idy (for this observation date)
c   number_of_stations_for_this_observation_date
c   id x y swe
c   id x y swe
c   id x y swe
c   iyr imo idy (for this observation date)
c   number_of_stations_for_this_observation_date
c   id x y swe
c   id x y swe

c For example:

c   2
c   2014 3 15
c   3
c   101 3456.7 23677.4 0.42
c   102 3556.3 25079.3 0.52
c   103 3106.2 29089.3 0.59
c   2014 4 1
c   2
c   101 3456.7 23677.4 0.48
c   103 3106.2 29089.3 0.62

c Then this repeats for each year of the assimilation (the input
c   file looks like the above for a single-year run, and for
c   multi-year runs the data for each following year is just
c   stacked on top of the previous year).  The example below is
c   for a two-year assimilation run.
c NOTE: the first '2=nyears' is because this is two-year run (if
c   this were a 1-year run, this would be a 1, etc.).

c   2
c   2
c   2014 3 15
c   3
c   101 3456.7 23677.4 0.42
c   102 3556.3 25079.3 0.52
c   103 3106.2 29089.3 0.59
c   2014 4 1
c   2
c   101 3456.7 23677.4 0.48
c   103 3106.2 29089.3 0.62
c   1
c   2015 3 25
c   2
c   101 3456.7 23677.4 0.23
c   102 3556.3 25079.3 0.32

c For the run where you have years with no data to assimilate,
c   and some years with data to assimilate, the code still
c   requires a "total_number_of_observation_dates_for_this_year"
c   line for each year.  So, if you have a year with no data, this
c   must be set to be 0 (zero).  If this is 0, the code sets the
c   correction factor to equal 1.0 for that simulation year, and
c   no adjustments are made to the precipitation for that year.
c   For example, if the first two, and fourth, years of a five-
c   year run have no data to assimilate, your input file would
c   look like:

c   5
c   0
c   0
c   1
c   2014 3 15
c   3
c   101 3456.7 23677.4 0.42
c   102 3556.3 25079.3 0.52
c   103 3106.2 29089.3 0.59
c   0
c   1
c   2015 3 25
c   2
c   101 3456.7 23677.4 0.23
c   102 3556.3 25079.3 0.32

c Provide the name of the data file that contains the observed swe
c   information (as described above).  This can no longer be
c   changed (defining it this way allows the error checking to be
c   done).
      fname_sweobs = 'swe_assim/swe_obs.dat'

c Define the file names of the swe depth (swed), annual summed snow
c   precipitation (sspr), and annual summed snowmelt (ssmt) outputs
c   from the first iteration of the data assimilation run.  In this
c   implementation of the data assimilation code, I have assumed
c   that the output files are those created by outputs_user.f,
c   where there is an individual file for each variable.
c NOTE: in the latest code version these paths have already been
c   defined in the snowmodel.par file.
c     fname_swed = 'outputs/wo_assim/swed.gdat'
c     fname_sspr = 'outputs/wo_assim/sspr.gdat'
c     fname_ssmt = 'outputs/wo_assim/ssmt.gdat'
      i_len_wo = 80 - trailing_blanks(output_path_wo_assim)
      fname_swed = output_path_wo_assim(1:i_len_wo)//'swed.gdat'
      fname_sspr = output_path_wo_assim(1:i_len_wo)//'sspr.gdat'
      fname_ssmt = output_path_wo_assim(1:i_len_wo)//'ssmt.gdat'

c THE PARAMETERS BELOW ARE RARELY CHANGED, UNLESS YOU ARE DOING AN
c   AREAS ASSIMILATION (INSTEAD OF ASSIMILATING POINT DATA).

c Beta controls the interpolation distance weights.  Beta = 1.0
c   will give you a very smooth field, and correction factor
c   distributions that may not produce swe's that exactly match
c   the observations.  Beta << 1.0 will give you correction factor
c   fields that go right through the data.  If you just have one
c   data point/area, beta is not used.
      beta = 1.0
c     beta = 0.1
c     beta = 0.5

c Define whether this simulation will be processing areas (data
c   within groups of grid cells: areas_flag = 1.0), or points
c   (single grid cells: areas_flag = 0.0).  Note that if you have
c   a combination of areas and points, you have to use the areas
c   option and treat each point like a single-grid-cell (small)
c   area.
      areas_flag = 0.0
c     areas_flag = 1.0

c If this is an areas simulation, open and read in the areas mask
c   data.  Note that here I assume that the area mask is a nx by ny
c   file with undef values everywhere except at the area 'stations'.
c   And that each 'station' area is given a 1.0, 2.0, etc. that
c   corresponds to the order of the station listing in the 'station'
c   data input file (the first 'station' listed has mask value = 1.0,
c   the second listed has mask value = 2.0, etc.
      if (areas_flag.eq.1.0) then
c       open(63,file=
c    &    '../1_topo_vege/8_mk_glac_front_mask/seals_fjord_mask.gdat',
c    &    form='unformatted',access='direct',recl=4*nx*ny)
c       read(63,rec=1) ((areas_mask(i,j),i=1,nx),j=1,ny)
c If you have two masks for two different observation dates, then
c   do something like the following.
c       open(63,file='swe_assim/zack_obs_mask.gdat',
c    &    form='unformatted',access='direct',recl=4*nx*ny)
c       read(63,rec=1) ((areas_mask(i,j,1),i=1,nx),j=1,ny)
c       read(63,rec=2) ((areas_mask(i,j,2),i=1,nx),j=1,ny)
      endif

c Define whether this simulation is going to restrict the
c   assimilation influence to some local area surrounding each
c   data point that is assimilated.  This was implemented for
c   ANWR simulations where we only had observations in a corner
c   of the simulation domain and I didn't want the corrections
c   to extend too far outside that local observation area.  So,
c   this is an example of what can be done, and it is not written
c   for general application.  If you want to do something similar,
c   the associated subroutine can be edited for your specific
c   simulation of interest.  For yes, local_assim_flag = 1, for
c   no, local_assim_flag = 0.
      local_assim_flag = 0

c Identify the file that contains the local data assimilation
c   mask.  This is only used if local_assim_flag = 1.
      fname_sweobs_barnes_mask =
     &  '../swe_obs/2014/barnes/obs.gridded.gdat'

c END USER EDIT SECTION.
c END USER EDIT SECTION.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Open the swe input file.
      open (unit=61,file=fname_sweobs)

c Read in the number of years this assimilation will process.
      read(61,*) nyears

c LOOP THROUGH THE YEARS IN THE SIMULATION.
      do nyear=1,nyears

c Run the data assimilation routines.
        call data_assimilation(nx,ny,deltax,deltay,beta,
     &    areas_flag,sprec_ratio,smelt_ratio,corr_factor,
     &    areas_mask,xmn,ymn,xstn,ystn,nobs_dates,iobs_rec,
     &    local_assim_flag,fname_swed,fname_sspr,fname_ssmt,
     &    fname_sweobs,fname_sweobs_barnes_mask,iday_init,
     &    imonth_init,iyear_init,nyear,nobs_total,print_inc,
     &    nyears,dt,max_iter,xhour_init)

c Build an array indicating the appropriate correction factor to
c   use at any given time during the simulation.  What this does
c   is define an index array that contains the record number that
c   gets used at every model time step during the second model run
c   loop.  This record number corresponds to the record (krec) of
c   the corr_factor(i,j,krec) array that was generated and saved
c   in the subroutine above.
        call corr_factor_index(nobs_dates,icorr_factor_index,
     &    iobs_rec,max_iter,sprec_ratio,smelt_ratio,print_inc,
     &    iday_init,imonth_init,iyear_init,nyear,nobs_total_cfi,
     &    nyears)

      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine data_assimilation(nx,ny,deltax,deltay,beta,
     &  areas_flag,sprec_ratio,smelt_ratio,corr_factor,
     &  areas_mask,xmn,ymn,xstn,ystn,nobs_dates,iobs_rec,
     &  local_assim_flag,fname_swed,fname_sspr,fname_ssmt,
     &  fname_sweobs,fname_sweobs_barnes_mask,iday_init,
     &  imonth_init,iyear_init,nyear,nobs_total,print_inc,
     &  nyears,dt,max_iter,xhour_init)

      implicit none

      include 'snowmodel.inc'

      real deltax,deltay,undef,dn,beta,areas_flag,swe_count,cf_min,
     &  print_inc,dt,xhour_init
      real sprec_ratio(max_obs_dates),smelt_ratio(max_obs_dates)
      real corr_factor(nx_max,ny_max,max_obs_dates)
      real swe_tmp(nx_max,ny_max),sum_sprec_tmp1(nx_max,ny_max),
     &  sum_sprec_tmp2(nx_max,ny_max),grid(nx_max,ny_max),
     &  areas_mask(nx_max,ny_max),sum_smelt_tmp1(nx_max,ny_max),
     &  sum_smelt_tmp2(nx_max,ny_max)
      real corr_factor_tmp(nx_max*ny_max),swe_obs(nx_max*ny_max),
     &  swe_model(nx_max*ny_max),sumsprec_model(nx_max*ny_max),
     &  delta_old(nx_max*ny_max),obsid(nx_max*ny_max),
     &  sumsmelt_model(nx_max*ny_max),obsid_old(nx_max*ny_max),
     &  delta_old_tmp(nx_max*ny_max)
c     real corr_offset(nx_max*ny_max)

      double precision xmn,ymn
      double precision xstn(nx_max*ny_max),ystn(nx_max*ny_max)

      integer iobs_rec(max_obs_dates)
      integer ii(nx_max*ny_max),jj(nx_max*ny_max)
      integer iobs_num,irec1,irec2,nobs_dates,nx,ny,i,j,ifill,
     &  iobsint,k,nstns,nstns_old,kk,local_assim_flag,iiyr,iimo,
     &  iidy,iobs_rec_tmp,iday_init,imonth_init,iyear_init,nyear,
     &  krec,nobs_total,nyears,max_iter

      character*89 fname_swed,fname_sspr,fname_ssmt
      character*80 fname_sweobs
      character*80 fname_sweobs_barnes_mask

c Perform some initialization steps.
      if (nyear.eq.1) then

c Define some of the constants and parameters used in the data
c   assimilation.  ifill should be = 1; in that case undef is not
c   really used (so it does not have to be the same as defined
c   in the .par file).
        undef = -9999.0
        ifill = 1
        iobsint = 0

c Open a file to write some basic correction factor information.
c   This just saves information that the user might want to look
c   at.
        open (unit=77,file='swe_assim/corr_factor.txt')

c Open an output file for the correction factor array.
        open(62,file='swe_assim/corr_factor.gdat',
     &    form='unformatted',access='direct',recl=4*nx*ny)

c Initialize the number-of-observations counter.
        nobs_total = 0

      endif

c Read the number of observation dates for this year.
      read(61,*) nobs_dates

c If you have observations for this year, generate the correction
c   factors.  For the case of no observations for this year, set
c   the correction factor equal to 1.0.
      if (nobs_dates.gt.0) then

c Loop through the observation dates.
        do iobs_num=1,nobs_dates

c Increment the number-of-observations counter.
          nobs_total = nobs_total + 1

c Read the date corresponding to this observation.
          read(61,*) iiyr,iimo,iidy

c Convert this date to the corresponding record number in the
c   original SnowModel output files.  Note that this has assumed
c   that daily data files were written out since the start of
c   the simulation.
          call get_obs_record(iday_init,imonth_init,iyear_init,
     &      iidy,iimo,iiyr,iobs_rec_tmp,print_inc,dt)
          iobs_rec(iobs_num) = iobs_rec_tmp

c For this observation date, read in the data describing the
c   location and swe values for each observation.  For areas
c   simulations, xstn, and ystn correspond to the center of the
c   area domain and they are not really used.
          read(61,*) nstns
          do k=1,nstns
            read(61,*) obsid(k),xstn(k),ystn(k),swe_obs(k)
          enddo

c Convert the x and y locations to (ii,jj) locations.
          do k=1,nstns
            ii(k) = 1 + nint((xstn(k) - xmn) / deltax)
            jj(k) = 1 + nint((ystn(k) - ymn) / deltay)
          enddo

c If you do a data assimilation run from start to finish, it is
c   not required to close and reopen these files.  But if you are
c   doing a history restart then these files are no longer open
c   so you must do this.  What I do below works for both cases.
          close (238)
          close (239)
          close (240)

c Open the required inputs from the initial assimilation loop.
c   Open swe depth (swe_depth).
c     /outputs/wo_assim/swed.gdat is unit 238 in outputs_user.f
c   Open sum snow precip (sum_sprec).
c     /outputs/wo_assim/sspr.gdat is unit 239 in outputs_user.f
c   Open sum snow melt (sum_smelt).
c     /outputs/wo_assim/ssmt.gdat is unit 240 in outputs_user.f
          open (238,file=fname_swed,
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (239,file=fname_sspr,
     &      form='unformatted',access='direct',recl=4*1*nx*ny)
          open (240,file=fname_ssmt,
     &      form='unformatted',access='direct',recl=4*1*nx*ny)

c Read the model output for the first observation time.
          if (iobs_num.eq.1) then
            irec1 = iobs_rec(iobs_num)
            read(238,rec=irec1) ((swe_tmp(i,j),i=1,nx),j=1,ny)
            read(239,rec=irec1) ((sum_sprec_tmp1(i,j),i=1,nx),j=1,ny)
            read(240,rec=irec1) ((sum_smelt_tmp1(i,j),i=1,nx),j=1,ny)

c For points, just pull the data at the appropriate grid cell.
c   For areas, average the data over the masked out area for each
c   'station'.
            do k=1,nstns
              if (areas_flag.eq.0.0) then
                swe_model(k) = swe_tmp(ii(k),jj(k))
                sumsprec_model(k) = sum_sprec_tmp1(ii(k),jj(k))
                sumsmelt_model(k) = sum_smelt_tmp1(ii(k),jj(k))
              elseif (areas_flag.eq.1.0) then
                swe_model(k) = 0.0
                sumsprec_model(k) = 0.0
                sumsmelt_model(k) = 0.0
                swe_count = 0.0
                do j=1,ny
                  do i=1,nx
                    if (areas_mask(i,j).eq.obsid(k)) then
c The following is used if the mask changes with observation time.
c                   if (areas_mask(i,j,iobs_num).eq.obsid(k)) then
                      swe_count = swe_count + 1.0
                      swe_model(k) = swe_model(k) + swe_tmp(i,j)
                      sumsprec_model(k) = sumsprec_model(k) +
     &                  sum_sprec_tmp1(i,j)
                      sumsmelt_model(k) = sumsmelt_model(k) +
     &                  sum_smelt_tmp1(i,j)
                    endif
                  enddo
                enddo
                swe_model(k) = swe_model(k) / swe_count
                sumsprec_model(k) = sumsprec_model(k) / swe_count
                sumsmelt_model(k) = sumsmelt_model(k) / swe_count
              endif
            enddo
          endif

c Read the model output for any additional observation times (irec1
c   = current obs time, irec2 = previous obs time).
          if (iobs_num.gt.1) then
            irec1 = iobs_rec(iobs_num)
            irec2 = iobs_rec(iobs_num-1)
            read(238,rec=irec1) ((swe_tmp(i,j),i=1,nx),j=1,ny)
            read(239,rec=irec1) ((sum_sprec_tmp1(i,j),i=1,nx),j=1,ny)
            read(239,rec=irec2) ((sum_sprec_tmp2(i,j),i=1,nx),j=1,ny)
            read(240,rec=irec1) ((sum_smelt_tmp1(i,j),i=1,nx),j=1,ny)
            read(240,rec=irec2) ((sum_smelt_tmp2(i,j),i=1,nx),j=1,ny)

c For points, just pull the data at the appropriate grid cell.
c   For areas, average the data over the masked out area for each
c   'station'.
            do k=1,nstns
              if (areas_flag.eq.0.0) then
                swe_model(k) = swe_tmp(ii(k),jj(k))
                sumsprec_model(k) = sum_sprec_tmp1(ii(k),jj(k)) -
     &            sum_sprec_tmp2(ii(k),jj(k))
                sumsmelt_model(k) = sum_smelt_tmp1(ii(k),jj(k)) -
     &            sum_smelt_tmp2(ii(k),jj(k))
              elseif (areas_flag.eq.1.0) then
                swe_model(k) = 0.0
                sumsprec_model(k) = 0.0
                sumsmelt_model(k) = 0.0
                swe_count = 0.0
                do j=1,ny
                  do i=1,nx
                    if (areas_mask(i,j).eq.obsid(k)) then
c The following is used if the mask changes with observation time.
c                   if (areas_mask(i,j,iobs_num).eq.obsid(k)) then
                      swe_count = swe_count + 1.0
                      swe_model(k) = swe_model(k) + swe_tmp(i,j)
                      sumsprec_model(k) = sumsprec_model(k) +
     &                  sum_sprec_tmp1(i,j) - sum_sprec_tmp2(i,j)
                      sumsmelt_model(k) = sumsmelt_model(k) +
     &                  sum_smelt_tmp1(i,j) - sum_smelt_tmp2(i,j)
                    endif
                  enddo
                enddo
                swe_model(k) = swe_model(k) / swe_count
                sumsprec_model(k) = sumsprec_model(k) / swe_count
                sumsmelt_model(k) = sumsmelt_model(k) / swe_count
              endif
            enddo
          endif

c To avoid a divide by zero later on, make sure sumsprec_model and
c   sumsmelt_model are not both zero.
          do k=1,nstns
            sumsprec_model(k) = sumsprec_model(k) + 1.0e-6
          enddo

c Determine whether we will adjust the precipitation or melt.  To
c   do this, calculate the relative contributions of precipitation
c   and melt inputs for this correction period.  This can be
c   different for each observation interval.  Calculate the average
c   over all of the stations/areas in the domain.
          sprec_ratio(iobs_num) = 0.0
          smelt_ratio(iobs_num) = 0.0
          do k=1,nstns
            sprec_ratio(iobs_num) = sprec_ratio(iobs_num) +
     &        sumsprec_model(k) / (sumsprec_model(k)+sumsmelt_model(k))
            smelt_ratio(iobs_num) = smelt_ratio(iobs_num) +
     &        sumsmelt_model(k) / (sumsprec_model(k)+sumsmelt_model(k))
          enddo
          sprec_ratio(iobs_num) = sprec_ratio(iobs_num) / real(nstns)
          smelt_ratio(iobs_num) = smelt_ratio(iobs_num) / real(nstns)

c Initialize the delta swe variable.
          if (iobs_num.eq.1) then
            do k=1,nstns
              delta_old(k) = 0.0
            enddo
          else
            do k=1,nstns
              delta_old(k) = 0.0
            enddo
            do k=1,nstns
              do kk=1,nstns_old
                if(obsid(k).eq.obsid_old(kk))
     &            delta_old(k) = delta_old_tmp(kk)
              enddo
            enddo
c           write (77,*)
c           do k=1,nstns
c             write (77,*) 'k, delta_old(k)',k,100.*delta_old(k)
c           enddo
c           write (77,*)
          endif

c Calculate the correction factor to be used in the next model
c   iteration.  Let the correction factor equal 1.0 during
c   periods where we have no swe observations.  Also, note that the
c   reason for the delta_old variable is to account for the fact
c   that that delta will be fixed with the previous date correction
c   time period.  This is one of the things that allows the
c   correction to be done in two model iterations.
c If sumsprec_model or sumsmelt_model are too small to be used in
c   the assimilation (like less than 1 mm), set corr_factor_tmp = 1.0
c   so no adjustments are performed for this observation interval.
          cf_min = 0.1
          do k=1,nstns
            if (sprec_ratio(iobs_num).ge.smelt_ratio(iobs_num)) then
              if (sumsprec_model(k).lt.1.0e-3) then
                corr_factor_tmp(k) = 1.0
              else
                corr_factor_tmp(k) = 1.0 +
     &            (swe_obs(k) - swe_model(k) - delta_old(k)) /
     &            sumsprec_model(k)
                corr_factor_tmp(k) = max(cf_min,corr_factor_tmp(k))
              endif
            else
              if (sumsmelt_model(k).lt.1.0e-3) then
                corr_factor_tmp(k) = 1.0
              else
                corr_factor_tmp(k) = 1.0 +
     &            (swe_model(k) - swe_obs(k) + delta_old(k)) /
     &            sumsmelt_model(k)
                corr_factor_tmp(k) = max(cf_min,corr_factor_tmp(k))
              endif
            endif
c Save some information about the model calculations.
c           write (77,*) '---'
c           write (77,*) k,swe_obs(k)
c           write (77,*) k,swe_model(k)
c           write (77,*) k,delta_old(k)
c           write (77,*) k,swe_obs(k)-swe_model(k)-delta_old(k)
c           write (77,*) k,sumsprec_model(k)
c           write (77,*) k,sumsmelt_model(k)
c           write (77,*) k,corr_factor_tmp(k)
c           write (77,*) '---'

c Save some data from this observation time for use at the next
c   observation time.
            nstns_old = nstns
            obsid_old(k) = obsid(k)
            delta_old_tmp(k) = swe_obs(k) - swe_model(k)
          enddo

c Now that I have the correction factors calculated at each
c   observation point, interpolate those over the simulation domain.

c Use the barnes oi scheme to create the distribution. If there is
c   only a single station, distribute those data uniformly over
c   the domain.
          if (nstns.ge.2) then
            call get_dn(nx,ny,deltax,deltay,nstns,dn,iobsint)

c Modify the size of dn.
            dn = beta * dn

            call barnes_oi(nx,ny,deltax,deltay,xmn,ymn,
     &        nstns,xstn,ystn,corr_factor_tmp,dn,grid,undef,ifill)
          elseif (nstns.eq.1) then
            call single_stn(nx,ny,nstns,corr_factor_tmp,grid)
          endif

c The following calculations are done if you want to implement the
c   special case where you limit the data assimilation corrections
c   to a certain area of your simulation domain.  Edits to this
c   subroutine will certainly be required to make it work for your
c   specific application.  This subroutine generates correction
c   factors with 1.0's in places outside the local obs influences.
          if (local_assim_flag.eq.1) then
            call mk_local_cfs(nx,ny,undef,xmn,ymn,deltax,deltay,
     &        fname_sweobs,fname_sweobs_barnes_mask,nobs_dates,
     &        corr_factor_tmp,beta,iobsint,ifill,grid)
          endif

c Define the correction surface record that corresponds to this
c   year and observation.
          krec = nobs_total + (nyear - 1)
          if (krec.gt.max_obs_dates) then
            print *, 'max_obs_dates must be increased in snowmodel.inc'
            print *, 'krec = ',krec,'  max_obs_dates = ',max_obs_dates
            stop
          endif

c Use the gridded output file to build the corr_factor array.
          do j=1,ny
            do i=1,nx
              corr_factor(i,j,krec) = grid(i,j)
              corr_factor(i,j,krec) =
     &          max(cf_min,corr_factor(i,j,krec))
            enddo
          enddo

c Note that the interpolation scheme may have produced correction
c   factors that do not produce exact matches with the
c   observations (like happens with the case of having a single
c   data point).  If you are interested, calculate the difference
c   between the exact value and the actual calculated value, and
c   then write it out as done below.
c         do k=1,nstns
c           if (sprec_ratio(iobs_num).ge.smelt_ratio(iobs_num)) then
c             corr_offset(k) = sumsprec_model(k) *
c    &          (corr_factor(ii(k),jj(k),iobs_num) - corr_factor_tmp(k))
c           else
c             corr_offset(k) = sumsmelt_model(k) *
c    &          (corr_factor(ii(k),jj(k),iobs_num) - corr_factor_tmp(k))
c           endif
c         enddo

c Write some information to the text file.
          write (77,*)
     &  '**************************************************************'

c         write (77,*) ' sprec_ratio =',sprec_ratio(iobs_num),
c    &      '  smelt_ratio =',smelt_ratio(iobs_num)
c         write (77,*)

          write (77,*) iiyr,iimo,iidy
          write (77,*) '          sprec_ratio =',sprec_ratio(iobs_num)
          write (77,*) '          smelt_ratio =',smelt_ratio(iobs_num)
          write (77,*)

          do k=1,nstns
c           write (77,*) k,' swe diff =',
c    &        100.0*abs(swe_obs(k)-swe_model(k)),' SWE OBS =',
c    &        100.0*swe_obs(k)
c           write (77,*) 'sumsprec =',sumsprec_model(k)*100.,
c    &        '  SWE MODEL =',swe_model(k)*100.
c           write (77,*) 'iobs_num =',iobs_num,
c    &        '  CORR_FACTOR =',corr_factor_tmp(k)

            write (77,*) '         SWE OBS (cm) =',100.0*swe_obs(k)
            write (77,*) '       SWE MODEL (cm) =',100.0*swe_model(k)
            write (77,*) '          CORR_FACTOR =',corr_factor_tmp(k)

cc          write (77,*) 'corr_offset =',100.*corr_offset(k),
cc   &        '  ij',ii(k),jj(k)
cc          write (77,*) '     delta_old =',100.*delta_old(k),
cc   &        '      corr fact used =',corr_factor(ii(k),jj(k),iobs_num)
c           write (77,*)
c           write (77,*) k,' sumsprec_model(k) =',sumsprec_model(k)
c           write (77,*) k,' sumsmelt_model(k) =',sumsmelt_model(k)
c           write (77,*)
          enddo

          write (77,*)
     &  '**************************************************************'

c Write the output data to a grads file.
          write(62,rec=krec) ((corr_factor(i,j,krec),i=1,nx),j=1,ny)

        enddo

c Fill corr_factor with 1.0 for the period following the last obs
c   date in the current year.  This is also required for the history
c   restart to work correctly.  Without the history restart this was
c   already done as part of the model initialization.
        if (krec+1.gt.max_obs_dates) then
          print *, 'max_obs_dates must be increased in snowmodel.inc'
          print *, 'krec+1 = ',krec+1,'  max_obs_dates = ',max_obs_dates
          stop
        endif
        do j=1,ny
          do i=1,nx
            corr_factor(i,j,krec+1) = 1.0
          enddo
        enddo

        write(62,rec=krec+1) ((corr_factor(i,j,krec+1),i=1,nx),j=1,ny)

c The met, topo, and veg files must be closed for the next model
c   iteration.
        close (20)
        close (37)
        close (38)

        close (238)
        close (239)
        close (240)

      else

c For the case of no observations for this year, set the correction
c   factor equal to 1.0.
        krec = nobs_total + nyear
        if (krec.gt.max_obs_dates) then
          print *, 'max_obs_dates must be increased in snowmodel.inc'
          print *, 'krec = ',krec,'  max_obs_dates = ',max_obs_dates
          stop
        endif

        do j=1,ny
          do i=1,nx
            corr_factor(i,j,krec) = 1.0
          enddo
        enddo

        write(62,rec=krec) ((corr_factor(i,j,krec),i=1,nx),j=1,ny)

      endif

c Create the GrADS .ctl (control) file to go with the GrADS
c   .gdat corr_factor file that was generated by this model run.
      call mk_cf_prec_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  nobs_total,nyears)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine corr_factor_index(nobs_dates,icorr_factor_index,
     &  iobs_rec,max_iter,sprec_ratio,smelt_ratio,print_inc,
     &  iday_init,imonth_init,iyear_init,nyear,nobs_total_cfi,
     &  nyears)

      implicit none

      include 'snowmodel.inc'

      integer icorr_factor_index(max_time_steps)

      integer kk,istart,iend,nobs_dates,iter,max_iter,krec,nyear,
     &  nobs_total_cfi,ioptn,julian_start,iiyr,julian_end,iday_init,
     &  imonth_init,iyear_init,nyears
      integer iobs_rec(max_obs_dates)
      real sprec_ratio(max_obs_dates),smelt_ratio(max_obs_dates) 
      real print_inc

c Initialize the number-of-observations counter.
      if (nyear.eq.1) nobs_total_cfi = 0

c Build an array indicating the appropriate correction factor to
c   use at each time step during the simulation.
      if (nobs_dates.gt.0) then

c Loop through the observation dates.
        do kk=1,nobs_dates+1

c Increment the number-of-observations counter.
          if (kk.le.nobs_dates) nobs_total_cfi = nobs_total_cfi + 1

c FIRST, FROM THE SIMULATION START UNTIL THE FIRST OBSERVATION, FOR
c   EACH YEAR.
          if (kk.eq.1) then

c Here istart equals the first model time step of each year.
            ioptn = 3
            call calndr (ioptn,iday_init,imonth_init,iyear_init,
     &        julian_start)

c Find the Julian day for this data record.
            iiyr = iyear_init + (nyear - 1)
            call calndr (ioptn,iday_init,imonth_init,iiyr,julian_end)

c Calculate istart.
            istart = 1 + (julian_end - julian_start) * nint(print_inc)

c Here iend equals the model time step corresponding to the end of
c   the first observation day.  Take advantage of the data-file
c   record that was calculated before.  Convert from daily data
c   output records to model time steps using print_inc.
            iend = iobs_rec(kk) * nint(print_inc)

c Define the corr_factor data array record.
            krec = nobs_total_cfi + (nyear - 1)

c Fill the index for each model time step.
            do iter=istart,iend
              if (sprec_ratio(kk).ge.smelt_ratio(kk)) then
                icorr_factor_index(iter) = krec
              else
                icorr_factor_index(iter) = -krec
              endif
            enddo

c SECOND, BETWEEN THE LAST OBSERVATION AND THE END OF THE SIMULATION.
          elseif (kk.eq.nobs_dates+1) then
            istart = iobs_rec(kk-1) * nint(print_inc) + 1

c Here iend equals the last time step in the year of interest.
c Find the Julian day for this data record.
            iiyr = iyear_init + nyear
            call calndr (ioptn,iday_init,imonth_init,iiyr,julian_end)

c Calculate iend for this year.
            iend = (julian_end - julian_start) * nint(print_inc)
            iend = min(iend,max_iter)

c Define the corr_factor data array record.
            krec = nobs_total_cfi + nyear

c Fill the index for each model time step.
            do iter=istart,iend
              icorr_factor_index(iter) = krec
            enddo

c THIRD, ANY PERIODS BETWEEN OBSERVATIONS.
          else
            istart = iobs_rec(kk-1) * nint(print_inc) + 1
            iend = iobs_rec(kk) * nint(print_inc)

c Define the corr_factor data array record.
            krec = nobs_total_cfi + (nyear - 1)

c Fill the index for each model time step.
            do iter=istart,iend
              if (sprec_ratio(kk).ge.smelt_ratio(kk)) then
                icorr_factor_index(iter) = krec
              else
                icorr_factor_index(iter) = -krec
              endif
            enddo
          endif
        enddo

      else

c Create an array indes for the case of no observations for this
c   year.  Here istart equals the first model time step of each year.
        ioptn = 3
        call calndr (ioptn,iday_init,imonth_init,iyear_init,
     &    julian_start)

c Find the Julian day for this data record.
        iiyr = iyear_init + (nyear - 1)
        call calndr (ioptn,iday_init,imonth_init,iiyr,julian_end)

c Calculate istart.
        istart = 1 + (julian_end - julian_start) * nint(print_inc)

c Calculate iend for this year.
        iiyr = iyear_init + nyear
        call calndr (ioptn,iday_init,imonth_init,iiyr,julian_end)
        iend = (julian_end - julian_start) * nint(print_inc)
        iend = min(iend,max_iter)

c Define the corr_factor data array record.
        krec = nobs_total_cfi + nyear

c Fill the index for each model time step.
        do iter=istart,iend
          icorr_factor_index(iter) = krec
        enddo

      endif

c SAVE A VERSION OF THE INDEX THAT THE USER CAN EASILY LOOK AT.
      if (nyear.eq.nyears) then
        print *
        do iter=1,max_iter
          write (77,*) iter,icorr_factor_index(iter)
        enddo
        print *
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_local_cfs(nx,ny,undef,xmn,ymn,deltax,deltay,
     &  fname_sweobs,fname_sweobs_barnes_mask,nobs_dates,
     &  corr_factor_tmp,beta,iobsint,ifill,grid)

      implicit none

      include 'snowmodel.inc'

      integer nstns,k,nx,ny,i,j,ntstations,icount,nobs_dates,ifill,
     &  iobsint,nstns2

      real dummy,stnid,undef,deltax,deltay,beta,dn
      real xmask(nx_max,ny_max),grid(nx_max,ny_max)
      real corr_factor_tmp(nx_max*ny_max),cf_tmp2(nx_max*ny_max),
     &  obsid(nx_max*ny_max)

      real var(nstns_max)

      double precision xmn,ymn
      double precision yg(nx_max,ny_max),xg(nx_max,ny_max)
      double precision xstn(nstns_max),ystn(nstns_max)

      character*80 fname_sweobs
      character*80 fname_sweobs_barnes_mask

      print *
      print *,'You are doing local assimilation, this requires'
      print *,'an observation mask to have been generated before'
      print *,'the model run and saved in the file called:'
      print *,'fname_sweobs_barnes_mask'
      print *
      print *,'This was also not tested after some big changes to'
      print *,'the data assimilation code.  So I strongly suggest'
      print *,'you make sure this is doing what you want when you'
      print *,'first start using it.'
      print *
      stop
      if (nobs_dates.gt.1) then
        print *,'THIS HAS NOT BEEN MADE TO WORK WITH MORE THAN'
        print *,'ONE OBS TIME.'
        stop
      endif
      print *

c Save the correction factors for the local-influence assimilation
c   scheme.
      open (unit=78,file='swe_assim/cf.txt')
      do k=1,nstns
        write (78,*) corr_factor_tmp(k)
      enddo
      close (78)

c These are the obs and correction factors calculated from the
c   first loop in SnowModel.
      open (721,file=fname_sweobs)
      open (731,file='swe_assim/cf.txt')

      read (721,*) nstns
      do k=1,nstns
        read (721,*) stnid,xstn(k),ystn(k),dummy
        read (731,*) var(k)
c       if (var(k).gt.1.5) then
c         print *, 'cf > 1.5 found; setting to 1.5',k,var(k)
c         var(k) = 1.5
c       endif
c       if (var(k).lt.0.5) then
c         print *, 'cf < 0.5 found; setting to 0.5',k,var(k)
c         var(k) = 0.5
c       endif
c       var(k) = min(1.5,var(k))
c       var(k) = max(0.5,var(k))
      enddo
      close (721)
      close (731)

c Create a collection of 'stations' with correction factors of
c   1.0 in areas outside of our traverse regions.
      open(741,file=fname_sweobs_barnes_mask,
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (741,rec=1) ((xmask(i,j),i=1,nx),j=1,ny)
      close (741)

c Create an array of e, n coordinates for this domain.
      do j=1,ny
        do i=1,nx
          xg(i,j) = xmn + deltax * (real(i) - 1.0)
          yg(i,j) = ymn + deltay * (real(j) - 1.0)
        enddo
      enddo

      do j=1,ny
        do i=1,nx
          if (xmask(i,j).ne.undef) then
            xg(i,j) = undef
            yg(i,j) = undef
          endif
        enddo
      enddo

c Count how many cf=1.0 'stations' you are going to end up with.
      icount = 0
      do j=1,ny,100
        do i=1,nx,100
          if (xg(i,j).ne.undef) then
            icount = icount + 1
          endif
        enddo
      enddo

c Write out the original stations.
      open (761,file='swe_assim/cf_with_mask.txt')
      ntstations = nstns + icount
      write (761,88) ntstations
      do k=1,nstns
        write (761,89) k,xstn(k),ystn(k),var(k)
      enddo

c Write out the cf=1.0 stations.
      icount = 0
      do j=1,ny,100
        do i=1,nx,100
          if (xg(i,j).ne.undef) then
            icount = icount + 1
            write (761,89) icount+1000,xg(i,j),yg(i,j),1.0
          endif
        enddo
      enddo
      close (761)

c Read in the new local cf data.
      open (79,file='swe_assim/cf_with_mask.txt')
      read (79,*) nstns2
      do k=1,nstns2
        read (79,*) obsid(k),xstn(k),ystn(k),cf_tmp2(k)
      enddo
      close (79)

      call get_dn(nx,ny,deltax,deltay,nstns2,dn,iobsint)

      dn = beta * dn

      call barnes_oi(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns2,xstn,ystn,cf_tmp2,dn,grid,undef,ifill)

c Write the output data to a grads file.
      open(511,file='swe_assim/corr_factor_w-mask.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      write(511,rec=1) ((grid(i,j),i=1,nx),j=1,ny)
      close (511)

  88  format (i10)
  89  format (i10,2f15.1,f10.4)

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

      subroutine mk_cf_prec_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  nobs_total,nyears)

      implicit none

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,nobs_total,nyears
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_desc,trailing_blanks

      character*95 output_fname
      character*80 filename,description
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      data description /
     &  'cf    0  0 precip correction factor (above and below 1.0)'/

      undef = -9999.0

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert the write interval from seconds to hours or a day.
      igrads_dt = 1
      cdt = 'dy'

      filename = 'swe_assim/corr_factor.ctl'
      output_fname = 'corr_factor.gdat'

      len_desc = 80 - trailing_blanks(description)

      open (71,file=filename)

      write (71,51) output_fname

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) nobs_total+nyears,nint(xhour_init),
     &  iday_init,cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59) description(1:len_desc)
      write (71,60)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   51 format ('DSET ^',a)
   52 format ('TITLE SnowModel data assimilation precip corr factor')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS     1')
   59 format (a)
   60 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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

c enbal_code.f

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE ENBAL_CODE(nx,ny,Tair_grid,uwind_grid,sfc_pressure,
     &  vwind_grid,rh_grid,Tsfc,Qsi_grid,Qli_grid,Qle,Qh,Qe,
     &  Qc,Qm,e_balance,Qf,snow_d,ht_windobs,icond_flag,
     &  albedo,snow_z0,veg_z0,vegtype,undef,albedo_snow_forest,
     &  albedo_snow_clearing,albedo_glacier,snod_layer,T_old,
     &  gamma,KK)

      implicit none

      include 'snowmodel.inc'

      real Tair_grid(nx_max,ny_max)
      real rh_grid(nx_max,ny_max)
      real uwind_grid(nx_max,ny_max)
      real vwind_grid(nx_max,ny_max)
      real Qsi_grid(nx_max,ny_max)
      real Qli_grid(nx_max,ny_max)
      real albedo(nx_max,ny_max)
      real vegtype(nx_max,ny_max)
      real veg_z0(nx_max,ny_max)

      real Tsfc(nx_max,ny_max),Qle(nx_max,ny_max),
     &  Qh(nx_max,ny_max),Qe(nx_max,ny_max),Qc(nx_max,ny_max),
     &  Qm(nx_max,ny_max),e_balance(nx_max,ny_max),Qf(nx_max,ny_max),
     &  snow_d(nx_max,ny_max),sfc_pressure(nx_max,ny_max)

      real snow_z0,veg_z0_tmp,windspd,ht_windobs,undef,
     &  albedo_snow_forest,albedo_snow_clearing,albedo_glacier,
     &  count_Tsfc_not_converged

      integer i,j,nx,ny,icond_flag,k

      integer KK(nx_max,ny_max)
      real snod_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real gamma(nx_max,ny_max,nz_max)
      real snod_layer_z(2)
      real T_old_z(2)
      real gamma_z(2)

c     print *,'   solving the energy balance'

      count_Tsfc_not_converged = 0.0

      do j=1,ny
        do i=1,nx

          windspd = sqrt(uwind_grid(i,j)**2+vwind_grid(i,j)**2)

c Prevent the problem of low wind speeds in the logarithmic wind
c   profile calculations.
          windspd = max(1.0,windspd)

          veg_z0_tmp = veg_z0(i,j)

c Extract the vertical column for this i,j point, and send it
c   to the subroutine. *** Note that I should use f95, then I would
c   not have to do this (I could pass in subsections of the arrays).
          if (icond_flag.eq.1) then
            do k=1,2
              snod_layer_z(k) = snod_layer(i,j,k)
              T_old_z(k) = T_old(i,j,k)
              gamma_z(k) = gamma(i,j,k)
            enddo
          endif

          CALL ENBAL_CORE(Tair_grid(i,j),windspd,rh_grid(i,j),
     &      Tsfc(i,j),Qsi_grid(i,j),Qli_grid(i,j),Qle(i,j),Qh(i,j),
     &      Qe(i,j),Qc(i,j),Qm(i,j),e_balance(i,j),Qf(i,j),undef,
     &      sfc_pressure(i,j),snow_d(i,j),ht_windobs,
     &      icond_flag,albedo(i,j),snow_z0,veg_z0_tmp,vegtype(i,j),
     &      albedo_snow_forest,albedo_snow_clearing,albedo_glacier,
     &      snod_layer_z,T_old_z,gamma_z,KK(i,j),
     &      count_Tsfc_not_converged)
        enddo
      enddo

c Calculate the % of the grid cells that did not converge during
c   this time step.
      count_Tsfc_not_converged = 100.0* count_Tsfc_not_converged /
     &  real(nx*ny)

c Set the not-converged threshold to be 1% of the grid cells.
      if (count_Tsfc_not_converged.gt.1.0) then
        print *,'Over 1% of the grid cells failed to converge'
        print *,'  in the Tsfc energy balance calculation. This'
        print *,'  usually means there in a problem with the'
        print *,'  atmopheric forcing inputs, or that windspd_min'
        print *,'  in snowmodel.par is set too low; like less than'
        print *,'  1 m/s.'
        print *
        print *,'% Tsfc not converged = ',count_Tsfc_not_converged
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE ENBAL_CORE(Tair,windspd,rh,
     &    Tsfc,Qsi,Qli,Qle,Qh,
     &    Qe,Qc,Qm,e_balance,Qf,undef,
     &    sfc_pressure,snow_d,ht_windobs,
     &    icond_flag,albedo,snow_z0,veg_z0_tmp,vegtype,
     &    albedo_snow_forest,albedo_snow_clearing,albedo_glacier,
     &    snod_layer_z,T_old_z,gamma_z,KK,
     &    count_Tsfc_not_converged)

c This is the FORTRAN code which implements the surface energy
c   balance model used in:
c
c   (1) "Local Advection of Momentum, Heat, and Moisture during the
c       Melt of Patchy Snow Covers", G. E. Liston, J. Applied 
c       Meteorology, 34, 1705-1715, 1995.
c
c   (2) "An Energy-Balance Model of Lake-Ice Evolution", G. E.
c       Liston, D. K. Hall, J. of Glaciology, (41), 373-382, 1995.
c
c   (3) "Sensitivity of Lake Freeze-Up and Break-Up to Climate
c       Change: A Physically Based Modeling Study", G. E. Liston,
c       D. K. Hall, Annals of Glaciology, (21), 387-393, 1995.
c
c   (4) "Below-Surface Ice Melt on the Coastal Antarctic Ice
c       Sheet", G. E. Liston, and 4 others, J. of Glaciology, (45),
c       273-285, 1999.
c
c This version runs at sub-hourly to daily time steps.
c
c In addition, this version includes the influence of direct and
c   diffuse solar radiation, and the influence of topographic 
c   slope and aspect on incoming solar radiation.

      implicit none

      real Tair,windspd,rh,Tsfc,Qsi,Qli,Qle,Qh,Qe,Qc,Qm,e_balance,
     &  Qf,sfc_pressure,snow_d,ht_windobs,albedo,snow_z0,
     &  veg_z0_tmp,vegtype,emiss_sfc,Stef_Boltz,ro_air,Cp,gravity,
     &  xls,xkappa,xLf,Tf,ro_water,Cp_water,ro_ice,z_0,ea,de_h,
     &  stability,es0,undef,albedo_snow_forest,albedo_snow_clearing,
     &  albedo_glacier,count_Tsfc_not_converged

      integer icond_flag,KK

      real snod_layer_z(2)
      real T_old_z(2)
      real gamma_z(2)

c Define the constants used in the computations.
        CALL CONSTS_ENBAL(emiss_sfc,Stef_Boltz,ro_air,Cp,gravity,
     &    xls,xkappa,xLf,Tf,ro_water,Cp_water,ro_ice)

c Define the surface characteristics based on the snow conditions.
        CALL GET_SFC(snow_d,albedo,z_0,Tf,Tair,snow_z0,veg_z0_tmp,
     &    vegtype,albedo_snow_forest,albedo_snow_clearing,
     &    albedo_glacier)

c Atmospheric vapor pressure from relative humidity data.
        CALL VAPPRESS(ea,rh,Tair,Tf)

c Compute the turbulent exchange coefficients.
        CALL EXCOEFS(De_h,z_0,ht_windobs,windspd,xkappa)

c Compute the flux contribution due to conduction.
        CALL CONDUCT(icond_flag,Qc,snod_layer_z,T_old_z,gamma_z,
     &    KK)

c Solve the energy balance for the surface temperature.
        CALL SFCTEMP(Tsfc,Tair,Qsi,Qli,ea,albedo,De_h,
     &    sfc_pressure,ht_windobs,windspd,ro_air,Cp,emiss_sfc,
     &    Stef_Boltz,gravity,xLs,xkappa,z_0,Tf,Qc,
     &    count_Tsfc_not_converged)

c Make sure the snow surface temperature is <= 0 C.
        CALL MELTTEMP(Tsfc,Tf,snow_d,vegtype)

c Compute the stability function.
        CALL STABLEFN(stability,Tair,Tsfc,windspd,ht_windobs,
     &    gravity,xkappa,z_0)

c Compute the water vapor pressure at the surface.
        CALL VAPOR(es0,Tsfc,Tf)

c Compute the latent heat flux.
        CALL LATENT(Qe,De_h,stability,ea,es0,ro_air,xLs,
     &    sfc_pressure,undef,snow_d,vegtype)

c Compute the sensible heat flux.
        CALL SENSIBLE(Qh,De_h,stability,Tair,Tsfc,ro_air,Cp,
     &    undef,snow_d,vegtype)

c Compute the longwave flux emitted by the surface.
        CALL LONGOUT(Qle,Tsfc,emiss_sfc,Stef_Boltz)

c Compute the energy flux available for melting or freezing.
        CALL MFENERGY(albedo,Qsi,Qli,Qle,Qh,Qe,Qc,Qm,Qf,Tsfc,
     &    Tf,Tair,windspd,ht_windobs,gravity,De_h,ea,ro_air,xLs,
     &    sfc_pressure,Cp,emiss_sfc,Stef_Boltz,snow_d,xkappa,z_0,
     &    vegtype,icond_flag,undef,snod_layer_z,T_old_z,gamma_z,KK)

c Perform an energy balance check.
        CALL ENBAL(e_balance,albedo,Qsi,Qli,Qle,Qh,Qe,Qc,Qm,
     &    undef,snow_d,vegtype)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE VAPPRESS(ea,rh,Tair,Tf)

      implicit none

      real A,B,C,ea,rh,Tair,Tf

c Also see the VAPOR subroutine.

c Coeffs for saturation vapor pressure over water (Buck 1981).
c   Note: temperatures for Buck`s equations are in deg C, and
c   vapor pressures are in mb.  Do the adjustments so that the
c   calculations are done with temperatures in K, and vapor
c   pressures in Pa.

c Because I am interested in sublimation over snow during the
c   winter, do these calculations over ice.

c Over water.
c       A = 6.1121 * 100.0
c       B = 17.502
c       C = 240.97
c Over ice.
        A = 6.1115 * 100.0
        B = 22.452
        C = 272.55

c Atmospheric vapor pressure from relative humidity data.
      ea = rh / 100.0 * A * exp((B * (Tair - Tf))/(C + (Tair - Tf)))

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MFENERGY(albedo,Qsi,Qli,Qle,Qh,Qe,Qc,Qm,Qf,Tsfc,
     &  Tf,Tair,windspd,ht_windobs,gravity,De_h,ea,ro_air,xLs,
     &  sfc_pressure,Cp,emiss_sfc,Stef_Boltz,snow_d,xkappa,z_0,
     &  vegtype,icond_flag,undef,snod_layer_z,T_old_z,gamma_z,KK)

      implicit none

      real albedo,Qsi,Qli,Qle,Qh,Qe,Qc,Qm,Qf,Tsfc,Tf,Tair,windspd,
     &  ht_windobs,gravity,De_h,ea,ro_air,xLs,sfc_pressure,Cp,
     &  emiss_sfc,Stef_Boltz,snow_d,xkappa,z_0,vegtype,xTsfc,
     &  xstability,xes0,xQe,xqh,xQle,xQc,undef

      integer icond_flag,KK

      real snod_layer_z(2)
      real T_old_z(2)
      real gamma_z(2)

c If Qm is > 0, then this is the energy available for melting.
c   If Qm is < 0, then this is the energy available for freezing
c   liquid water in the snowpack.
      if (snow_d.gt.0.0 .and. Tsfc.eq.Tf) then
        Qm = (1.0-albedo) * Qsi + Qli + Qle + Qh + Qe + Qc
      elseif (vegtype.eq.20.0 .and. Tsfc.eq.Tf) then
        Qm = (1.0-albedo) * Qsi + Qli + Qle + Qh + Qe + Qc
      else
        Qm = 0.0
      endif

      if (Tsfc.lt.Tf) then
        xTsfc = Tf
        CALL STABLEFN(xstability,Tair,xTsfc,windspd,ht_windobs,
     &    gravity,xkappa,z_0)
        CALL VAPOR(xes0,xTsfc,Tf)
        CALL LATENT(xQe,De_h,xstability,ea,xes0,ro_air,xLs,
     &    sfc_pressure,undef,snow_d,vegtype)
        CALL SENSIBLE(xQh,De_h,xstability,Tair,xTsfc,ro_air,Cp,
     &    undef,snow_d,vegtype)
        CALL LONGOUT(xQle,xTsfc,emiss_sfc,Stef_Boltz)
        CALL CONDUCT(icond_flag,xQc,snod_layer_z,T_old_z,gamma_z,
     &    KK)
        Qf = (1.0-albedo) * Qsi + Qli + xQle + xQh + xQe + xQc
      else
        Qf = 0.0
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MELTTEMP(Tsfc,Tf,snow_d,vegtype)

      implicit none

      real Tsfc,snow_d,vegtype,Tf

      if (snow_d.gt.0.0 .or. vegtype.eq.20.0) then
        if (Tsfc.gt.Tf) Tsfc = Tf
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CONDUCT(icond_flag,Qc,snod_layer_z,T_old_z,gamma_z,
     &  KK)

      implicit none

      integer icond_flag,KK

      real Qc
      real snod_layer_z(2)
      real T_old_z(2)
      real gamma_z(2)

      if (icond_flag.eq.0) then
        Qc = 0.0
      else
        if (KK.le.1) then
          Qc = 0.0
        else
          Qc = - (gamma_z(1) + gamma_z(2))/2.0 *
     &      (T_old_z(1) - T_old_z(2)) /
     &      (snod_layer_z(1) + snod_layer_z(2))/2.0
        endif
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CONSTS_ENBAL(emiss_sfc,Stef_Boltz,ro_air,Cp,gravity,
     &  xLs,xkappa,xLf,Tf,ro_water,Cp_water,ro_ice)

      implicit none

      real emiss_sfc,Stef_Boltz,ro_air,Cp,gravity,xLs,xkappa,xLf,
     &  Tf,ro_water,Cp_water,ro_ice

      emiss_sfc = 0.98
      Stef_Boltz = 5.6696e-8
      ro_air = 1.275
      Cp = 1004.
      gravity = 9.81
      xLs = 2.500e6
      xkappa = 0.4
      xLf = 3.34e5
      Tf = 273.15
      ro_water = 1000.0
      Cp_water = 4180.0
      ro_ice = 917.0

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE ENBAL(e_balance,albedo,Qsi,Qli,Qle,Qh,Qe,Qc,Qm,
     &  undef,snow_d,vegtype)

      implicit none

      real e_balance,albedo,Qsi,Qli,Qle,Qh,Qe,Qc,Qm,undef,snow_d,
     & vegtype

      if (snow_d.gt.0.0 .or. vegtype.eq.20.0) then
        e_balance = (1.0-albedo)*Qsi + Qli + Qle + Qh + Qe + Qc - Qm 
      else
        e_balance = undef
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE STABLEFN(stability,Tair,Tsfc,windspd,ht_windobs,
     &  gravity,xkappa,z_0)

      implicit none

      real C1,C2,B1,B2,stability,Tair,Tsfc,windspd,ht_windobs,
     &  gravity,xkappa,z_0,B8,B3,z_0_tmp

      z_0_tmp = min(0.25*ht_windobs,z_0)
      C1 = 5.3 * 9.4 * (xkappa/(log(ht_windobs/z_0_tmp)))**2 *
     &  sqrt(ht_windobs/z_0_tmp)
      C2 = gravity * ht_windobs / (Tair * windspd**2)
      B1 = 9.4 * C2
      B2 = C1 * sqrt(C2)

      if (Tsfc.gt.Tair) then
c Unstable case.
        B3 = 1.0 + B2 * sqrt(Tsfc - Tair)
        stability = 1.0 + B1 * (Tsfc - Tair) / B3
      elseif (Tsfc.lt.Tair) then
c Stable case.
        B8 = B1 / 2.0
        stability = 1.0 / ((1.0 + B8 * (Tair - Tsfc))**2)
      else
c Neutrally stable case.
        stability = 1.0
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE LONGOUT(Qle,Tsfc,emiss_sfc,Stef_Boltz)

      implicit none

      real Qle,emiss_sfc,Stef_Boltz,Tsfc

      Qle = (- emiss_sfc) * Stef_Boltz * Tsfc**4

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SENSIBLE(Qh,De_h,stability,Tair,Tsfc,ro_air,Cp,
     &  undef,snow_d,vegtype)

      implicit none

      real Qh,De_h,stability,Tair,Tsfc,ro_air,Cp,undef,snow_d,vegtype

      if (snow_d.gt.0.0 .or. vegtype.eq.20.0) then
        Qh = ro_air * Cp * De_h * stability * (Tair - Tsfc)
      else
        Qh = undef
c To do this, see the laps snow-shrub parameterization runs.
c       Qh = ro_air * Cp * De_h * stability * (Tair - Tsfc)
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SFCTEMP(Tsfc,Tair,Qsi,Qli,ea,albedo,De_h,
     &  sfc_pressure,ht_windobs,windspd,ro_air,Cp,emiss_sfc,
     &  Stef_Boltz,gravity,xLs,xkappa,z_0,Tf,Qc,
     &  count_Tsfc_not_converged)

      implicit none

      real Tsfc,Tair,Qsi,Qli,ea,albedo,De_h,sfc_pressure,ht_windobs,
     &  windspd,ro_air,Cp,emiss_sfc,Stef_Boltz,gravity,xLs,xkappa,
     &  z_0,Tf,Qc,AAA,CCC,DDD,EEE,FFF,C1,C2,B1,B2,z_0_tmp,
     &  count_Tsfc_not_converged

      AAA = ro_air * Cp * De_h
      CCC = 0.622 / sfc_pressure
      DDD = emiss_sfc * Stef_Boltz
      EEE = (1.0-albedo) * Qsi + Qli + Qc
      FFF = ro_air * xLs * De_h

c Compute the constants used in the stability coefficient
c   computations.
      z_0_tmp = min(0.25*ht_windobs,z_0)
      C1 = 5.3 * 9.4 * (xkappa/(log(ht_windobs/z_0_tmp)))**2 *
     &  sqrt(ht_windobs/z_0_tmp)
      C2 = gravity * ht_windobs / (Tair * windspd**2)
      B1 = 9.4 * C2
      B2 = C1 * sqrt(C2)

      CALL SOLVE(Tsfc,Tair,ea,AAA,CCC,DDD,EEE,FFF,B1,B2,Tf,
     &  count_Tsfc_not_converged)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SOLVE(xnew,Tair,ea,AAA,CCC,DDD,EEE,FFF,B1,B2,Tf,
     &  count_Tsfc_not_converged)

      implicit none

      integer maxiter,i

      real tol,old,A,B,C,other1,other2,es0,dother1,dother2,xnew,
     &  Tair,ea,AAA,CCC,DDD,EEE,FFF,B1,B2,Tf,B3,stability,
     &  dstability,fprime1,fprime2,fprime3,fprime4,B8,funct,
     &  fprime,count_Tsfc_not_converged

      tol = 1.0e-2
      maxiter = 20
      old = Tair

c Because I am interested in sublimation over snow during the
c   winter, do these calculations over ice.

c Over water.
c       A = 6.1121 * 100.0
c       B = 17.502
c       C = 240.97
c Over ice.
        A = 6.1115 * 100.0
        B = 22.452
        C = 272.55

      do i=1,maxiter

c This section accounts for an increase in turbulent fluxes
c   under unstable conditions.
        other1 = AAA * (Tair - old)
        es0 = A * exp((B * (old - Tf))/(C + (old - Tf)))
        other2 = FFF*CCC*(ea-es0)

        dother1 = - AAA
        dother2 = (- FFF)*CCC*es0*B*C/((C + (old - Tf))**2)

      if (old.gt.Tair) then
c Unstable case.
        B3 = 1.0 + B2 * sqrt(old - Tair)
        stability = 1.0 + B1 * (old - Tair) / B3
        dstability = B1/B3 - (B1*B2*(old-Tair))/
     &    (2.0*B3*B3*sqrt(old-Tair))
        fprime1 = (- 4.0)*DDD*old**3
        fprime2 = stability * dother1 + other1 * dstability
        fprime3 = stability * dother2 + other2 * dstability
        fprime4 = - 0.0

      elseif (old.lt.Tair) then
c Stable case.
        B8 = B1 / 2.0
        stability = 1.0 / ((1.0 + B8 * (Tair - old))**2)
        dstability = 2.0 * B8 / ((1.0 + B8 * (Tair - old))**3)
        fprime1 = (- 4.0)*DDD*old**3
        fprime2 = stability * dother1 + other1 * dstability
        fprime3 = stability * dother2 + other2 * dstability
        fprime4 = - 0.0

      else
c Neutrally stable case.
        stability = 1.0
        fprime1 = (- 4.0)*DDD*old**3
        fprime2 = dother1
        fprime3 = dother2
        fprime4 = - 0.0
      endif

        funct = EEE - DDD*old**4 + AAA*(Tair-old)*stability +
     &    FFF*CCC*(ea-es0)*stability +
     &    0.0
        fprime = fprime1 + fprime2 + fprime3 + fprime4

        xnew = old - funct/fprime

        if (abs(xnew - old).lt.tol) return
        old = xnew

      enddo

c If the maximum iterations are exceeded, send a message and set
c   the surface temperature to the air temperature.
c     write (*,102)
c 102 format('max iteration exceeded when solving for Tsfc')

c Count the number of times the model did not converge for this
c   time step.
      count_Tsfc_not_converged = count_Tsfc_not_converged + 1.0

      xnew = Tair

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE LATENT(Qe,De_h,stability,ea,es0,ro_air,xLs,
     &  sfc_pressure,undef,snow_d,vegtype)

      implicit none

      real Qe,De_h,stability,ea,es0,ro_air,xLs,sfc_pressure,undef,
     &  snow_d,vegtype
  
      if (snow_d.gt.0.0 .or. vegtype.eq.20.0) then
        Qe = ro_air * xLs * De_h * stability *
     &    (0.622/sfc_pressure * (ea - es0))
      else
        Qe = undef
c To do this, see the laps snow-shrub parameterization runs.
c       Qe = ro_air * xLs * De_h * stability *
c    &    (0.622/sfc_pressure * (ea - es0))
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE EXCOEFS(De_h,z_0,ht_windobs,windspd,xkappa)

      implicit none

      real De_h,z_0,ht_windobs,windspd,xkappa,z_0_tmp

      z_0_tmp = min(0.25*ht_windobs,z_0)
      De_h = (xkappa**2) * windspd / ((log(ht_windobs/z_0_tmp))**2)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE VAPOR(es0,Tsfc,Tf)

      implicit none

      real es0,Tsfc,Tf,A,B,C

c Coeffs for saturation vapor pressure over water (Buck 1981).
c   Note: temperatures for Buck`s equations are in deg C, and
c   vapor pressures are in mb.  Do the adjustments so that the
c   calculations are done with temperatures in K, and vapor
c   pressures in Pa.

c Because I am interested in sublimation over snow during the
c   winter, do these calculations over ice.

c Over water.
c       A = 6.1121 * 100.0
c       B = 17.502
c       C = 240.97
c Over ice.
        A = 6.1115 * 100.0
        B = 22.452
        C = 272.55

c Compute the water vapor pressure at the surface.
      es0 = A * exp((B * (Tsfc - Tf))/(C + (Tsfc - Tf)))

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GET_SFC(snow_d,albedo,z_0,Tf,Tair,snow_z0,veg_z0_tmp,
     &  vegtype,albedo_snow_forest,albedo_snow_clearing,
     &  albedo_glacier)

      implicit none

      real snow_d,albedo,z_0,Tf,Tair,snow_z0,veg_z0_tmp,vegtype,
     &  albedo_veg,albedo_snow_forest,albedo_snow_clearing,
     &  albedo_glacier
c     real scf

c Note that this is very crude for many reasons, and should be
c   improved.  See below for an alternative solution.

      albedo_veg = 0.15

c Define the albedo and roughness length.
      if (snow_d.gt.0.0) then
c Snow.
        z_0 = snow_z0
        if (Tair.gt.Tf) then
          if (vegtype.le.5.0) then
c Melting.  Assume, because of leaf litter, etc., that the snow
c   albedo under a forest canopy is different than in a clearing.
            albedo = albedo_snow_forest
          else
            albedo = albedo_snow_clearing
          endif
c For thin melting snowcovers (less than 15 cm), reduce the albedo
c   to account for the observed enhanced melting processes.
c         scf = min(1.0,snow_d/0.15)
c         albedo = scf * albedo + (1.0 - scf) * albedo_veg
        else
c Dry.
          albedo = 0.8
        endif

c No snow.
      else
        if (vegtype.eq.20.0) then
c Glacier or permanant snow.
          z_0 = snow_z0
          albedo = albedo_glacier
        else
c Land.
          z_0 = veg_z0_tmp
          if (vegtype.le.5.0) then
            z_0 = 0.2
          elseif (vegtype.le.11.0) then
            z_0 = 0.04
          else
            z_0 = 0.02
          endif
          albedo = albedo_veg
        endif
      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c The code below simulates the TIME-EVOLUTION OF SNOW ALBEDO in
c   response to precipitation and melt. 

c The general model equations are described in the paper:
c   Modeling Snow Depth for Improved Simulation of
c   Snow-Vegetion-Atmosphere Interactions.
c   Strack, J. E., G. E. Liston, and R. A. Pielke, Sr., 2004,
c   Journal of Hydrometeorology, 5, 723-734.

c     implicit none
c     real albedo
c     real Tair
c     real prec
c     real al_gr_cold
c     real al_gr_melt
c     real al_min
c     real al_max
c     real dt,tau_1

c Model time step.
c     dt = 3600.
c     dt = 86400.

c Maximum albedo, minimum albedo, gradient cold snow albedo	  
c   gradient melting snow albedo
c     al_max = 0.8
c     al_min = 0.5
c     al_gr_cold = 0.008
c     al_gr_melt = 0.24
c     tau_1 = 86400.

c Define the initial condition.
c     albedo = al_max

c     do iter=1,maxiter

c In the presence of precipitation, re-initialize the albedo.
c       if (prec.gt.0.003) albedo = al_max

c Evolve the snow albedo.
c       CALL SNOW_ALBEDO(Tair,albedo,al_gr_cold,al_min,
c    &    al_gr_melt,dt,tau_1)

c     enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     SUBROUTINE SNOW_ALBEDO(Tair,albedo,al_gr_cold,al_min,
c    &  al_gr_melt,dt,tau_1)
     
c     implicit none
c     real Tair
c     real albedo
c     real al_gr_cold
c     real al_min
c     real al_gr_melt
c     real dt,tau_1
      
c     if (Tair.le.0.0) then
c       albedo = albedo - (al_gr_cold * dt / tau_1)
c       albedo = max(albedo,al_min)
c     else 
c       albedo = (albedo - al_min) * exp(-al_gr_melt * dt / tau_1) +
c    &    al_min
c     endif

c     return
c     end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c micromet_code.f

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine MICROMET_CODE(nx,ny,xmn,ymn,deltax,deltay,
     &  iyear_init,imonth_init,iday_init,xhour_init,dt,undef,
     &  ifill,iobsint,dn,iter,curve_len_scale,slopewt,curvewt,
     &  topo,curvature,terrain_slope,slope_az,Tair_grid,
     &  rh_grid,uwind_grid,vwind_grid,Qsi_grid,prec_grid,
     &  i_tair_flag,i_rh_flag,i_wind_flag,i_solar_flag,
     &  i_prec_flag,isingle_stn_flag,igrads_metfile,
     &  windspd_grid,winddir_grid,windspd_flag,winddir_flag,
     &  sprec,windspd_min,Qli_grid,i_longwave_flag,vegtype,
     &  forest_LAI,iyear,imonth,iday,xhour,corr_factor,
     &  icorr_factor_index,lapse_rate_user_flag,
     &  iprecip_lapse_rate_user_flag,use_shortwave_obs,
     &  use_longwave_obs,use_sfc_pressure_obs,sfc_pressure,
     &  run_enbal,run_snowpack,calc_subcanopy_met,vegsnowd_xy,
     &  gap_frac,cloud_frac_factor,barnes_lg_domain,n_stns_used,
     &  k_stn,xlat_grid,xlon_grid,UTC_flag,icorr_factor_loop,
     &  snowmodel_line_flag,xg_line,yg_line,irun_data_assim,
     &  wind_lapse_rate,iprecip_scheme,cf_precip_flag,cf_precip,
     &  cloud_frac_grid,snowfall_frac,seaice_run)

      implicit none

      include 'snowmodel.inc'

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn  ! center x coords of lower left grid cell
      double precision ymn  ! center y coords of lower left grid cell
      integer nstns_orig ! number of input values

      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

      double precision xstn_orig(nstns_max)     ! input stn x coords
      double precision ystn_orig(nstns_max)     ! input stn y coords
      real Tair_orig(nstns_max)     ! input values
      real rh_orig(nstns_max)       ! input values
      real winddir_orig(nstns_max)  ! input values
      real windspd_orig(nstns_max)  ! input values
      real prec_orig(nstns_max)     ! input values
      real elev_orig(nstns_max)     ! station elevation
      real dn                  ! average observation spacing
      real topo(nx_max,ny_max) ! grid topography
      real xlat_grid(nx_max,ny_max) ! lat (dec deg) of cell centers
      real xlon_grid(nx_max,ny_max) ! lon (dec deg) of cell centers

      real Tair_grid(nx_max,ny_max)   ! output values
      real rh_grid(nx_max,ny_max)     ! output values
      real uwind_grid(nx_max,ny_max)  ! output, E-W wind component
      real vwind_grid(nx_max,ny_max)  ! output, N-S wind component
      real windspd_grid(nx_max,ny_max)
      real winddir_grid(nx_max,ny_max)
      real Qsi_grid(nx_max,ny_max)    ! output
      real Qli_grid(nx_max,ny_max)    ! output
      real prec_grid(nx_max,ny_max)   ! output
      real sprec(nx_max,ny_max)   ! output
      real sfc_pressure(nx_max,ny_max)

      integer iyear,imonth,iday  ! model year, month, and day
      real xhour                 ! model decimal hour
      real dt                    ! model time step, in seconds
      integer iter               ! model iteration
      integer iyear_init     ! model start year
      integer imonth_init    ! model start month
      integer iday_init      ! model start day
      real xhour_init        ! model start hour
      integer J_day          ! model Julian day, actually day-of-year

      real undef       ! undefined value
      integer ifill    ! flag (=1) forces a value in every cell
      integer iobsint  ! flag (=1) use dn value from .par file

      real curvature(nx_max,ny_max)     ! topographic curvature
      real slope_az(nx_max,ny_max)      ! azimuth of topographic slope
      real terrain_slope(nx_max,ny_max) ! terrain slope
      real vegtype(nx_max,ny_max)
      real vegsnowd_xy(nx_max,ny_max)
      real topo_ref_grid(nx_max,ny_max) ! reference surface

      real curve_len_scale   ! length scale for curvature calculation
      real slopewt           ! wind model slope weight
      real curvewt           ! wind model curvature weight

      integer i_tair_flag,i_rh_flag,i_wind_flag,i_solar_flag,
     &  i_prec_flag,i_longwave_flag,isingle_stn_flag,igrads_metfile,
     &  lapse_rate_user_flag,iprecip_lapse_rate_user_flag,n_stns_used,
     &  icorr_factor_loop,irun_data_assim,iprecip_scheme

      real windspd_flag,winddir_flag,windspd_min,calc_subcanopy_met,
     &  T_lapse_rate,Td_lapse_rate,precip_lapse_rate,
     &  use_shortwave_obs,use_longwave_obs,use_sfc_pressure_obs,
     &  run_enbal,run_snowpack,gap_frac,cloud_frac_factor,
     &  barnes_lg_domain,UTC_flag,wind_lapse_rate

      integer nftypes
      parameter (nftypes=5)
      real forest_LAI(nftypes)

      real corr_factor(nx_max,ny_max,max_obs_dates+1)
      integer icorr_factor_index(max_time_steps)
      integer k_stn(nx_max,ny_max,9)

      real cf_precip(nx_max,ny_max)
      real cf_precip_flag

      real cloud_frac_grid(nx_max,ny_max)
      real snowfall_frac

      real seaice_run
      integer i,j,irec,irec_day

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Calculate what the current simulation date should be.
      call get_model_time(iyear_init,imonth_init,iday_init,
     &  xhour_init,iter,dt,iyear,imonth,iday,xhour,J_day)

      if (irun_data_assim.eq.1) then
        write (*,150) icorr_factor_loop,iyear,imonth,iday,xhour
  150   format('In Assim Loop #',i1,';  WORKING ON MODEL TIME =',
     &    i5,2i4,f6.1)
      else
        write (*,151) iyear,imonth,iday,xhour
  151   format('                   WORKING ON MODEL TIME =',
     &    i5,2i4,f6.1)
      endif

c Read in the observations for this time step, and build an array of
c   valid observations to be interpolated.
      call get_obs_data(nstns_orig,Tair_orig,rh_orig,xstn_orig,
     &  ystn_orig,elev_orig,iyear,imonth,iday,xhour,undef,
     &  windspd_orig,winddir_orig,prec_orig,isingle_stn_flag,
     &  igrads_metfile,iter)

c Make the topographic calculations required by the wind and solar
c   radiation models.  These calculations are not fixed in time
c   because as the snow depth evolves it modifies the "topography".
      call topo_data(nx,ny,deltax,deltay,topo,
     &  curvature,terrain_slope,slope_az,curve_len_scale)

c Calculate the temperature and dew-point lapse rates to be used in
c   the interpoations.
      call get_lapse_rates(imonth,iday,T_lapse_rate,
     &  Td_lapse_rate,xlat_grid(1,1),lapse_rate_user_flag,
     &  precip_lapse_rate,iprecip_lapse_rate_user_flag)

c Calculate the forest lai for each of the five forest types, and
c   for this day of the simulation (the lai varies seasonally for
c   the case of deciduous trees).
      call get_lai(J_day,forest_LAI)

c If this is a Lagrangian sea ice parcel trajectory simulation,
c   extract the lat-lon of the parcels at this time step.  Note
c   that these files have daily data in them, so the record has
c   to be adjusted to account for this.
      if (seaice_run.eq.4.0) then 

        if (iter.eq.1) then 
          open (91,file='extra_met/grid_lat_time.gdat',
     &      form='unformatted',access='direct',recl=4*nx*ny)
          open (92,file='extra_met/grid_lon_time.gdat',
     &      form='unformatted',access='direct',recl=4*nx*ny)
        endif

        if (dt.eq.86400.0) then 
          irec = iter 
        elseif (dt.eq.10800.0) then 
          call get_daily_irec (iter,dt,irec_day)
          irec = irec_day
        else
          print *,'This has not been set up to work on dt'
          print *,'  values other than 1 day and 3-hours.'
          print *,'  dt =',dt
          stop
        endif

        read (91,rec=irec) ((xlat_grid(i,j),i=1,nx),j=1,ny)
        read (92,rec=irec) ((xlon_grid(i,j),i=1,nx),j=1,ny)

      endif

c TEMPERATURE.
      if (i_tair_flag.eq.1) then
c       print *,'   solving for temperature'
        call temperature(nx,ny,deltax,deltay,xmn,ymn,
     &    nstns_orig,xstn_orig,ystn_orig,Tair_orig,dn,Tair_grid,
     &    undef,ifill,iobsint,iyear,imonth,iday,xhour,elev_orig,
     &    topo,T_lapse_rate,barnes_lg_domain,n_stns_used,k_stn,
     &    snowmodel_line_flag,xg_line,yg_line,seaice_run)
      endif

c RELATIVE HUMIDITY.
      if (i_rh_flag.eq.1) then
c       print *,'   solving for relative humidity'
        call relative_humidity(nx,ny,deltax,deltay,xmn,ymn,
     &    nstns_orig,xstn_orig,ystn_orig,rh_orig,dn,rh_grid,undef,
     &    ifill,iobsint,iyear,imonth,iday,xhour,elev_orig,topo,
     &    Tair_orig,Tair_grid,Td_lapse_rate,barnes_lg_domain,
     &    n_stns_used,k_stn,snowmodel_line_flag,xg_line,yg_line,
     &    seaice_run)
      endif

c WIND SPEED AND DIRECTION.
      if (i_wind_flag.eq.1) then
c       print *,'   solving for wind speed and direction'
        call wind(nx,ny,deltax,deltay,xmn,ymn,windspd_orig,
     &    nstns_orig,xstn_orig,ystn_orig,dn,undef,ifill,
     &    iobsint,iyear,imonth,iday,xhour,elev_orig,
     &    winddir_orig,uwind_grid,vwind_grid,slopewt,curvewt,
     &    curvature,slope_az,terrain_slope,windspd_grid,
     &    winddir_grid,windspd_flag,winddir_flag,windspd_min,
     &    vegtype,forest_LAI,calc_subcanopy_met,vegsnowd_xy,
     &    barnes_lg_domain,n_stns_used,k_stn,snowmodel_line_flag,
     &    xg_line,yg_line,topo_ref_grid,topo,wind_lapse_rate,
     &    curve_len_scale,seaice_run)

c Provide the ability to read in an alternate wind dataset that
c   has been previously generated with another program, like
c   NUATMOS.  The following assumes you are reading in a single
c   file with u and v values.  The file name is hardcoded here.
      elseif (i_wind_flag.eq.-1) then
        call read_wind_file(nx,ny,iter,uwind_grid,vwind_grid,
     &    windspd_grid,winddir_grid,windspd_flag,winddir_flag,
     &    windspd_min)
      endif

c SOLAR RADIATION.
      if (i_solar_flag.eq.1) then
c       print *,'   solving for solar radiation'
        call solar(nx,ny,xhour,J_day,topo,rh_grid,Tair_grid,
     &    xlat_grid,Qsi_grid,slope_az,terrain_slope,dt,vegtype,
     &    forest_LAI,T_lapse_rate,Td_lapse_rate,
     &    calc_subcanopy_met,gap_frac,cloud_frac_factor,UTC_flag,
     &    xlon_grid,cloud_frac_grid)

c If requested, modify the model output to account for shortwave
c   radiation observations.
        if (use_shortwave_obs.eq.1.0) then
          if (barnes_lg_domain.eq.1.0) then
            print *,'The model is not configured to assimilate'
            print *,'  solar data with barnes_lg_domain = 1.0.'
            stop
          endif
          call shortwave_data(nx,ny,deltax,deltay,xmn,ymn,
     &      iyear,imonth,iday,xhour,undef,Qsi_grid,iter)
        endif
      endif

c INCOMING LONGWAVE RADIATION.
      if (i_longwave_flag.eq.1) then
c       print *,'   solving for incoming longwave radiation'
        call longwave(nx,ny,rh_grid,Tair_grid,Qli_grid,topo,
     &    vegtype,forest_LAI,T_lapse_rate,Td_lapse_rate,
     &    calc_subcanopy_met,cloud_frac_factor)

c If requested, modify the model output to account for longwave
c   radiation observations.
        if (use_longwave_obs.eq.1.0) then
          if (barnes_lg_domain.eq.1.0) then
            print *,'The model is not configured to assimilate'
            print *,'  longwave data with barnes_lg_domain = 1.0.'
            stop
          endif
          call longwave_data(nx,ny,deltax,deltay,xmn,ymn,
     &      iyear,imonth,iday,xhour,undef,Qli_grid,iter)
        endif
      endif

c PRECIPITATION.
      if (i_prec_flag.eq.1) then
c       print *,'   solving for precipitation'
        call precipitation(nx,ny,deltax,deltay,xmn,ymn,
     &    nstns_orig,xstn_orig,ystn_orig,prec_orig,dn,prec_grid,
     &    undef,ifill,iobsint,iyear,imonth,iday,xhour,elev_orig,
     &    topo,Tair_grid,sprec,corr_factor,icorr_factor_index,iter,
     &    precip_lapse_rate,barnes_lg_domain,n_stns_used,k_stn,
     &    snowmodel_line_flag,xg_line,yg_line,topo_ref_grid,
     &    iprecip_scheme,cf_precip_flag,cf_precip,snowfall_frac,
     &    seaice_run)
      endif

c SURFACE PRESSURE.
c Surface pressure is used in EnBal and SnowMass.  If needed for
c   this SnowModel simulation, calculate the distribution here.
      if (run_enbal.eq.1.0 .or. run_snowpack.eq.1.0) then
        call pressure(nx,ny,topo,sfc_pressure)

c If requested, modify the model output to account for surface
c   pressure observations.
        if (use_sfc_pressure_obs.eq.1.0) then
          if (barnes_lg_domain.eq.1.0) then
            print *,'The model is not configured to assimilate'
            print *,'  pressure data with barnes_lg_domain = 1.0.'
            stop
          endif
          call sfc_pressure_data(nx,ny,deltax,deltay,xmn,ymn,
     &      iyear,imonth,iday,xhour,undef,sfc_pressure,iter)
        endif
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine precipitation(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns_orig,xstn_orig,ystn_orig,prec_orig,dn,prec_grid,
     &  undef,ifill,iobsint,iyear,imonth,iday,xhour,elev_orig,
     &  topo,Tair_grid,sprec,corr_factor,icorr_factor_index,iter,
     &  precip_lapse_rate,barnes_lg_domain,n_stns_used,k_stn,
     &  snowmodel_line_flag,xg_line,yg_line,topo_ref_grid,
     &  iprecip_scheme,cf_precip_flag,cf_precip,snowfall_frac,
     &  seaice_run)

c Interpolate the observed precipitation values to the grid.  Also
c   interpolate the station elevations to a reference surface.  Use
c   a precipitation "lapse rate", or adjustment factor to define
c   the precipitation on the actual elevation grid.  The reason the
c   interpolated station elevations are used as the topographic
c   reference surface (instead of something like sea level), is
c   because the precipitation adjustment factor is a non-linear 
c   function of elevation difference.
 
c The adjustment factor that is used comes from: Thornton, P. E.,
c   S. W. Running, and M. A. White, 1997: Generating surfaces of
c   daily meteorological variables over large regions of complex
c   terrain.  J. Hydrology, 190, 214-251.

      implicit none

      include 'snowmodel.inc'

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn  ! center x coords of lower left grid cell
      double precision ymn  ! center y coords of lower left grid cell

      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

      integer nstns        ! number of input values, all good
      integer nstns_orig   ! number of input values
      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords
      real prec(nstns_max) ! input values
      real elev(nstns_max) ! station elevation
      real undef           ! undefined value

      double precision xstn_orig(nstns_max) ! input stn x coords
      double precision ystn_orig(nstns_max) ! input stn y coords
      real elev_orig(nstns_max) ! station elevation
      real prec_orig(nstns_max) ! input values

      real dn                           ! average observation spacing
      real topo(nx_max,ny_max)          ! grid topography
      real prec_grid(nx_max,ny_max)     ! output values
      real Tair_grid(nx_max,ny_max)     ! input values
      real sprec(nx_max,ny_max)         ! output values
      real topo_ref_grid(nx_max,ny_max) ! reference surface

      integer ifill    ! flag (=1) forces a value in every cell
      integer iobsint  ! flag (=1) use dn value from .par file

      integer iyear,imonth,iday  ! model year, month, and day
      real xhour                 ! model decimal hour

      real delta_topo,alfa,Tf,precip_lapse_rate_m,precip_lapse_rate,
     &  barnes_lg_domain
      integer i,j,iter,n_stns_used
      integer k_stn(nx_max,ny_max,9)

      real corr_factor(nx_max,ny_max,max_obs_dates+1)
      integer icorr_factor_index(max_time_steps)
      integer iprecip_scheme

      real cf_precip(nx_max,ny_max)
      real cf_precip_flag

      real Tair_C,Tair_C_center,slope,b,snowfall_frac
      real snowfall_frac_1,snowfall_frac_2,snowfall_frac_3

      real seaice_run

c Filter through the original input data, and eliminate any
c   missing values.
      call get_good_values1(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,prec_orig,prec)

c Use the barnes oi scheme to interpolate the station elevation data
c   to the grid, so that it can be used as a topographic reference
c   surface.
      call interpolate(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,elev,dn,topo_ref_grid,undef,ifill,iobsint,
     &  iyear,imonth,iday,xhour,barnes_lg_domain,n_stns_used,
     &  k_stn,snowmodel_line_flag,xg_line,yg_line,seaice_run)

c Use the barnes oi scheme to interpolate the station data to
c   the grid.
      call interpolate(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,prec,dn,prec_grid,undef,ifill,iobsint,
     &  iyear,imonth,iday,xhour,barnes_lg_domain,n_stns_used,
     &  k_stn,snowmodel_line_flag,xg_line,yg_line,seaice_run)

c Convert the precipitation "lapse rate" (km-1) to m-1.
      precip_lapse_rate_m = precip_lapse_rate / 1000.0

c Choose between Glen's original precipitation increase with
c   elevation scheme, and Ward van Pelt's scheme used in our
c   Svalbard simulations (see van Pelt et al. 2016).

c This is Glen's original MicroMet precipitation adjustment
c   scheme.
      if (iprecip_scheme.eq.1) then

        do j=1,ny
          do i=1,nx

c Convert the gridded station data to the SnowModel-grid elevations.
            delta_topo = topo(i,j) - topo_ref_grid(i,j)

c Don't let the elevation difference be greater than some number
c  (like 1800 meters gives a factor of 4.4).  If it is too large
c   you get huge precipitation adjustment, a divide by zero, or
c   even negative adjustments for high elevations).
            delta_topo = min(delta_topo,1800.0)
            alfa = precip_lapse_rate_m * delta_topo
            prec_grid(i,j) = prec_grid(i,j) * (1.0 + alfa)/(1.0 - alfa)

          enddo
        enddo

c This is van Pelt's precipitation adjustment scheme.
      elseif (iprecip_scheme.eq.2) then

        do j=1,ny
          do i=1,nx
     
c Don't correct precipitation above 1000 m a.s.l..
            if (topo_ref_grid(i,j).le.1000.0) then
              if (topo(i,j).gt.1000.0) then
                delta_topo = 1000.0 - topo_ref_grid(i,j)
              else
                delta_topo = topo(i,j) - topo_ref_grid(i,j)
              endif
            endif
            if (topo_ref_grid(i,j).gt.1000.0) then
              if (topo(i,j).gt.1000.0) then
                delta_topo = 0.0 
              else
                 delta_topo = topo(i,j) - 1000.0
              endif
            endif
                
c Don't let the elevation difference be greater than some number
c  (like 1800 meters gives a factor of 4.4).  If it is too large
c   you get huge precipitation adjustment, a divide by zero, or
c   even negative adjustments for high elevations).
            delta_topo = min(delta_topo,1800.0)
            alfa = 1.75 * delta_topo / 1000.0
            prec_grid(i,j) = prec_grid(i,j) * max(1.0+alfa,0.1)

          enddo
        enddo

      endif

c Convert the precipitation values from mm to m swe.  Also, make
c   sure the interpolation has not created any negetive
c   precipitation values.
      do j=1,ny
        do i=1,nx
          prec_grid(i,j) = prec_grid(i,j) / 1000.0
          prec_grid(i,j) = max(0.0,prec_grid(i,j))
        enddo
      enddo

c This is my original code:
c Use the temperature distribution to define whether this
c   precipitation is falling as rain or snow (generate a
c   snow-precipitation array following the air temperature
c   threshold defined by Auer (1974) = 2.0 C).  Note here that,
c   if you ever want it, rain_prec = prec_grid - sprec.  This
c   snow precipitation (sprec) is assumed to be in meters
c   snow-water-equivalent per time step.
c     Tf = 273.15
c     do j=1,ny
c       do i=1,nx
c         if (Tair_grid(i,j).lt.2.0+Tf) then
c           if (icorr_factor_index(iter).gt.0) then
c             prec_grid(i,j) =
c    &          corr_factor(i,j,icorr_factor_index(iter)) *
c    &          prec_grid(i,j)
c           else
c             prec_grid(i,j) = prec_grid(i,j)
c           endif
c           sprec(i,j) = prec_grid(i,j)
c         else
c           sprec(i,j) = 0.0
c         endif
c       enddo
c     enddo

c First apply any precipitation correction factors calculated as
c   part of any data assimilation.
      do j=1,ny
        do i=1,nx
          if (icorr_factor_index(iter).gt.0) then
            prec_grid(i,j) =
     &        corr_factor(i,j,icorr_factor_index(iter)) *
     &        prec_grid(i,j)
          endif
        enddo
      enddo

c Apply the user-defined precipitation correction factor, if one
c   exists.
      if (cf_precip_flag.ne.0.0) then
        do j=1,ny
          do i=1,nx
            prec_grid(i,j) = cf_precip(i,j) * prec_grid(i,j)
          enddo
        enddo
      endif

c Now calculate whether the precipiation is falling as rain or
c   snow, and how much of each.
      Tf = 273.15

c Auer (1974); a rain-snow threshold at +2.0 C.
      if (snowfall_frac.eq.1.0) then

        do j=1,ny
          do i=1,nx
            Tair_C = Tair_grid(i,j) - Tf
            if (Tair_C.lt.2.0) then
              snowfall_frac_1 = 1.0
            else
              snowfall_frac_1 = 0.0
            endif
            sprec(i,j) = snowfall_frac_1 * prec_grid(i,j)
          enddo
        enddo

c Dai, A. (2008): Temperature and pressure dependence of the
c   rain-snow phase transition over land and ocean, Geophys.
c   Res. Lett., 35, L12802, doi:10.1029/2008GL033295.
c In this implementation I have clipped Dai's values to 0
c   and 1.
      elseif (snowfall_frac.eq.2.0) then

        do j=1,ny
          do i=1,nx
            Tair_C = Tair_grid(i,j) - Tf
            snowfall_frac_2 = - 0.482292 *
     &        (tanh(0.7205 * (Tair_C - 1.1662)) - 1.0223) 
            if (Tair_C.lt.-4.0) then
              snowfall_frac_2 = 1.0
            elseif (Tair_C.gt.6.0) then
              snowfall_frac_2 = 0.0
            endif
            sprec(i,j) = snowfall_frac_2 * prec_grid(i,j)
          enddo
        enddo

c Glen's linear approximation to Dai (2008).  This plots right
c   over the top of Dai, between frac = 0.1 and 0.9.
      elseif (snowfall_frac.eq.3.0) then

c Define where you want the center temperature to be when frac = 0.5.
        Tair_C_center = 1.1
c Define the slope of the line.
        slope = -0.30
c Calculate the intercept (the 0.5 is the frac for Tair_C_center).
        b = 0.5 - slope * Tair_C_center

        do j=1,ny
          do i=1,nx
            Tair_C = Tair_grid(i,j) - Tf

c Solve the equation in the form y = m*x + b
            snowfall_frac_3 = slope * Tair_C + b
            snowfall_frac_3 = max(0.0,snowfall_frac_3)
            snowfall_frac_3 = min(1.0,snowfall_frac_3)

            sprec(i,j) = snowfall_frac_3 * prec_grid(i,j)
          enddo
        enddo

      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine longwave(nx,ny,rh_grid,Tair_grid,Qli_grid,topo,
     &  vegtype,forest_LAI,T_lapse_rate,Td_lapse_rate,
     &  calc_subcanopy_met,cloud_frac_factor)

      implicit none

      include 'snowmodel.inc'

      integer nx       ! number of x output values
      integer ny       ! number of y output values

      real Tair_grid(nx_max,ny_max)
      real rh_grid(nx_max,ny_max)
      real Qli_grid(nx_max,ny_max)
      real topo(nx_max,ny_max)
      real vegtype(nx_max,ny_max)

      real T_lapse_rate,Td_lapse_rate,es,e,emiss_cloud,
     &  A,B,C,Tf,Stef_Boltz,cloud_frac,E1,X1,Y1,Z1,E2,X2,Y2,Z2,
     &  Xs,Ys,Zs,forest_frac,E3,X3,Y3,Z3,alfa,calc_subcanopy_met,
     &  cloud_frac_factor

      integer nftypes
      parameter (nftypes=5)
      real forest_LAI(nftypes)

      integer i,j,nveg

c Coeffs for saturation vapor pressure over water (Buck 1981).
c   Note: temperatures for Buck`s equations are in deg C, and
c   vapor pressures are in mb.  Do the adjustments so that the
c   calculations are done with temperatures in K, and vapor
c   pressures in Pa.
      A = 6.1121 * 100.0
      B = 17.502
      C = 240.97

c Over ice.
c     A = 6.1115 * 100.0
c     B = 22.452
c     C = 272.55

c Define the freezing temperature to be used to convert from C to K.
      Tf = 273.15

c Define the Stefan Boltzmann constant.
      Stef_Boltz = 5.6696e-8

c Constants required for Iziomon et al. (2003).
      E1 = 200.0
      X1 = 0.35
      Y1 = 0.100
      Z1 = 0.224

      E2 = 1500.0
      X2 = 0.43
      Y2 = 0.115
      Z2 = 0.320

c Assume the X and Y coefficients increase linearly to 3000 m,
c   and adjust Z to create a best fit to the CLPX data.
      E3 = 3000.0
      X3 = 0.51
      Y3 = 0.130
      Z3 = 1.100

      do j=1,ny
        do i=1,nx

c Compute the cloud fraction.
          call get_cloudfrac(Tair_grid(i,j),rh_grid(i,j),topo(i,j),
     &      cloud_frac,T_lapse_rate,Td_lapse_rate,cloud_frac_factor)

c Calculate the vapor pressure.
          es = A * exp((B * (Tair_grid(i,j) - Tf))/
     &      (C + (Tair_grid(i,j) - Tf)))
          e = es * rh_grid(i,j) / 100.0

c Compute Qli following Iziomon et al. (2003).
          if (topo(i,j).lt.E1) then
            Xs = X1
            Ys = Y1
            Zs = Z1
          elseif (topo(i,j).gt.E2) then
            Xs = X3
            Ys = Y3
            Zs = Z3
          else
            Xs = X1 + (topo(i,j) - E1) * (X3 - X1)/(E3 - E1)
            Ys = Y1 + (topo(i,j) - E1) * (Y3 - Y1)/(E3 - E1)
            Zs = Z1 + (topo(i,j) - E1) * (Z3 - Z1)/(E3 - E1)
          endif

          alfa = 1.083
          emiss_cloud = alfa *
     &      (1.0 - Xs * exp((- Ys) * e/Tair_grid(i,j))) *
     &      (1.0 + Zs * cloud_frac**2)
          emiss_cloud = min(1.0,emiss_cloud)

          Qli_grid(i,j) = emiss_cloud * Stef_Boltz * Tair_grid(i,j)**4

c Modify the incoming longwave radiation for the forest canopy.
          if (vegtype(i,j).le.5.0) then
            if (calc_subcanopy_met.eq.1.0) then

c Define the forest-canopy parameters.
              nveg = nint(vegtype(i,j))
              if (forest_LAI(nveg).lt.0.2) then
                forest_frac = 0.5 * forest_LAI(nveg)
              else
                forest_frac =
     &            min(1.0,0.55 + 0.29 * log(forest_LAI(nveg)))
              endif
              Qli_grid(i,j) = Qli_grid(i,j) * (1.0 - forest_frac) +
     &          (Stef_Boltz * Tair_grid(i,j)**4) * forest_frac
            endif
          endif

        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine solar(nx,ny,xhour,J_day,topo,rh_grid,Tair_grid,
     &  xlat_grid,Qsi_grid,slope_az,terrain_slope,dt,vegtype,
     &  forest_LAI,T_lapse_rate,Td_lapse_rate,
     &  calc_subcanopy_met,gap_frac,cloud_frac_factor,UTC_flag,
     &  xlon_grid,cloud_frac_grid)

c First take the surface gridded fields of Tair and RH, and
c   calculate Td for the topographic surface.  Then use Tair and
c   Td, and the associated lapse rates, and calculate Tair and Td
c   for the 700 mb level.  Use these surfaces to calculate RH at
c   700 mb, and convert these values to cloud fraction following
c   Walcek, C. J., 1994: Cloud cover and its relationship to
c   relative humidity during a spring midlatitude cyclone.  Mon.
c   Wea. Rev., 122, 1021-1035.

      implicit none

      include 'snowmodel.inc'

      integer nx       ! number of x output values
      integer ny       ! number of y output values

      real topo(nx_max,ny_max)
      real Tair_grid(nx_max,ny_max)
      real rh_grid(nx_max,ny_max)
      real Qsi_grid(nx_max,ny_max)
      real slope_az(nx_max,ny_max)
      real terrain_slope(nx_max,ny_max)
      real vegtype(nx_max,ny_max)
      real xlat_grid(nx_max,ny_max)
      real xlon_grid(nx_max,ny_max)
      real cloud_frac_grid(nx_max,ny_max)

      real xhour                 ! model decimal hour
      real xxhour
      integer J_day              ! model day of year
      integer i,j,ihrs_day,ihour,nveg
      real dt,cloud_frac,Qsi_tmp,Qsi_sum,trans_veg,
     &  T_lapse_rate,Td_lapse_rate,calc_subcanopy_met,gap_frac,
     &  cloud_frac_factor,UTC_flag

      integer nftypes
      parameter (nftypes=5)
      real forest_LAI(nftypes)

      ihrs_day = 24

      do j=1,ny
        do i=1,nx

c Compute the cloud fraction.
          call get_cloudfrac(Tair_grid(i,j),rh_grid(i,j),topo(i,j),
     &      cloud_frac,T_lapse_rate,Td_lapse_rate,cloud_frac_factor)

c Save a copy of the cloud fraction distribution.
          cloud_frac_grid(i,j) = cloud_frac

c Compute the incoming solar radiation.  The solar_rad subroutine
c   calculates the instantaneous incoming solar radiation, so
c   if the time step is very long, account for this by calculating
c   the incoming solar radiation every 3 hours and then taking the
c   average.
          if (dt.le.10800.0) then
            call solar_rad(Qsi_grid(i,j),J_day,xlat_grid(i,j),
     &        cloud_frac,xhour,slope_az(i,j),terrain_slope(i,j),
     &        UTC_flag,xlon_grid(i,j))
          elseif (dt.eq.86400.0) then
            Qsi_sum = 0.0
            do ihour=3,ihrs_day,3
              xxhour = real(ihour)
              call solar_rad(Qsi_tmp,J_day,xlat_grid(i,j),
     &          cloud_frac,xxhour,slope_az(i,j),terrain_slope(i,j),
     &          UTC_flag,xlon_grid(i,j))
                Qsi_sum = Qsi_sum + Qsi_tmp
            enddo
            Qsi_grid(i,j) = Qsi_sum / (real(ihrs_day)/3.0)
          else
            print *,'The model may not do what you want with this dt'
            stop
          endif

c Modify the incoming solar radiation for the forest canopy.
          if (vegtype(i,j).le.5.0) then
            if (calc_subcanopy_met.eq.1.0) then

c Define the forest-canopy transmissivity.  0.71 provided a
c   best-fit to the observations, when averaged over the two years
c   of hourly data.
              nveg = nint(vegtype(i,j))
              trans_veg = exp((- 0.71) * forest_LAI(nveg))

c Account for any gaps in the forest canopy that will allow
c   direct incoming solar radiation to reach the snow surface.
              trans_veg = gap_frac * (1.0 - trans_veg) + trans_veg

              Qsi_grid(i,j) = trans_veg * Qsi_grid(i,j)
            endif
          endif

        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_cloudfrac(Tair_grid,rh_grid,topo,
     &  cloud_frac,T_lapse_rate,Td_lapse_rate,cloud_frac_factor)

      implicit none

      real Td_lapse_rate,topo_ref,delta_topo,A,B,C,e,es,dx,
     &  T_lapse_rate,press_ratio,f_max,one_minus_RHe,f_1,
     &  Td_700,Tair_700,rh_700,cloud_frac,Tair_grid,rh_grid,
     &  topo,Td_grid,Tf,cloud_frac_factor

c Coeffs for saturation vapor pressure over water (Buck 1981).
c   Note: temperatures for Buck`s equations are in deg C, and
c   vapor pressures are in mb.  Do the adjustments so that the
c   calculations are done with temperatures in K, and vapor
c   pressures in Pa.
      A = 6.1121 * 100.0
      B = 17.502
      C = 240.97

c Over ice.
c     A = 6.1115 * 100.0
c     B = 22.452
c     C = 272.55

c Define the freezing temperature to be used to convert from C to K.
      Tf = 273.15

c Assume that 700 mb is equivalent to 3000 m in a standard
c   atmosphere.
      topo_ref = 3000.0

c Define the ratio of 700 mb level pressure to the surface pressure
c   (~1000 mb).
      press_ratio = 0.7

c Assume dx = 80.0 km, for Walcek (1994).
      dx = 80.0

c Walcek coefficients.
      f_max = 78.0 + 80.0/15.5
      one_minus_RHe = 0.196 + (0.76-80.0/2834.0) * (1.0 - press_ratio)
      f_1 = f_max * (press_ratio - 0.1) / 0.6 / 100.0

c Convert the gridded topo-surface RH to Td.
      es = A * exp((B * (Tair_grid - Tf))/(C + (Tair_grid - Tf)))
      e = es * max(10.0,rh_grid) / 100.0
      Td_grid = C * log(e/A) / (B - log(e/A)) + Tf

c Convert the topo-surface temperature values to 700 mb values.
c     delta_topo = topo - topo_ref
      delta_topo = topo_ref - topo
      Td_700 = Td_grid + Td_lapse_rate * delta_topo
      Tair_700 = Tair_grid + T_lapse_rate * delta_topo

c Convert each Td to a gridded relative humidity (0-1).
      e = A * exp((B * (Td_700 - Tf))/(C + (Td_700 - Tf)))
      es = A * exp((B * (Tair_700 - Tf))/(C + (Tair_700 - Tf)))
      rh_700 = e/es
      rh_700 = min(1.0,rh_700)
      rh_700 = max(0.0,rh_700)

c Use this RH at 700 mb to define the cloud fraction (0-1).
      cloud_frac = f_1 * exp((rh_700 - 1.0)/one_minus_RHe)

c If the user wants to, reduce the calculate cloud fraction by the
c   cloud_frac_factor.
      cloud_frac = cloud_frac_factor * cloud_frac

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine solar_rad(Qsi,J_day,xlat,
     &  cloud_frac,xxhour,slope_az,terrain_slope,
     &  UTC_flag,xlon)

      implicit none

      integer J_day

      real solar_const,days_yr,Trop_Can,solstice,pi,deg2rad,
     &  Qsi_direct,Qsi_diffuse,cos_i,cos_Z,Qsi,xlat,sin_z,xxhour,
     &  cloud_frac,slope_az,terrain_slope,sol_dec,hr_angl,
     &  trans_direct,trans_diffuse,Qsi_trans_dir,Qsi_trans_dif,
     &  sun_azimuth,slope_az_S0,xxxhour,UTC_flag,xlon

c Required constants.
      solar_const = 1370.
      days_yr = 365.25
      Trop_Can = 0.41
      solstice = 173.
      pi = 2.0 * acos(0.0)
      deg2rad = pi / 180.0

c COMPUTE THE BASIC SOLAR RADIATION PARAMETERS.

c Compute the solar declination angle (radians).
      sol_dec = Trop_Can *
     &  cos(2.*pi * (real(J_day) - solstice)/days_yr)
      
c For the case of running UTC time and a latitudinal variation
c   in solar radiation, adjust the time to correspond to the
c   local time at this longitude position.
      if (UTC_flag.ne.0.0) then
        xxxhour = xxhour + xlon / 15.0
        if (xxxhour.ge.24.0) xxxhour = xxxhour - 24.0
        if (xxxhour.lt.0.0) xxxhour = xxxhour + 24.0
      else
        xxxhour = xxhour
      endif

c Compute the sun's hour angle (radians).
      hr_angl = (xxxhour * 15.0 - 180.0) * deg2rad

c Compute cos_Z.  Note that the sin of the solar elevation angle,
c   sin_alfa, is equal to the cosine of the solar zenith angle,
c   cos_Z.
      cos_Z = sin(sol_dec) * sin(xlat * deg2rad) + 
     &  cos(sol_dec) * cos(xlat * deg2rad) * cos(hr_angl)
      cos_Z = max(0.0,cos_Z)

c Account for clouds, water vapor, pollution, etc.
      trans_direct = (0.6 + 0.2 * cos_Z) * (1.0 - cloud_frac)
      trans_diffuse = (0.3 + 0.1 * cos_Z) * cloud_frac

c Compute the solar radiation transmitted through the atmosphere.
      Qsi_trans_dir = solar_const * trans_direct
      Qsi_trans_dif = solar_const * trans_diffuse

c COMPUTE THE CORRECTIONS TO ALLOW FOR TOPOGRAPHIC SLOPE AND ASPECT.

c The sine of the solar zenith angle.
      sin_Z = sqrt(1.0 - cos_Z*cos_Z)

c Azimuth of the sun, with south having zero azimuth for the
c   northern hemisphere.
      sun_azimuth = 
     &  asin(max(-1.0,min(1.0,cos(sol_dec)*sin(hr_angl)/sin_Z)))
      if (xlat.lt.0.0) then
        sun_azimuth = - sun_azimuth
      endif

c Make the corrections so that the angles below the local horizon
c   are still measured from the normal to the slope.
      if (xlat.ge.0.0) then
        if (hr_angl.lt.0.0) then
          if (hr_angl.lt.sun_azimuth) sun_azimuth = - pi - sun_azimuth
        elseif (hr_angl.gt.0.0) then
          if (hr_angl.gt.sun_azimuth) sun_azimuth = pi - sun_azimuth
        endif
      else
c       if (hr_angl.lt.0.0) then
c         if (hr_angl.lt.sun_azimuth) sun_azimuth = - pi - sun_azimuth
c       elseif (hr_angl.gt.0.0) then
c         if (hr_angl.gt.sun_azimuth) sun_azimuth = pi - sun_azimuth
c       endif
      endif

c Build, from the variable with north having zero azimuth, a 
c   slope_azimuth value with south having zero azimuth.  Also
c   make north have zero azimuth if in the southern hemsisphere.
      if (xlat.ge.0.0) then
        if (slope_az.ge.180.0) then
          slope_az_S0 = slope_az - 180.0
        else
          slope_az_S0 = slope_az + 180.0
        endif
      else
        slope_az_S0 = slope_az
      endif

c Compute the angle between the normal to the slope and the angle
c   at which the direct solar radiation impinges on the sloping
c   terrain (radians).
      cos_i = cos(terrain_slope * deg2rad) * cos_Z + 
     &  sin(terrain_slope * deg2rad) * sin_Z * 
     &  cos(sun_azimuth - slope_az_S0 * deg2rad)

c Adjust the topographic correction due to local slope so that
c   the correction is zero if the sun is below the local horizon 
c   (i.e., the slope is in the shade) or if the sun is below the
c   global horizon.
      if (cos_i.lt.0.0) cos_i = 0.0
      if (cos_Z.le.0.0) cos_i = 0.0

c Adjust the solar radiation for slope, etc.
      Qsi_direct = cos_i * Qsi_trans_dir
      Qsi_diffuse = cos_Z * Qsi_trans_dif

c Combine the direct and diffuse solar components.
      Qsi = Qsi_direct + Qsi_diffuse

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wind(nx,ny,deltax,deltay,xmn,ymn,windspd_orig,
     &  nstns_orig,xstn_orig,ystn_orig,dn,undef,ifill,
     &  iobsint,iyear,imonth,iday,xhour,elev_orig,
     &  winddir_orig,uwind_grid,vwind_grid,slopewt,curvewt,
     &  curvature,slope_az,terrain_slope,windspd_grid,
     &  winddir_grid,windspd_flag,winddir_flag,windspd_min,
     &  vegtype,forest_LAI,calc_subcanopy_met,vegsnowd_xy,
     &  barnes_lg_domain,n_stns_used,k_stn,snowmodel_line_flag,
     &  xg_line,yg_line,topo_ref_grid,topo,wind_lapse_rate,
     &  curve_len_scale,seaice_run)

c This program takes the station wind speed and direction, converts
c   them to u and v components, interpolates u and v to a grid,
c   converts the gridded values to speed and direction, and then
c   runs a simple wind model that adjusts those speeds and
c   directions according to topographic slope and curvature
c   relationships.  The resulting speeds and directions are
c   converted to u and v components and passed back to the main
c   program to be written to a file.  (All of the conversion
c   between u-v and speed-dir is done because of the problems
c   with interpolating over the 360/0 direction line.)

      implicit none

      include 'snowmodel.inc'

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn  ! center x coords of lower left grid cell
      double precision ymn  ! center y coords of lower left grid cell

      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

      integer nstns        ! number of input values, all good
      integer nstns_orig   ! number of input values
      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords
      real elev(nstns_max) ! station elevation
      real undef           ! undefined value

      double precision xstn_orig(nstns_max) ! input stn x coords
      double precision ystn_orig(nstns_max) ! input stn y coords
      real elev_orig(nstns_max) ! station elevation

      real windspd_orig(nstns_max) ! input values
      real winddir_orig(nstns_max) ! input values

      real speed(nstns_max)  ! input values
      real dir(nstns_max)    ! input values
      real u(nstns_max)      ! u component of wind
      real v(nstns_max)      ! v component of wind

      real dn                  ! average observation spacing
      real uwind_grid(nx_max,ny_max)  ! output values
      real vwind_grid(nx_max,ny_max)  ! output values
      real u_grid(nx_max,ny_max)  ! temporary u wind component
      real v_grid(nx_max,ny_max)  ! temporary v wind component
      real winddir_grid(nx_max,ny_max)  ! temporary wind direction
      real windspd_grid(nx_max,ny_max)  ! temporary wind speed

      real curvature(nx_max,ny_max)     ! topographic curvature
      real slope_az(nx_max,ny_max)      ! azimuth of topographic slope
      real terrain_slope(nx_max,ny_max) ! terrain slope
      real vegtype(nx_max,ny_max)
      real vegsnowd_xy(nx_max,ny_max)
      real topo_ref_grid(nx_max,ny_max) ! reference surface
      real topo(nx_max,ny_max)

      integer ifill    ! flag (=1) forces a value in every cell
      integer iobsint  ! flag (=1) use dn value from .par file

      integer iyear,imonth,iday  ! model year, month, and day
      real xhour                 ! model decimal hour

      real pi,deg2rad,rad2deg,slopewt,curvewt,curve_len_scale
      integer i,j,k,n_stns_used
      integer k_stn(nx_max,ny_max,9)
      real windspd_flag,winddir_flag,u_sum,v_sum,windspd_min,
     &  calc_subcanopy_met,barnes_lg_domain,wind_lapse_rate,
     &  delta_topo,alfa1,alfa2

      integer nftypes
      parameter (nftypes=5)
      real forest_LAI(nftypes)
      real seaice_run

c Define the required constants.
      pi = 2.0 * acos(0.0)
      deg2rad = pi / 180.0
      rad2deg = 180.0 / pi

c Filter through the original input data, and eliminate any
c   missing values (i.e., make sure each wind direction is paired
c   up with a wind speed.
      call get_good_values2(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,windspd_orig,winddir_orig,
     &  speed,dir)

c Convert these station data to u and v wind components.
      do k=1,nstns
        speed(k) = max(windspd_min,speed(k))
        u(k) = (- speed(k)) * sin(deg2rad * dir(k))
        v(k) = (- speed(k)) * cos(deg2rad * dir(k))
      enddo

c Use the barnes oi scheme to interpolate the station data to
c   the grid.
c U component.
      call interpolate(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,u,dn,u_grid,undef,ifill,iobsint,
     &  iyear,imonth,iday,xhour,barnes_lg_domain,n_stns_used,
     &  k_stn,snowmodel_line_flag,xg_line,yg_line,seaice_run)

c V component.
      call interpolate(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,v,dn,v_grid,undef,ifill,iobsint,
     &  iyear,imonth,iday,xhour,barnes_lg_domain,n_stns_used,
     &  k_stn,snowmodel_line_flag,xg_line,yg_line,seaice_run)

c If desired, impose a wind speed increase with elevation.  Here
c   the wind_lapse_rate = the wind speed increase per 1-km elevation
c   gain.  The adjustment is patterned after the precipitation-
c   elevation adjustment.  topo_ref_grid here comes from the
c   precipitation adjustment.
      if (wind_lapse_rate.ne.0.0) then
        alfa1 = (wind_lapse_rate - 1.0) / (1.0 + wind_lapse_rate)
c Convert to m-1.
        alfa1 = alfa1 / 1000.0
        do j=1,ny
          do i=1,nx
            delta_topo = topo(i,j) - topo_ref_grid(i,j)
c Impose some limits to the adjustment.
            delta_topo = min(delta_topo,1800.0)
            alfa2 = alfa1 * delta_topo
            u_grid(i,j) = u_grid(i,j) * (1.0 + alfa2)/(1.0 - alfa2)
            v_grid(i,j) = v_grid(i,j) * (1.0 + alfa2)/(1.0 - alfa2)
          enddo
        enddo
      endif

c Convert these u and v components to speed and directions.
      do j=1,ny
        do i=1,nx

c Some compilers do not allow both u and v to be 0.0 in
c   the atan2 computation.
          if (abs(u_grid(i,j)).lt.1e-10) u_grid(i,j) = 1e-10

          winddir_grid(i,j) = rad2deg * atan2(u_grid(i,j),v_grid(i,j))
          if (winddir_grid(i,j).ge.180.0) then
            winddir_grid(i,j) = winddir_grid(i,j) - 180.0
          else
            winddir_grid(i,j) = winddir_grid(i,j) + 180.0
          endif

c         winddir_grid(i,j) = 270.0 -
c    &      rad2deg*atan2(v_grid(i,j),u_grid(i,j))
c         if (winddir_grid(i,j).ge.360.0)
c    &      winddir_grid(i,j) = winddir_grid(i,j)-360.0

          windspd_grid(i,j) = sqrt(u_grid(i,j)**2 + v_grid(i,j)**2)
        enddo
      enddo

c Modify the wind speed and direction according to simple
c   wind-topography relationships.
      call topo_mod_winds(nx,ny,winddir_grid,slopewt,curvewt,
     &  windspd_grid,uwind_grid,vwind_grid,curvature,slope_az,
     &  terrain_slope,vegtype,forest_LAI,calc_subcanopy_met,
     &  vegsnowd_xy,curve_len_scale,deltax,deltay)

c Avoid problems of zero (low) winds (for example, turbulence
c   theory, log wind profile, etc., says that we must have some
c   wind.  Thus, some equations blow up when the wind speed gets
c   very small).
      do j=1,ny
        do i=1,nx
          if (windspd_grid(i,j).lt.windspd_min) then
            windspd_grid(i,j) = windspd_min
            uwind_grid(i,j) = (- windspd_grid(i,j)) *
     &        sin(deg2rad*winddir_grid(i,j))
            vwind_grid(i,j) = (- windspd_grid(i,j)) *
     &        cos(deg2rad*winddir_grid(i,j))
          endif
        enddo
      enddo

c Find the maximum wind speed in the domain, and the
c   domain-averaged wind direction.
      windspd_flag = 0.0
      u_sum = 0.0
      v_sum = 0.0
      do j=1,ny
        do i=1,nx
          windspd_flag = max(windspd_flag,windspd_grid(i,j))
          u_sum = u_sum + uwind_grid(i,j)
          v_sum = v_sum + vwind_grid(i,j)
        enddo
      enddo
      u_sum = u_sum / real(nx*ny)
      v_sum = v_sum / real(nx*ny)

c Some compilers do not allow both u and v to be 0.0 in
c   the atan2 computation.
      if (abs(u_sum).lt.1e-10) u_sum = 1e-10

      winddir_flag = rad2deg * atan2(u_sum,v_sum)
      if (winddir_flag.ge.180.0) then
        winddir_flag = winddir_flag - 180.0
      else
        winddir_flag = winddir_flag + 180.0
      endif

c     winddir_flag = 270.0 - rad2deg*atan2(v_sum,u_sum)
c     if (winddir_flag.ge.360.0) winddir_flag = winddir_flag-360.0

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine relative_humidity(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns_orig,xstn_orig,ystn_orig,rh_orig,dn,rh_grid,undef,
     &  ifill,iobsint,iyear,imonth,iday,xhour,elev_orig,topo,
     &  Tair_orig,Tair_grid,Td_lapse_rate,barnes_lg_domain,
     &  n_stns_used,k_stn,snowmodel_line_flag,xg_line,yg_line,
     &  seaice_run)

c This procedure follows: Kunkel, K. E., 1989: Simple procedures for
c   extrapolation of humidity variables in the mountainous Western
c   United States. J. Climate, 2, 656-669.

c First convert stn relative humidity to dew-point temperature.  Use
c   the Td lapse rate to take the stn Td to sea level.  Interpolate
c   the stn Td to the grid.  Use the Td lapse rate to take the sea
c   level grid to the actual elevations.  Convert each Td to
c   relative humidity.

      implicit none

      include 'snowmodel.inc'

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn  ! center x coords of lower left grid cell
      double precision ymn  ! center y coords of lower left grid cell

      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

      integer nstns        ! number of input values, all good
      integer nstns_orig   ! number of input values
      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords
      real elev(nstns_max) ! station elevation
      real undef           ! undefined value

      double precision xstn_orig(nstns_max) ! input stn x coords
      double precision ystn_orig(nstns_max) ! input stn y coords
      real elev_orig(nstns_max) ! station elevation

      real Tair_orig(nstns_max)  ! input values
      real rh_orig(nstns_max)    ! input values

      real Tair(nstns_max)  ! input values
      real Td(nstns_max)    ! input values
      real rh(nstns_max)    ! input values

      real dn                  ! average observation spacing
      real topo(nx_max,ny_max) ! grid topography
      real Tair_grid(nx_max,ny_max)  ! output values
      real Td_grid(nx_max,ny_max)    ! output values
      real rh_grid(nx_max,ny_max)    ! output values

      integer ifill    ! flag (=1) forces a value in every cell
      integer iobsint  ! flag (=1) use dn value from .par file

      integer iyear,imonth,iday  ! model year, month, and day
      real xhour                 ! model decimal hour

      real Td_lapse_rate,topo_ref,delta_topo,A,B,C,e,es,Tf,
     &  barnes_lg_domain
      integer i,j,k,n_stns_used
      integer k_stn(nx_max,ny_max,9)
      real seaice_run

c Coeffs for saturation vapor pressure over water (Buck 1981).
c   Note: temperatures for Buck`s equations are in deg C.
      A = 6.1121 * 100.0
      B = 17.502
      C = 240.97

c Over ice.
c     A = 6.1115 * 100.0
c     B = 22.452
c     C = 272.55

c Define the freezing temperature to be used to convert from C to K.
      Tf = 273.15

c Filter through the original input data, and eliminate any
c   missing values.
      call get_good_values2(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,Tair_orig,rh_orig,
     &  Tair,rh)

c Convert the stn relative humidity to Td.
      do k=1,nstns

c Saturation vapor pressure at temperature, T.
        es = A * exp((B * (Tair(k) - Tf))/(C + (Tair(k) - Tf)))

c Dew point temperature for a given temperature and relative humidity.
        e = es * max(10.0,rh(k)) / 100.0
        Td(k) = C * log(e/A) / (B - log(e/A)) + Tf

      enddo

c Define the topographic reference surface.
      topo_ref = 0.0

c Convert the station data to sea level values.
      do k=1,nstns
        delta_topo = topo_ref - elev(k)
        Td(k) = Td(k) + Td_lapse_rate * delta_topo
      enddo

c Use the barnes oi scheme to interpolate the station data to
c   the grid.
      call interpolate(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,Td,dn,Td_grid,undef,ifill,iobsint,
     &  iyear,imonth,iday,xhour,barnes_lg_domain,n_stns_used,
     &  k_stn,snowmodel_line_flag,xg_line,yg_line,seaice_run)

c Convert these grid values back to the actual gridded elevations.
      do j=1,ny
        do i=1,nx
          delta_topo = topo(i,j) - topo_ref
          Td_grid(i,j) = Td_grid(i,j) + Td_lapse_rate * delta_topo
        enddo
      enddo

c Convert each Td to a gridded relative humidity.
      do j=1,ny
        do i=1,nx
          e = A * exp((B * (Td_grid(i,j) - Tf)) /
     &      (C + (Td_grid(i,j) - Tf)))
          es = A * exp((B * (Tair_grid(i,j) - Tf)) /
     &     (C + (Tair_grid(i,j) - Tf)))
          rh_grid(i,j) = 100.0 * e/es

c Make sure the interpolation processes has not created any values
c   above 100 and below 0.
          rh_grid(i,j) = min(100.0,rh_grid(i,j))
          rh_grid(i,j) = max(0.0,rh_grid(i,j))
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine temperature(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns_orig,xstn_orig,ystn_orig,Tair_orig,dn,Tair_grid,
     &  undef,ifill,iobsint,iyear,imonth,iday,xhour,elev_orig,
     &  topo,T_lapse_rate,barnes_lg_domain,n_stns_used,k_stn,
     &  snowmodel_line_flag,xg_line,yg_line,seaice_run)

c The lapse rate used depends on the month of the year, and is
c   defined by: Kunkel, K. E., 1989: Simple procedures for
c   extrapolation of humidity variables in the mountainous Western
c   United States. J. Climate, 2, 656-669.

c First adjust the stn temperatures to a common level (sea level),
c   assuming this lapse rate.  Then interpolate the temperatures
c   to the model grid.  Then use the topography data and lapse
c   rate to adjust the gridded temperatures values back to the
c   actual elevation.

c Contact Glen if you are interested in the temperature inversion
c   code presented in: Mernild, S. H., and G. E. Liston, 2010:
c   The influence of air temperature inversions on snowmelt and
c   glacier mass-balance simulations, Ammassalik Island, SE
c   Greenland. J. Applied Meteorology and Climatology, 49, 47-67.

      implicit none

      include 'snowmodel.inc'

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn  ! center x coords of lower left grid cell
      double precision ymn  ! center y coords of lower left grid cell

      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

      integer nstns        ! number of input values, all good
      integer nstns_orig   ! number of input values
      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords
      real Tair(nstns_max) ! input values
      real elev(nstns_max) ! station elevation
      real undef           ! undefined value

      double precision xstn_orig(nstns_max) ! input stn x coords
      double precision ystn_orig(nstns_max) ! input stn y coords
      real elev_orig(nstns_max) ! station elevation
      real Tair_orig(nstns_max) ! input values

      real dn                  ! average observation spacing
      real topo(nx_max,ny_max) ! grid topography
      real Tair_grid(nx_max,ny_max) ! output values

      integer ifill    ! flag (=1) forces a value in every cell
      integer iobsint  ! flag (=1) use dn value from .par file

      integer iyear,imonth,iday  ! model year, month, and day
      real xhour                 ! model decimal hour

      real T_lapse_rate,topo_ref,delta_topo,barnes_lg_domain
      integer i,j,k,n_stns_used
      integer k_stn(nx_max,ny_max,9)
      real seaice_run

c Filter through the original input data, and eliminate any
c   missing values.
      call get_good_values1(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,Tair_orig,Tair)

c Define the topographic reference surface.
      topo_ref = 0.0

c Convert the station data to sea level values.
      do k=1,nstns
        delta_topo = topo_ref - elev(k)
        Tair(k) = Tair(k) + T_lapse_rate * delta_topo
      enddo

c Use the barnes oi scheme to interpolate the station data to
c   the grid.
      call interpolate(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,Tair,dn,Tair_grid,undef,ifill,iobsint,
     &  iyear,imonth,iday,xhour,barnes_lg_domain,n_stns_used,
     &  k_stn,snowmodel_line_flag,xg_line,yg_line,seaice_run)

c Convert these grid values back to the actual gridded elevations.
      do j=1,ny
        do i=1,nx
          delta_topo = topo(i,j) - topo_ref
          Tair_grid(i,j) = Tair_grid(i,j) + T_lapse_rate * delta_topo
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine topo_mod_winds(nx,ny,winddir_grid,slopewt,curvewt,
     &  windspd_grid,uwind_grid,vwind_grid,curvature,slope_az,
     &  terrain_slope,vegtype,forest_LAI,calc_subcanopy_met,
     &  vegsnowd_xy,curve_len_scale,deltax,deltay)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,nveg,k

      real pi,deg2rad,rad2deg,slopewt,curvewt,dirdiff,curve_len_scale,
     &  wslope_max,beta,veg_ht,a,canopy_windwt,calc_subcanopy_met,
     &  deltax,deltay,xmult
      integer nftypes,loops_windwt_smoother
      parameter (nftypes=5)
      real forest_LAI(nftypes)

      real curvature(nx_max,ny_max)
      real windspd_grid(nx_max,ny_max)
      real winddir_grid(nx_max,ny_max)
      real uwind_grid(nx_max,ny_max)
      real vwind_grid(nx_max,ny_max)
      real wind_slope(nx_max,ny_max)
      real slope_az(nx_max,ny_max)
      real terrain_slope(nx_max,ny_max)
      real vegtype(nx_max,ny_max)
      real vegsnowd_xy(nx_max,ny_max)
      real windwt(nx_max,ny_max)

c Compute the wind modification factor which is a function of
c   topography and wind direction following Liston and Sturm (1998).

c Define the required constants.
      pi = 2.0 * acos(0.0)
      deg2rad = pi / 180.0
      rad2deg = 180.0 / pi

c Compute the slope in the direction of the wind.
      do i=1,nx
        do j=1,ny
          wind_slope(i,j) = deg2rad * terrain_slope(i,j) *
     &      cos(deg2rad * (winddir_grid(i,j) - slope_az(i,j)))
        enddo
      enddo

c Scale the wind slope such that the max abs(wind slope) has a value
c   of abs(0.5).  Include a 1 mm slope in slope_max to prevent
c   divisions by zero in flat terrain where the slope is zero.
      wslope_max = 0.0 + 0.001
      do j=1,ny
        do i=1,nx
          wslope_max = max(wslope_max,abs(wind_slope(i,j)))
        enddo
      enddo
      do j=1,ny
        do i=1,nx
          wind_slope(i,j) = wind_slope(i,j) / (2.0 * wslope_max)
        enddo
      enddo

c Calculate the wind speed and direction adjustments.  The
c   curvature and wind_slope values range between -0.5 and +0.5.
c   Valid slopewt and curvewt values are between 0 and 1, with
c   values of 0.5 giving approximately equal weight to slope and
c   curvature.  I suggest that slopewt and curvewt be set such
c   that slopewt + curvewt = 1.0.  This will limit the total
c   wind weight to between 0.5 and 1.5 (but this is not required).

c Compute the wind weighting factor.
      do i=1,nx
        do j=1,ny
          windwt(i,j) = 1.0 + slopewt * wind_slope(i,j) +
     &      curvewt * curvature(i,j)
        enddo
      enddo

c Smooth the wind weighting factor to eliminate any sharp speed
c   increases resulting from the merging of the curve wt and the
c   slope wt.  Define the number of times this is done to be a
c   function of the curvature length scale and the grid increment.
c   The first 0.5 here just means that half of the caclulated
c   number appears to be about right (additional loops with this
c   smoother does not change the results much).  If there are
c   unwanted wave features in the snow distribution, this 0.5
c   factor can be increased to 1.0 or more, to get rid of these
c   waves.  Also see "loops_snowd_smoother" in snowtran_code.f.
c     xmult = 0.5
      xmult = 1.0
c     xmult = 1.5
      loops_windwt_smoother = nint(xmult * curve_len_scale /
     &  (0.5 * (deltax + deltay)))

c     print *
c     print *, 'loops_windwt_smoother ',loops_windwt_smoother
c     print *

c Don't do this smoothing if the domain is arbitrarily small.
      if (nx.gt.100 .and. ny.gt.100) then
        do k=1,loops_windwt_smoother
          call smoother9(nx,ny,windwt)
        enddo
      endif

c Continue with the wind calculations.
      do i=1,nx
        do j=1,ny

c Generate the terrain-modified wind speed.
          windspd_grid(i,j) = windwt(i,j) * windspd_grid(i,j)

c Further modify the wind speed to account for forest canopies.
          if (vegtype(i,j).le.5.0) then
            if (calc_subcanopy_met.eq.1.0) then
              nveg = nint(vegtype(i,j))

c Define the canopy wind-weighting factor.  Assume z=0.6*canopy_ht,
c   and the canopy_ht equals the vegetation snow-holding depth.
              beta = 0.9
              veg_ht = vegsnowd_xy(i,j)
              a = beta * forest_LAI(nveg)
              canopy_windwt = exp((- a)*(1.0 - (0.6*veg_ht)/veg_ht))
              windspd_grid(i,j) = canopy_windwt * windspd_grid(i,j)

            endif
          endif

c Modify the wind direction according to Ryan (1977).  Note that it
c   is critical that "dirdiff" handles the cases where the slope
c   azimuth and the wind direction are on different sides of the
c   360-0 line.
          if (slope_az(i,j).gt.270.0.and.
     &      winddir_grid(i,j).lt.90.0) then
            dirdiff = slope_az(i,j) - winddir_grid(i,j) - 360.0
          elseif (slope_az(i,j).lt.90.0.and.
     &      winddir_grid(i,j).gt.270.0) then
            dirdiff = slope_az(i,j) - winddir_grid(i,j) + 360.0
          else
            dirdiff = slope_az(i,j) - winddir_grid(i,j)
          endif
          if (abs(dirdiff).le.90.0) then
            winddir_grid(i,j) = winddir_grid(i,j) - 0.5 *
     &        min(wind_slope(i,j)*rad2deg,45.0) *
     &        sin(deg2rad * (2.0 * dirdiff))
            if (winddir_grid(i,j).gt.360.0) then
              winddir_grid(i,j) = winddir_grid(i,j) - 360.0
            elseif (winddir_grid(i,j).lt.0.0) then
              winddir_grid(i,j) = winddir_grid(i,j) + 360.0
            endif
          endif

c Extract the u and v wind components.
          uwind_grid(i,j) = (- windspd_grid(i,j)) *
     &      sin(deg2rad*winddir_grid(i,j))
          vwind_grid(i,j) = (- windspd_grid(i,j)) *
     &      cos(deg2rad*winddir_grid(i,j))
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine topo_data(nx,ny,deltax,deltay,topo,
     &  curvature,terrain_slope,slope_az,curve_len_scale)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,inc

      real pi,rad2deg,deltax,deltay,deltaxy,curve_len_scale,curve_max

      real topo(nx_max,ny_max)
      real dzdx(nx_max,ny_max)
      real dzdy(nx_max,ny_max)
      real curvature(nx_max,ny_max)
      real slope_az(nx_max,ny_max)
      real terrain_slope(nx_max,ny_max)

c Compute the topographic information required to run the wind
c   model.

c Deal with the model running at a point, or along single or double
c   lines.
      if (nx.le.2  .or.  ny.le.2) then
        do i=1,nx
          do j=1,ny
            curvature(i,j) = 0.0
            terrain_slope(i,j) = 0.0
            slope_az(i,j) = 0.0
          enddo
        enddo
      else

c Define the required constants.
        pi = 2.0 * acos(0.0)
        rad2deg = 180.0 / pi

c CURVATURE CALCULATIONS.

c Compute the average grid increment.
        deltaxy = 0.5 * (deltax + deltay)

c Convert the length scale to an appropriate grid increment.
        inc = max(1,nint(curve_len_scale/deltaxy))

c Compute the curvature.
        do i=1,nx
          do j=1,ny
            curvature(i,j) = (4.0 * topo(i,j) -
     &        topo(max(1,i-inc),max(1,j-inc)) -
     &        topo(min(nx,i+inc),min(ny,j+inc)) -
     &        topo(min(nx,i+inc),max(1,j-inc)) -
     &        topo(max(1,i-inc),min(ny,j+inc))) /
     &        (sqrt(2.0) * 16.0 * real(inc) * deltaxy) +
     &        (4.0 * topo(i,j) -
     &        topo(min(nx,i+inc),j) - topo(max(1,i-inc),j) -
     &        topo(i,min(ny,j+inc)) - topo(i,max(1,j-inc))) /
     &        (16.0 * real(inc) * deltaxy)
          enddo
        enddo

c Scale the curvature such that the max abs(curvature) has a value
c   of abs(0.5).  Include a 1 mm curvature in curve_max to prevent
c   divisions by zero in flat terrain where the curvature is zero.
        curve_max = 0.0 + 0.001
        do j=1,ny
          do i=1,nx
            curve_max = max(curve_max,abs(curvature(i,j)))
          enddo
        enddo
        do j=1,ny
          do i=1,nx
            curvature(i,j) = curvature(i,j) / (2.0 * curve_max)
          enddo
        enddo

c SLOPE CALCULATIONS.

c Find dzdx.
        do j=1,ny
          dzdx(1,j) = (topo(2,j) - topo(1,j)) / deltax
          do i=2,nx-1
            dzdx(i,j) = (topo(i+1,j) - topo(i-1,j)) / (2.0 * deltax)
          enddo
          dzdx(nx,j) = (topo(nx,j) - topo(nx-1,j)) / deltax
        enddo

c Find dzdy.
        do i=1,nx
          dzdy(i,1) = (topo(i,2) - topo(i,1)) / deltay
          do j=2,ny-1
            dzdy(i,j) = (topo(i,j+1) - topo(i,j-1)) / (2.0 * deltay)
          enddo
          dzdy(i,ny) = (topo(i,ny) - topo(i,ny-1)) / deltay
        enddo

c Calculate the terrain slope and azimuth.
        do i=1,nx
          do j=1,ny

c Some compilers will not allow dzdx and dzdy to both be 0.0 in
c   the atan2 computation.
c           if (abs(dzdx(i,j)).lt.1e-10) dzdx(i,j) = 1e-10
            if (abs(dzdy(i,j)).lt.1e-10) dzdy(i,j) = 1e-10

c Compute the slope azimuth, making sure that north has zero
c   azimuth.  Also note that for the Ryan wind rotation, the
c   azimuth values must range from 0 to 360.
            slope_az(i,j) = rad2deg *
     &        (3.0 / 2.0 * pi - atan2(dzdy(i,j),dzdx(i,j)))
            if (slope_az(i,j).ge.360.0) slope_az(i,j) =
     &        slope_az(i,j) - 360.0

c Compute the slope of the terrain.
            terrain_slope(i,j) = rad2deg *
     &        atan(sqrt(dzdx(i,j)*dzdx(i,j) + dzdy(i,j)*dzdy(i,j)))

          enddo
        enddo

      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine interpolate(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,var,dn,grid,undef,ifill,iobsint,
     &  iyear,imonth,iday,xhour,barnes_lg_domain,n_stns_used,
     &  k_stn,snowmodel_line_flag,xg_line,yg_line,seaice_run)

      implicit none

      include 'snowmodel.inc'

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn  ! center x coords of lower left grid cell
      double precision ymn  ! center y coords of lower left grid cell

      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords
      real var(nstns_max)  ! input values

      double precision xstn_tmp(nstns_max) ! input stn x coords
      double precision ystn_tmp(nstns_max) ! input stn y coords
      real var_tmp(nstns_max)  ! input values

      integer nstns        ! number of input values, all good
      real undef           ! undefined value
      real dn                  ! average observation spacing
      real grid(nx_max,ny_max) ! output values

      integer i,j      ! col, row counters
      integer ifill    ! flag (=1) forces a value in every cell
      integer iobsint  ! flag (=1) use dn value from .par file

      integer iyear,imonth,iday  ! model year, month, and day
      real xhour                 ! model decimal hour

      integer k_stn(nx_max,ny_max,9)
      integer k,n_stns_used
      real barnes_lg_domain,seaice_run

c Use the barnes oi scheme to grid the station data. If there is
c   only a single station, distribute those data uniformly over
c   the grid.  In the event that there are no valid observations,
c   send an error message and stop (although this should have been
c   caught as part of a preprocessor step).

c The interpolation can be done two different ways:
c   First, barnes_oi does the interpolation by processing all of
c     the available station data for each model grid cell.
c   Second, barnes_oi_ij does the interpolation by processing only
c     the "n_stns_used" number of stations for each model grid cell.
c   For small domains, with relatively few met stations (100's),
c   the first way is best.  For large domains (like the
c   United States, Globe, Pan-Arctic, North America, Greenland)
c   and many met stations (like 1000's), the second approach is the
c   most efficient.  But, the second approach carries the following
c   restrictions: 1) there can be no missing data for the fields of
c   interest; 2) there can be no missing stations (all stations
c   must exist throughout the simulation period); and 3) the
c   station met file must list the stations in the same order for
c   all time steps.  In addition, the code limits the number of
c   nearest stations used to be 5 or less.

c In this first case you are doing a Lagrangian sea ice parcel
c   simulation.
      if (seaice_run.eq.4.0) then
        do j=1,ny
          do i=1,nx
            grid(i,j) = var(i)
          enddo
        enddo
      else
        if (nstns.ge.2) then
          call get_dn(nx,ny,deltax,deltay,nstns,dn,iobsint)

          if (barnes_lg_domain.eq.1.0 .or. barnes_lg_domain.eq.2.0) then

            if (barnes_lg_domain.eq.2.0) then
              call get_nearest_stns_2(nx,ny,xmn,ymn,deltax,deltay,
     &          n_stns_used,k_stn,nstns,xstn,ystn,xg_line,yg_line,
     &          snowmodel_line_flag)
            endif

            do j=1,ny
              do i=1,nx

c Use that nearest station list to extract the station information
c   to be used in the interpolation.
                do k=1,n_stns_used
                  xstn_tmp(k) = xstn(k_stn(i,j,k))
                  ystn_tmp(k) = ystn(k_stn(i,j,k))
                  var_tmp(k) = var(k_stn(i,j,k))
                enddo

c Do the interpolation for this model grid cell.
                call barnes_oi_ij(deltax,deltay,xmn,ymn,
     &            n_stns_used,xstn_tmp,ystn_tmp,var_tmp,dn,grid,
     &            undef,ifill,i,j,snowmodel_line_flag,xg_line,yg_line)

              enddo
            enddo

          else

            call barnes_oi(nx,ny,deltax,deltay,xmn,ymn,
     &        nstns,xstn,ystn,var,dn,grid,undef,ifill)

          endif

        elseif (nstns.eq.1) then

          call single_stn(nx,ny,nstns,var,grid)

        else

          print *,'found no valid obs data at this time step'
          print *,'  model time =', iyear,imonth,iday,xhour
          stop

        endif

      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_nearest_stns_2(nx,ny,xmn,ymn,deltax,deltay,
     &  n_stns_used,k_stn,nstns,xstn,ystn,xg_line,yg_line,
     &  snowmodel_line_flag)

      implicit none

      include 'snowmodel.inc'

      double precision xstn(nstns_max)
      double precision ystn(nstns_max)
      double precision dsq(nstns_max)
      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

      double precision xg,yg,xmn,ymn,dist_min
      real deltax,deltay

      integer i,j,k,kk,nstns,n_stns_used,nx,ny
      integer k_stn(nx_max,ny_max,9)

      do j=1,ny
        do i=1,nx

c xcoords of grid nodes at index i,j
c ycoords of grid nodes at index i,j
          if (snowmodel_line_flag.eq.1.0) then
            xg = xg_line(i,j)
            yg = yg_line(i,j)
          elseif (snowmodel_line_flag.eq.0.0) then
            print *, 'This can be very slow.  I suggest you study'
            print *, 'and implement some version of the code below'
            print *, 'this subroutine before continuing along this'
            print *, 'simulation approach.'
            stop
            xg = xmn + deltax * (real(i) - 1.0)
            yg = ymn + deltay * (real(j) - 1.0)
          endif

c Loop through all of the stations, calculating the distance
c   between the current grid point and each of the stations.
          do k=1,nstns
            dsq(k) = (xg - xstn(k))**2 + (yg - ystn(k))**2
          enddo

c Loop through each of the station distances and find the
c   stations closest to the grid point in question.
          do kk=1,n_stns_used
            dist_min = 1.0e30
            do k=1,nstns
              if (dsq(k).le.dist_min) then
                k_stn(i,j,kk) = k
                dist_min = dsq(k)
              endif
            enddo

c Eliminate the last found minimum from the next search by making
c   its distance a big number.
            dsq(k_stn(i,j,kk)) = 1.0e30
          enddo

        enddo
      enddo

      return
      end

c get_nearest.f

c Find the ii,jj locations of the nearest observation grid
c   point (veg grid) closest to each of the output grid cells
c   (the topo grid).

c I'm starting out with original veg data on a UTM grid, and
c   interpolating to the UTM coords of the topo data grid.

c nnx, nny is the original veg data grid.
c nx, ny is the topo data grid.

cc    implicit none

cc    integer nx,ny,nstns,nnx,nny,n,icount

cc    parameter (nx=23401,ny=27502,nnx=14288,nny=16176)
cc    parameter (nstns=1)

cc    real dsq(nnx,nny)
cc    real xk_min(nx,ny,nstns)

cc    real del_x,del_y,gx_ll,gy_ll
cc    real xmn,ymn,deltax,deltay,xg,yg,xs,ys,dist_min
cc    integer i,j,k,kk,ii,jj,iii,iiii,jjj,jjjj,inc

cc    real gx_stn(nnx,nny)
cc    real gy_stn(nnx,nny)

cc    print *,'preforming the station indexing calculations'

c Grid increment for the veg grid, in m.
cc    del_x = 30.0
cc    del_y = 30.0

c Coordinates of center of lower left grid cell of veg (input) grid,
c   and grid increments.
cc    gx_ll = 398580.0 + del_x/2.0
cc    gy_ll = 8495405.0 + del_y/2.0

c UTM coordinates of the 'stations' or input grid-cell centers.
cc    do j=1,nny
cc      do i=1,nnx
cc        gx_stn(i,j) = gx_ll + del_x * (real(i) - 1.0)
cc        gy_stn(i,j) = gy_ll + del_y * (real(j) - 1.0)
cc      enddo
cc    enddo

c Grid increment for the topo grid, in m.
cc    deltax = 20.0
cc    deltay = 20.0

c Coordinates of center of lower left grid cell of topo (output) grid,
c   and grid increments.
cc    xmn = 381990.0 + deltax/2.0
cc    ymn = 8449790.0 + deltay/2.0

c Search for the stations nearest to the grid point of interest.
cc    icount = 0
cc    do j=1,ny
cc      if (mod(j,100).eq.0.0) print *, 'j = ',j
cc      do i=1,nx

c xcoords of topo grid nodes at index i,j
c ycoords of topo grid nodes at index i,j
cc        xg = xmn + deltax * (real(i) - 1.0)
cc        yg = ymn + deltay * (real(j) - 1.0)

c Find the corresponding veg grid cell coordinate to this grid cell.
cc        iii = nint((xg - gx_ll)/del_x + 1.0)
cc        jjj = nint((yg - gy_ll)/del_y + 1.0)

c Don't let things go outside the array bounds.
cc        inc = 5
cc        if (iii.le.inc) iii = 1 + inc
cc        if (iii.ge.nnx-inc+1) iii = nnx - inc
cc        if (jjj.le.inc) jjj = 1 + inc
cc        if (jjj.ge.nny-inc+1) jjj = nny - inc

c Loop through all of the stations, calculating the distance
c   between the current grid point and each of the stations.
cc        do jj=jjj-inc,jjj+inc
cc          do ii=iii-inc,iii+inc
cc            xs = gx_stn(ii,jj)
cc            ys = gy_stn(ii,jj)
cc            dsq(ii,jj) = (xg - xs)**2 + (yg - ys)**2
cc          enddo
cc        enddo

c Loop through each of the station distances and find the
c   stations closest to the grid point in question.
cc        do kk=1,nstns
cc          dist_min = 1.0e30
cc          do jj=jjj-inc,jjj+inc
cc            do ii=iii-inc,iii+inc
cc              if (dsq(ii,jj).le.dist_min) then
cc                k = ii + (jj - 1) * nnx
cc                xk_min(i,j,kk) = real(k)
cc                dist_min = dsq(ii,jj)
cc                iiii = ii
cc                jjjj = jj
cc              endif
cc            enddo
cc          enddo

c Eliminate the last found minimum from the next search by making
c   its distance a big number.
cc          dsq(iiii,jjjj) = 1.0e30
cc        enddo

cc      enddo
cc    enddo

c Save xk_min.
cc    print *,'--------------------------------------'
cc    print *,'--------------------------------------'
cc    print *, 'saving data'

cc    open (unit=41,file='nearest_stns_1.gdat',
cc   &  form='unformatted',access='direct',recl=4*nx)

cc    print *, '1'
cc    do j=1,ny
cc      write(41,rec=j) (xk_min(i,j,1),i=1,nx)
cc    enddo

cc    end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_good_values1(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,var_orig,var)

      implicit none

      include 'snowmodel.inc'

      integer nstns        ! number of input values, all good
      integer nstns_orig   ! number of input values
      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords
      real elev(nstns_max) ! input stn elevation
      real var(nstns_max)  ! input values
      real undef           ! undefined value
      double precision xstn_orig(nstns_max) ! input stn x coords
      double precision ystn_orig(nstns_max) ! input stn y coords
      real elev_orig(nstns_max) ! input stn elevation
      real var_orig(nstns_max)  ! input values

      integer k

c Before performing the interpolation, sort through the data and
c   toss out any undefined values.
      nstns = 0
      do k=1,nstns_orig
        if (var_orig(k).ne.undef) then
          nstns = nstns + 1
          xstn(nstns) = xstn_orig(k)
          ystn(nstns) = ystn_orig(k)
          var(nstns) = var_orig(k)
          elev(nstns) = elev_orig(k)
        endif
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_good_values2(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,var1_orig,var2_orig,
     &  var1,var2)

c Account for the special case where you must have two coincident
c   values to do the interpolation, like Tair and rh to interpolate
c   rh, and wind speed and dir to interpolate the winds.

      implicit none

      include 'snowmodel.inc'

      integer nstns        ! number of input values, all good
      integer nstns_orig   ! number of input values
      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords
      real elev(nstns_max) ! input stn elevation
      real undef           ! undefined value
      double precision xstn_orig(nstns_max) ! input stn x coords
      double precision ystn_orig(nstns_max) ! input stn y coords
      real elev_orig(nstns_max) ! input stn elevation

      real var1_orig(nstns_max)  ! input values
      real var2_orig(nstns_max)  ! input values
      real var1(nstns_max)  ! input values
      real var2(nstns_max)  ! input values

      integer k

c Before performing the interpolation, sort through the data and
c   toss out any undefined values.
      nstns = 0
      do k=1,nstns_orig
        if (var1_orig(k).ne.undef .and. var2_orig(k).ne.undef) then
          nstns = nstns + 1
          xstn(nstns) = xstn_orig(k)
          ystn(nstns) = ystn_orig(k)
          var1(nstns) = var1_orig(k)
          var2(nstns) = var2_orig(k)
          elev(nstns) = elev_orig(k)
        endif
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_obs_data(nstns_orig,Tair_orig,rh_orig,xstn_orig,
     &  ystn_orig,elev_orig,iyear,imonth,iday,xhour,undef,
     &  windspd_orig,winddir_orig,prec_orig,isingle_stn_flag,
     &  igrads_metfile,iter)

      implicit none

      include 'snowmodel.inc'

      integer iyr,imo,idy      ! year, month, and day of data
      real xhr                 ! decimal hour
      integer idstn            ! station id number

      integer k,nstns_orig,isingle_stn_flag,igrads_metfile,iter
      integer iyear,imonth,iday

      real Tair_orig(nstns_max),rh_orig(nstns_max)
      real winddir_orig(nstns_max),windspd_orig(nstns_max)
      double precision xstn_orig(nstns_max),ystn_orig(nstns_max)
      real elev_orig(nstns_max),xhour,prec_orig(nstns_max)
      real undef               ! undefined value

      if (isingle_stn_flag.eq.1) then
        nstns_orig = 1
      else
        read(20,*) nstns_orig
      endif

      if (nstns_orig.gt.nstns_max) then
        print *
        print *, 'You must increase the value of nstns_max in'
        print *, '  snowmodel.inc to be greater than nstns_orig.'
        print *, 'nstns_max = ',nstns_max
        print *, 'nstns_orig = ',nstns_orig
        print *
        stop
      endif

      do k=1,nstns_orig
        if (igrads_metfile.eq.1) then
          read(20,rec=iter) iyr,imo,idy,xhr,idstn,xstn_orig(k),
     &      ystn_orig(k),elev_orig(k),Tair_orig(k),rh_orig(k),
     &      windspd_orig(k),winddir_orig(k),prec_orig(k)
        else
          read(20,*) iyr,imo,idy,xhr,idstn,xstn_orig(k),
     &      ystn_orig(k),elev_orig(k),Tair_orig(k),rh_orig(k),
     &      windspd_orig(k),winddir_orig(k),prec_orig(k)
        endif

c MicroMet assumes that the air temperature comes in as deg C, but
c   all computations must be done in K.  Check for this and make
c   an appropriate adjustment.
        if (Tair_orig(k).lt.150.0 .and. Tair_orig(k).ne.undef)
     &    Tair_orig(k) = Tair_orig(k) + 273.15

c Do some error checking.  Check for the presence of data from
c  the same date.
        if (iyr.ne.iyear .or. imo.ne.imonth .or. idy.ne.iday
     &    .or. xhr.ne.xhour) then
          print *,'model time does not match data input time'
          print *,'  model =', iyear,imonth,iday,xhour
          print *,'  obs   =', iyr,imo,idy,xhr
          stop
        endif

      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_model_time(iyear_init,imonth_init,iday_init,
     &  xhour_init,iter,dt,iyear,imonth,iday,xhour,J_day)

      implicit none

      integer iyear,imonth,iday  ! model year, month, and day
      real xhour                 ! model decimal hour
      real dt                    ! model time step, in seconds
      integer iter               ! model iteration
      integer iyear_init         ! model start year
      integer imonth_init        ! model start month
      integer iday_init          ! model start day
      real xhour_init            ! model decimal start hour
      integer J_day              ! model day of year

c Misc. variables.
      real xmin,xhour_frac,xhour_tmp,xday
      integer ihour,last

      integer lastday(12)
      data lastday/31,28,31,30,31,30,31,31,30,31,30,31/

c Convert the simulation time to the exact year, month, day, and
c   decimal hour.

c Number of minutes into the simulation.  Here I have assumed that
c   dt will never be in fractions of minutes.
c     xmin = ((real(iter) - 1.0) * dt) / 60.0
      xmin = (real(iter) - 1.0) * (dt / 60.0)

c Model integration time in decimal hours.  The xhour_frac variable
c   needs to be fixed for dt < 3600 sec.
      xhour_tmp = xhour_init + xmin / 60.0
      ihour = mod(int(xhour_tmp),24)
      xhour_frac = 0.0
      xhour = real(ihour) + xhour_frac

c Number of days.
      xday = xhour_tmp / 24.0
      iday = iday_init + int(xday)

c Month and year, while accounting for leap-years.
      imonth = imonth_init
      iyear = iyear_init
 20   continue
      last = lastday(imonth)
      if (imonth.eq.2 .and. mod(iyear,4).eq.0
     &  .and. (mod(iyear,100).ne.0 .or. mod(iyear,1000).eq.0)) then
        last = last + 1
      endif
      if (iday.gt.last) then
        iday = iday - last
        imonth = imonth + 1
        if (imonth.gt.12) then
          imonth = 1
          iyear = iyear + 1
        endif
        go to 20
      endif

c Calculate the day of year (1...365,366) corresponding to the date
c   iyear-imonth-iday.
      J_day = iday
     &  + min(1,max(0,imonth-1))*31
     &  + min(1,max(0,imonth-2))*(28+(1-min(1,mod(iyear,4))))
     &  + min(1,max(0,imonth-3))*31
     &  + min(1,max(0,imonth-4))*30
     &  + min(1,max(0,imonth-5))*31
     &  + min(1,max(0,imonth-6))*30
     &  + min(1,max(0,imonth-7))*31
     &  + min(1,max(0,imonth-8))*31
     &  + min(1,max(0,imonth-9))*30
     &  + min(1,max(0,imonth-10))*31
     &  + min(1,max(0,imonth-11))*30
     &  + min(1,max(0,imonth-12))*31

c     print *, iyear,imonth,iday,xhour,J_day

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_lapse_rates(imonth,iday,T_lapse_rate,
     &  Td_lapse_rate,xlat,lapse_rate_user_flag,
     &  precip_lapse_rate,iprecip_lapse_rate_user_flag)

      implicit none

      integer imonth,iday,mbefore,mafter,k,months,
     &  lapse_rate_user_flag,iprecip_lapse_rate_user_flag
      real weight,T_lapse_rate,Td_lapse_rate,A,B,C,xlat,
     &  precip_lapse_rate
      parameter (months=12)
      integer lastday(months)
      data lastday/31,28,31,30,31,30,31,31,30,31,30,31/

c The lapse rate units are in deg_C km-1.  They are converted to
c   negative_deg_C m-1 below.
      real lapse_rate(months)
      real lapse_rate_nohem(months)
      real lapse_rate_sohem(months)
      real lapse_rate_user(months)
      data lapse_rate_nohem /4.4,5.9,7.1,7.8,8.1,8.2,
     &                       8.1,8.1,7.7,6.8,5.5,4.7/
      data lapse_rate_sohem /8.1,8.1,7.7,6.8,5.5,4.7,
     &                       4.4,5.9,7.1,7.8,8.1,8.2/

c If you want to use the 'user' array, put your monthly values in
c   here and set lapse_rate_user_flag = 1 in the .par file.
      data lapse_rate_user /4.4,5.9,7.1,7.8,8.1,8.2,
     &                      8.1,8.1,7.7,6.8,5.5,4.7/

c The initial vapor pressure coeffs are in units of km-1.
      real am(months)
      real am_nohem(months)
      real am_sohem(months)
      real am_user(months)
      data am_nohem /0.41,0.42,0.40,0.39,0.38,0.36,
     &               0.33,0.33,0.36,0.37,0.40,0.40/
      data am_sohem /0.33,0.33,0.36,0.37,0.40,0.40,
     &               0.41,0.42,0.40,0.39,0.38,0.36/

c If you want to use the 'user' array, put your monthly values in
c   here and set lapse_rate_user_flag = 1 in the .par file.
      data am_user /0.41,0.42,0.40,0.39,0.38,0.36,
     &              0.33,0.33,0.36,0.37,0.40,0.40/

c The precipitation lapse rate units are in km-1.
      real prec_lapse_rate(months)
      real precip_lapse_rate_nohem(months)
      real precip_lapse_rate_sohem(months)
      real precip_lapse_rate_user(months)
      data precip_lapse_rate_nohem /0.35,0.35,0.35,0.30,0.25,0.20,
     &                              0.20,0.20,0.20,0.25,0.30,0.35/
      data precip_lapse_rate_sohem /0.20,0.20,0.20,0.25,0.30,0.35,
     &                              0.35,0.35,0.35,0.30,0.25,0.20/

c If you want to use the 'user' array, put your monthly values in
c   here and set iprecip_lapse_rate_user_flag = 1 in the .par file.
      data precip_lapse_rate_user /0.35,0.35,0.35,0.30,0.25,0.20,
     &                             0.20,0.20,0.20,0.25,0.30,0.35/

c Air and dewpoint temperature.
      do k=1,months
        if (lapse_rate_user_flag.eq.0) then
          if (xlat.lt.0.0) then
            lapse_rate(k) = lapse_rate_sohem(k)
            am(k) = am_sohem(k)
          else
            lapse_rate(k) = lapse_rate_nohem(k)
            am(k) = am_nohem(k)
          endif
        elseif (lapse_rate_user_flag.eq.1) then
          lapse_rate(k) = lapse_rate_user(k)
          am(k) = am_user(k)
        endif
      enddo

c Precipitation.
      do k=1,months
        if (iprecip_lapse_rate_user_flag.eq.0) then
          if (xlat.lt.0.0) then
            prec_lapse_rate(k) = precip_lapse_rate_sohem(k)
          else
            prec_lapse_rate(k) = precip_lapse_rate_nohem(k)
          endif
        elseif (iprecip_lapse_rate_user_flag.eq.1) then
          prec_lapse_rate(k) = precip_lapse_rate_user(k)
        endif
      enddo

c Coeffs for saturation vapor pressure over water (Buck 1981).
c   Note: temperatures for Buck`s equations are in deg C, and
c   vapor pressures are in mb.  Do the adjustments so that the
c   calculations are done with temperatures in K, and vapor
c   pressures in Pa.
      A = 6.1121 * 100.0
      B = 17.502
      C = 240.97

c Over ice.
c     A = 6.1115 * 100.0
c     B = 22.452
c     C = 272.55

c Find the month before and after the day in question.
      if (iday.le.15) then
        mbefore = imonth - 1
        if (mbefore.eq.0) mbefore = 12
        mafter = imonth
        weight = (real(lastday(mbefore)) - 15. + real(iday)) /
     &    real(lastday(mbefore))
      else
        mbefore = imonth
        mafter = imonth + 1
        if (mafter.eq.13) mafter = 1
        weight = (real(iday) - 15.) / real(lastday(mbefore))
      endif

c Define the temperature lapse rate (deg C/m).
      T_lapse_rate = (- (weight * lapse_rate(mafter) +
     &  (1. - weight) * lapse_rate(mbefore))) / 1000.0

c Define the dew-point temperature lapse rate (deg C/m).
      Td_lapse_rate = (- ((weight * am(mafter) +
     &  (1. - weight) * am(mbefore)) * C)) / (B * 1000.0)

c Define the precipitation lapse rate (km-1).
      precip_lapse_rate = weight * prec_lapse_rate(mafter) +
     &  (1. - weight) * prec_lapse_rate(mbefore)

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

      subroutine barnes_oi(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,var,dn,grid,undef,ifill)

c This is an implementation of the Barnes objective analysis scheme
c   as described in:
c
c   Koch, S. E., M. DesJardins, and P. J. Kocin, 1983: An
c   interactive Barnes objective map analysis scheme for use with
c   satellite and conventional data. J. Climate and Applied
c   Meteorology, 22(9), 1487-1503.

      implicit none

      include 'snowmodel.inc'

      real gamma
      parameter (gamma=0.2)
      real pi

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn !center x coords of lower left grid cell
      double precision ymn !center y coords of lower left grid cell

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
          dsq = (xb - xa)**2 + (yb - ya)**2

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
          dsq = (xg - xa)**2 + (yg - ya)**2

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

      subroutine barnes_oi_ij(deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,var,dn,grid,
     &  undef,ifill,i,j,snowmodel_line_flag,xg_line,yg_line)

c This is an implementation of the Barnes objective analysis scheme
c   as described in:
c
c   Koch, S. E., M. DesJardins, and P. J. Kocin, 1983: An
c   interactive Barnes objective map analysis scheme for use with
c   satellite and conventional data. J. Climate and Applied
c   Meteorology, 22(9), 1487-1503.

      implicit none

      include 'snowmodel.inc'

      real gamma
      parameter (gamma=0.2)
      real pi

c     integer nx       ! number of x output values
c     integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn !center x coords of lower left grid cell
      double precision ymn !center y coords of lower left grid cell

      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

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
          dsq = (xb - xa)**2 + (yb - ya)**2

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

c     do 666 j=1,ny
c     do 555 i=1,nx

c xcoords of grid nodes at index i,j
c ycoords of grid nodes at index i,j
        if (snowmodel_line_flag.eq.1.0) then
          xg = xg_line(i,j)
          yg = yg_line(i,j)
        else
          xg = xmn + deltax * (real(i) - 1.0)
          yg = ymn + deltay * (real(j) - 1.0)
        endif

c Scan each input data point.
        ftot1 = 0.0
        wtot1 = 0.0
        ftot2 = 0.0
        wtot2 = 0.0
        nflag = 0

        do 333 nn=1,nstns
           
          xa = xstn(nn)
          ya = ystn(nn)
          dsq = (xg - xa)**2 + (yg - ya)**2

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

c 555 continue         ! end loop on cols i
c 666 continue         ! end loop on rows j

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine single_stn(nx,ny,nstns,var,grid)

      implicit none

      include 'snowmodel.inc'

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

      subroutine pressure(nx,ny,topo,sfc_pressure)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny

      real topo(nx_max,ny_max),sfc_pressure(nx_max,ny_max)
      real one_atmos,scale_ht

      one_atmos = 101300.0
c     scale_ht = 8500.0
      scale_ht = 8000.0

c Compute the average station pressure (in Pa).
      do j=1,ny
        do i=1,nx
          sfc_pressure(i,j) = one_atmos * exp((- topo(i,j))/scale_ht)
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine shortwave_data(nx,ny,deltax,deltay,xmn,ymn,
     &  iyear,imonth,iday,xhour,undef,var_grid,iter)

c This program takes observations as discrete points, compares
c   those observations with a gridded model representation of those
c   observations, at the corresponding grid cells, computes a
c   difference between the observations and modeled values, fits
c   a gridded surface through those differences, and adds the
c   difference grid to the model grid.  Thus, correcting the model
c   outputs with the observations.

      implicit none

      include 'snowmodel.inc'

      real deltax,deltay,xhour,xhr,elev(nstns_max),undef
      real var_grid(nx_max,ny_max),delta_var_grid(nx_max,ny_max)
      real var_obs(nstns_max),elev_orig(nstns_max),var_orig(nstns_max)

      double precision xmn,ymn
      double precision xstn(nstns_max),ystn(nstns_max)
      double precision xstn_orig(nstns_max),ystn_orig(nstns_max)

      integer nx,ny,k,nstns,iter,iyr,imo,idy,iyear,imonth,iday,
     &  idstn_orig,nstns_orig,i,j

c Open the observation data file.
      if (iter.eq.1) open (unit=71,file='extra_met/shortwave.dat')

c Read the data describing the time, location, and variable values
c   for each station, at this time step.  Here I have assumed that
c   the data file is in the 'non-single-station' format (with a
c   station count listed at the begining at each new time step).
      read(71,*) nstns_orig
      do k=1,nstns_orig
        read(71,*) iyr,imo,idy,xhr,idstn_orig,xstn_orig(k),
     &    ystn_orig(k),elev_orig(k),var_orig(k)
      enddo

c Compare the observation time with the model time.
      if (iyr.ne.iyear .or. imo.ne.imonth .or. idy.ne.iday
     &  .or. xhr.ne.xhour) then
        print *,'model time does not match obs data input time'
        print *,'  model =', iyear,imonth,iday,xhour
        print *,'  obs   =', iyr,imo,idy,xhr
        stop
      endif

c Filter through the original input data, and eliminate any
c   missing values.
      call get_good_values1(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,var_orig,var_obs)

c If there are no observational data at this time step, use the
c   modeled values without any modification.  If there are some
c   good data, do the correction/data assimilation.
      if (nstns.gt.0) then
        call DATA_ASSIM(nx,ny,deltax,deltay,xmn,ymn,xstn,ystn,
     &    nstns,var_obs,delta_var_grid,var_grid)
      endif

c For incoming shortwave, incoming longwave, and surface pressure,
c   make sure no negetive numbers have been produced.
      do j=1,ny
        do i=1,nx
          var_grid(i,j) = max(0.0,var_grid(i,j))
        enddo
      enddo

      open (74,file='extra_met/shortwave_grid.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      write (74,rec=iter) ((delta_var_grid(i,j),i=1,nx),j=1,ny)
      close(74)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine longwave_data(nx,ny,deltax,deltay,xmn,ymn,
     &  iyear,imonth,iday,xhour,undef,var_grid,iter)

c This program takes observations as discrete points, compares
c   those observations with a gridded model representation of those
c   observations, at the corresponding grid cells, computes a
c   difference between the observations and modeled values, fits
c   a gridded surface through those differences, and adds the
c   difference grid to the model grid.  Thus, correcting the model
c   outputs with the observations.

      implicit none

      include 'snowmodel.inc'

      real deltax,deltay,xhour,xhr,elev(nstns_max),undef
      real var_grid(nx_max,ny_max),delta_var_grid(nx_max,ny_max)
      real var_obs(nstns_max),elev_orig(nstns_max),var_orig(nstns_max)

      double precision xmn,ymn
      double precision xstn(nstns_max),ystn(nstns_max)
      double precision xstn_orig(nstns_max),ystn_orig(nstns_max)

      integer nx,ny,k,nstns,iter,iyr,imo,idy,iyear,imonth,iday,
     &  idstn_orig,nstns_orig,i,j

c Open the observation data file.
      if (iter.eq.1) open (unit=72,file='extra_met/longwave.dat')

c Read the data describing the time, location, and variable values
c   for each station, at this time step.  Here I have assumed that
c   the data file is in the 'non-single-station' format (with a
c   station count listed at the begining at each new time step).
      read(72,*) nstns_orig
      do k=1,nstns_orig
        read(72,*) iyr,imo,idy,xhr,idstn_orig,xstn_orig(k),
     &    ystn_orig(k),elev_orig(k),var_orig(k)
      enddo

c Compare the observation time with the model time.
      if (iyr.ne.iyear .or. imo.ne.imonth .or. idy.ne.iday
     &  .or. xhr.ne.xhour) then
        print *,'model time does not match obs data input time'
        print *,'  model =', iyear,imonth,iday,xhour
        print *,'  obs   =', iyr,imo,idy,xhr
        stop
      endif

c Filter through the original input data, and eliminate any
c   missing values.
      call get_good_values1(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,var_orig,var_obs)

c If there are no observational data at this time step, use the
c   modeled values without any modification.  If there are some
c   good data, do the correction/data assimilation.
      if (nstns.gt.0) then
        call DATA_ASSIM(nx,ny,deltax,deltay,xmn,ymn,xstn,ystn,
     &    nstns,var_obs,delta_var_grid,var_grid)
      endif

c For incoming shortwave, incoming longwave, and surface pressure,
c   make sure no negetive numbers have been produced.
      do j=1,ny
        do i=1,nx
          var_grid(i,j) = max(0.0,var_grid(i,j))
        enddo
      enddo

c     open (75,file='extra_met/longwave_grid.gdat',
c    &  form='unformatted',access='direct',recl=4*nx*ny)
c     write (75,rec=iter) ((delta_var_grid(i,j),i=1,nx),j=1,ny)
c     close(75)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine sfc_pressure_data(nx,ny,deltax,deltay,xmn,ymn,
     &  iyear,imonth,iday,xhour,undef,var_grid,iter)

c This program takes observations as discrete points, compares
c   those observations with a gridded model representation of those
c   observations, at the corresponding grid cells, computes a
c   difference between the observations and modeled values, fits
c   a gridded surface through those differences, and adds the
c   difference grid to the model grid.  Thus, correcting the model
c   outputs with the observations.

      implicit none

      include 'snowmodel.inc'

      real deltax,deltay,xhour,xhr,elev(nstns_max),undef
      real var_grid(nx_max,ny_max),delta_var_grid(nx_max,ny_max)
      real var_obs(nstns_max),elev_orig(nstns_max),var_orig(nstns_max)

      double precision xmn,ymn
      double precision xstn(nstns_max),ystn(nstns_max)
      double precision xstn_orig(nstns_max),ystn_orig(nstns_max)

      integer nx,ny,k,nstns,iter,iyr,imo,idy,iyear,imonth,iday,
     &  idstn_orig,nstns_orig,i,j

c Open the observation data file.
      if (iter.eq.1) open (unit=73,file='extra_met/sfc_pressure.dat')

c Read the data describing the time, location, and variable values
c   for each station, at this time step.  Here I have assumed that
c   the data file is in the 'non-single-station' format (with a
c   station count listed at the begining at each new time step).
      read(73,*) nstns_orig
      do k=1,nstns_orig
        read(73,*) iyr,imo,idy,xhr,idstn_orig,xstn_orig(k),
     &    ystn_orig(k),elev_orig(k),var_orig(k)
      enddo

c Compare the observation time with the model time.
      if (iyr.ne.iyear .or. imo.ne.imonth .or. idy.ne.iday
     &  .or. xhr.ne.xhour) then
        print *,'model time does not match obs data input time'
        print *,'  model =', iyear,imonth,iday,xhour
        print *,'  obs   =', iyr,imo,idy,xhr
        stop
      endif

c Filter through the original input data, and eliminate any
c   missing values.
      call get_good_values1(nstns_orig,xstn_orig,ystn_orig,
     &  elev_orig,undef,nstns,xstn,ystn,elev,var_orig,var_obs)

c If there are no observational data at this time step, use the
c   modeled values without any modification.  If there are some
c   good data, do the correction/data assimilation.
      if (nstns.gt.0) then
        call DATA_ASSIM(nx,ny,deltax,deltay,xmn,ymn,xstn,ystn,
     &    nstns,var_obs,delta_var_grid,var_grid)
      endif

c For incoming shortwave, incoming longwave, and surface pressure,
c   make sure no negetive numbers have been produced.
      do j=1,ny
        do i=1,nx
          var_grid(i,j) = max(0.0,var_grid(i,j))
        enddo
      enddo

c     open (76,file='extra_met/sfc_pressure_grid.gdat',
c    &  form='unformatted',access='direct',recl=4*nx*ny)
c     write (76,rec=iter) ((delta_var_grid(i,j),i=1,nx),j=1,ny)
c     close(76)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine DATA_ASSIM(nx,ny,deltax,deltay,xmn,ymn,xstn,ystn,
     &  nstns,var_obs,delta_var_grid,var_grid)

      implicit none

      include 'snowmodel.inc'

      real deltax,deltay,undef,dn
      real var_grid(nx_max,ny_max),delta_var_grid(nx_max,ny_max)
      real var_model(nstns_max),var_obs(nstns_max),
     &  delta_var(nstns_max)

      double precision xmn,ymn
      double precision xstn(nstns_max),ystn(nstns_max)

      integer ii(nstns_max),jj(nstns_max)
      integer nx,ny,i,j,ifill,iobsint,k,nstns

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
        delta_var(k) = var_obs(k) - var_model(k)
      enddo

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
        call barnes_oi(nx,ny,deltax,deltay,xmn,ymn,
     &    nstns,xstn,ystn,delta_var,dn,delta_var_grid,undef,ifill)
      elseif (nstns.eq.1) then
        call single_stn(nx,ny,nstns,delta_var,delta_var_grid)
      endif

c Use the gridded delta surface to correct the modeled variable.
      do j=1,ny
        do i=1,nx
          var_grid(i,j) = var_grid(i,j) + delta_var_grid(i,j)
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_lai(J_day,forest_LAI)

      implicit none

      integer nftypes,J_day,n
      parameter (nftypes=5)

      real vlai_summer(nftypes),vlai_winter(nftypes),
     &  forest_LAI(nftypes)

      real pi,daysinyr,tmax,tmin,peak_jday,dtseason,vtrans,tseason,
     &  fseason

c The five forest types in MicroMet/SnowModel:
c  1  coniferous forest
c  2  deciduous forest
c  3  mixed forest
c  4  scattered short-conifer
c  5  clearcut conifer

      data vlai_summer /2.5, 2.5, 2.5, 1.5, 1.0/
      data vlai_winter /2.5, 0.5, 1.5, 1.5, 1.0/

c Note: A maximum forest LAI of 5.0 will give almost zero (like
c   10 W m^2) incoming solar under the canopy.  Values for Fraser
c   Experimental Forest in Colorado are 2-3 (LSOS site = 1.8,
c   Kelly's/Gus' site = 2.3).

c Calculate a seasonally varying temperature, assuming a max and
c   min temperature and a cos distribution peaking in mid July
c   (J_day = 200).  Then use this to define the seasonal lai
c   variation.
      pi = 2.0 * acos(0.0)
      daysinyr = 366.0
      tmax = 298.0
      tmin = 273.0
      peak_jday = 200.0

      dtseason = tmax - tmin
      vtrans = tmin + dtseason / 2.0

      tseason = vtrans + dtseason / 2.0 *
     &  cos(2.0 * pi / daysinyr * (real(J_day) - peak_jday))

      fseason = 0.0016 * (tmax - tseason)**2

      do n=1,nftypes
        forest_LAI(n) = (1.0 - fseason) * vlai_summer(n) +
     &    fseason * vlai_winter(n)
      enddo

c     print *,J_day,(forest_LAI(n),n=1,nftypes)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_wind_file(nx,ny,iter,uwind_grid,vwind_grid,
     &  windspd_grid,winddir_grid,windspd_flag,winddir_flag,
     &  windspd_min)

      implicit none

      include 'snowmodel.inc'

      integer nx
      integer ny

      real uwind_grid(nx_max,ny_max)
      real vwind_grid(nx_max,ny_max)
      real winddir_grid(nx_max,ny_max)
      real windspd_grid(nx_max,ny_max)

      real pi,deg2rad,rad2deg
      integer i,j,iter,irec
      real windspd_flag,winddir_flag,u_sum,v_sum,windspd_min

c Define the required constants.
      pi = 2.0 * acos(0.0)
      deg2rad = pi / 180.0
      rad2deg = 180.0 / pi

c Read in the u and v arrays for this time step.
      if (iter.eq.1) then
        open (66,file='/data10/baffin/nuatmos/wind_spd_dir.gdat',
     &    form='unformatted',access='direct',recl=4*nx*ny)
      endif

      irec = (iter - 1) * 2
      read (66,rec=irec+1) ((uwind_grid(i,j),i=1,nx),j=1,ny)
      read (66,rec=irec+2) ((vwind_grid(i,j),i=1,nx),j=1,ny)

c Convert these u and v components to speed and directions.
      do j=1,ny
        do i=1,nx

c Some compilers do not allow both u and v to be 0.0 in
c   the atan2 computation.
          if (abs(uwind_grid(i,j)).lt.1e-10) uwind_grid(i,j) = 1e-10

          winddir_grid(i,j) = rad2deg *
     &      atan2(uwind_grid(i,j),vwind_grid(i,j))
          if (winddir_grid(i,j).ge.180.0) then
            winddir_grid(i,j) = winddir_grid(i,j) - 180.0
          else
            winddir_grid(i,j) = winddir_grid(i,j) + 180.0
          endif
          windspd_grid(i,j) =
     &      sqrt(uwind_grid(i,j)**2 + vwind_grid(i,j)**2)
        enddo
      enddo

c Avoid problems of zero (low) winds (for example, turbulence
c   theory, log wind profile, etc., says that we must have some
c   wind.  Thus, some equations blow up when the wind speed gets
c   very small).
      do j=1,ny
        do i=1,nx
          if (windspd_grid(i,j).lt.windspd_min) then
            windspd_grid(i,j) = windspd_min
            uwind_grid(i,j) = (- windspd_grid(i,j)) *
     &        sin(deg2rad*winddir_grid(i,j))
            vwind_grid(i,j) = (- windspd_grid(i,j)) *
     &        cos(deg2rad*winddir_grid(i,j))
          endif
        enddo
      enddo

c Find the maximum wind speed in the domain, and the
c   domain-averaged wind direction.
      windspd_flag = 0.0
      u_sum = 0.0
      v_sum = 0.0
      do j=1,ny
        do i=1,nx
          windspd_flag = max(windspd_flag,windspd_grid(i,j))
          u_sum = u_sum + uwind_grid(i,j)
          v_sum = v_sum + vwind_grid(i,j)
        enddo
      enddo
      u_sum = u_sum / real(nx*ny)
      v_sum = v_sum / real(nx*ny)

c Some compilers do not allow both u and v to be 0.0 in
c   the atan2 computation.
      if (abs(u_sum).lt.1e-10) u_sum = 1e-10

      winddir_flag = rad2deg * atan2(u_sum,v_sum)
      if (winddir_flag.ge.180.0) then
        winddir_flag = winddir_flag - 180.0
      else
        winddir_flag = winddir_flag + 180.0
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_daily_irec (iter,dt,irec_day)

      implicit none

      integer iter,irec_day
      real dt,secs_in_day,secs_in_sim

      secs_in_day = 60.0 * 60.0 * 24.0

      secs_in_sim = dt * real(iter - 1)
      irec_day = int(secs_in_sim / secs_in_day) + 1

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c outputs_user.f

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine OUTPUTS_USER(nx,ny,iter,Tair_grid,rh_grid,
     &  uwind_grid,vwind_grid,windspd_grid,winddir_grid,
     &  Qsi_grid,Qli_grid,prec_grid,Tsfc,Qle,Qh,Qe,Qc,Qm,Qf,
     &  e_balance,snow_depth,xro_snow,swe_depth,ro_nsnow,
     &  runoff,rain,sprec,sum_prec,sum_runoff,w_balance,
     &  snow_d,topo_land,wbal_qsubl,sum_sprec,wbal_salt,
     &  wbal_susp,ro_snow_grid,sum_Qcs,canopy_int,Qcs,
     &  iyear,imonth,iday,xhour,undef,deltax,xmn,ymn,
     &  wbal_subgrid,canopy_unload,sum_qsubl,sum_trans,
     &  sum_unload,sum_glacmelt,glacier_melt,swemelt,
     &  sfc_pressure,sum_swemelt,albedo,nrecs_max,
     &  icorr_factor_loop,swesublim,vegtype,iter_start,
     &  seaice_run,print_inc,cloud_frac_grid,
     &  output_path_wo_assim,output_path_wi_assim,print_var,
     &  print_outvars,Qsubl_depth,Qsalt,Qsusp)

c This subroutine is available to provide user-defined outputs.
c   These might be special-case situations, like just writing out
c   data at the end of every day, writing out a few grid cells,
c   saving each data arrays to individual files, etc.

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,iter,max_poly,num_poly,iyear,imonth,iday,
     &  icorr_factor_loop,iter_start,icorr_loop_new,
     &  individual_files,k,irec

      real Tair_grid(nx_max,ny_max),rh_grid(nx_max,ny_max),
     &  uwind_grid(nx_max,ny_max),vwind_grid(nx_max,ny_max),
     &  windspd_grid(nx_max,ny_max),winddir_grid(nx_max,ny_max),
     &  Qsi_grid(nx_max,ny_max),Qli_grid(nx_max,ny_max),
     &  prec_grid(nx_max,ny_max),Tsfc(nx_max,ny_max),
     &  Qle(nx_max,ny_max),Qh(nx_max,ny_max),Qe(nx_max,ny_max),
     &  Qc(nx_max,ny_max),Qm(nx_max,ny_max),Qf(nx_max,ny_max),
     &  e_balance(nx_max,ny_max),snow_depth(nx_max,ny_max),
     &  xro_snow(nx_max,ny_max),swe_depth(nx_max,ny_max),
     &  ro_nsnow(nx_max,ny_max),runoff(nx_max,ny_max),
     &  rain(nx_max,ny_max),sprec(nx_max,ny_max),
     &  sum_prec(nx_max,ny_max),sum_runoff(nx_max,ny_max),
     &  w_balance(nx_max,ny_max),snow_d(nx_max,ny_max),
     &  topo_land(nx_max,ny_max),wbal_qsubl(nx_max,ny_max),
     &  sum_sprec(nx_max,ny_max),wbal_salt(nx_max,ny_max),
     &  wbal_susp(nx_max,ny_max),ro_snow_grid(nx_max,ny_max),
     &  sum_Qcs(nx_max,ny_max),canopy_int(nx_max,ny_max),
     &  Qcs(nx_max,ny_max),wbal_subgrid(nx_max,ny_max),
     &  canopy_unload(nx_max,ny_max),sum_qsubl(nx_max,ny_max),
     &  sum_trans(nx_max,ny_max),glacier_melt(nx_max,ny_max),
     &  sum_unload(nx_max,ny_max),sum_glacmelt(nx_max,ny_max),
     &  swemelt(nx_max,ny_max),sfc_pressure(nx_max,ny_max),
     &  sum_swemelt(nx_max,ny_max),swesublim(nx_max,ny_max),
     &  vegtype(nx_max,ny_max),albedo(nx_max,ny_max),
     &  cloud_frac_grid(nx_max,ny_max),Qsubl_depth(nx_max,ny_max),
     &  Qsalt(nx_max,ny_max),Qsusp(nx_max,ny_max)

      real undef,xhour,deltax,pi,rad2deg,seaice_run,print_inc
      double precision xmn,ymn
      double precision nrecs_max,nrecs

      real uwnd(nx_max,ny_max)
      real vwnd(nx_max,ny_max)

c Define the output variable data block.  Note that the print_var
c   "yes/no" array was generated in readparam_code.f.
      real vars(nx_max,ny_max,n_print_vars)
      character*80 output_path_wo_assim,output_path_wi_assim
      character*1 print_var(n_print_vars)
      character*4 print_outvars(n_print_vars)

c This was defined in preprocess_code.f, in subroutine mk_ctl_files.
c     data print_outvars /'tair','relh','wspd','qsin','qlin',
c    &                    'qlem','albd','wdir','prec','rpre',
c    &                    'spre','smlt','ssub','roff','glmt',
c    &                    'snod','sden','swed','sspr','ssmt',
c    &                    'cldf','var1','var2','var3','var4',
c    &                    'var5','var6','var7','var8','var9'/
      
c These now come in from the .par file.
c     character path1*(*) 
c     character path2*(*) 

      integer i_trailing_blanks,trailing_blanks,i_len_wo,i_len_wi

c Calculate how long the paths are.
      i_len_wo = 80 - trailing_blanks(output_path_wo_assim)
      i_len_wi = 80 - trailing_blanks(output_path_wi_assim)
c     print *, i_len_wo,i_len_wi
c     print *, output_path_wo_assim(1:i_len_wo)
c     print *, output_path_wi_assim(1:i_len_wi)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c BEGIN USER EDIT SECTION.

c Define which variables you want to save, whether they will be
c   output at every time step or some time-step increment, which
c   directories the data will be put in, etc.

c Define the output file locations (paths).
c These now come in from the .par file.
c     parameter (path1 =
c    &  'outputs/wo_assim/') 
c     parameter (path2 =
c    &  'outputs/wi_assim/') 

c Write a seperate file for each variable (individual_files = 1).
c   No other option has been implemented here.
      individual_files = 1

c Define the number of time steps you are going to sum or average
c   over.  If you want to output data at every model time step, set
c   print_inc = 1.0.  For run with an hourly time step and data
c   writes once a day, print_inc = 24.0.  For a run with 3-hourly
c   time steps and data writes once a day, print_inc = 8.0.
c This now comes in from the .par file.
c     print_inc = 24.0
c     print_inc = 1.0
c     print_inc = 8.0

c Define the variables you want to save.  The following are the
c   variables this subroutine is currently set up to output.
c   Listed are the output variable name and the corresponding model
c   variable name.
c Note that these "yes/no" flags are now defined in snowmodel.par.

c VALUES AVERAGED OVER THE PERIOD.
c    1   tair(i,j) = Tair_grid(i,j) - 273.15
c    2   relh(i,j) = rh_grid(i,j)
c    3   wspd(i,j) = windspd_grid(i,j)
c    4   qsin(i,j) = Qsi_grid(i,j)
c    5   qlin(i,j) = Qli_grid(i,j)
c    6   qlem(i,j) = Qle(i,j)
c    7   albd(i,j) = albedo(i,j)
c    8   wdir(i,j) = from uwind_grid(i,j) and vwind_grid(i,j)

c VALUES SUMMED OVER THE PERIOD.
c    9   prec(i,j) = prec_grid(i,j)
c   10   rpre(i,j) = rain(i,j)
c   11   spre(i,j) = sprec(i,j)
c   12   smlt(i,j) = swemelt(i,j)
c   13   ssub(i,j) = swesublim(i,j)
c   14   roff(i,j) = runoff(i,j)
c   15   glmt(i,j) = glacier_melt(i,j)

c VALUES SAVED AT THE END OF THE PERIOD.
c   16   snod(i,j) = snow_depth(i,j)
c   17   sden(i,j) = xro_snow(i,j)
c   18   swed(i,j) = swe_depth(i,j)
c   19   sspr(i,j) = sum_sprec(i,j)
c   20   ssmt(i,j) = sum_swemelt(i,j)

c NEW VARIABLES ADDED TO THE OUTPUT DATA BLOCK.
c   (you have to modify the code below to
c    do the processing you want for these)
c   21   cldf(i,j) = cloud_fraction_grid(i,j)

c Define which variables you want to save by placing a yes = 'y' or
c   no = 'n' in front of the variable number.

c VALUES AVERAGED OVER THE PERIOD.
c VALUES AVERAGED OVER THE PERIOD.
c 1 = tair(i,j) = Tair_grid(i,j) - 273.15
c     print_var(1)  = 'y'

c 2 = relh(i,j) = rh_grid(i,j)
c     print_var(2)  = 'n'

c 3 = wspd(i,j) = windspd_grid(i,j)
c     print_var(3)  = 'n'

c 4 = qsin(i,j) = Qsi_grid(i,j)
c     print_var(4)  = 'n'

c 5 = qlin(i,j) = Qli_grid(i,j)
c     print_var(5)  = 'n'

c 6 = qlem(i,j) = Qle(i,j)
c     print_var(6)  = 'n'

c 7 = albd(i,j) = albedo(i,j)
c     print_var(7)  = 'n'

c 8 = wdir(i,j) = from uwind_grid(i,j) and vwind_grid(i,j)
c     print_var(8)  = 'n'

c VALUES SUMMED OVER THE PERIOD.
c VALUES SUMMED OVER THE PERIOD.
c  9 = prec(i,j) = prec_grid(i,j)
c     print_var(9)  = 'y'

c 10 = rpre(i,j) = rain(i,j)
c     print_var(10) = 'y'

c 11 = spre(i,j) = sprec(i,j)
c     print_var(11) = 'y'

c 12 = smlt(i,j) = swemelt(i,j)
c     print_var(12) = 'n'

c 13 = ssub(i,j) = swesublim(i,j)
c     print_var(13) = 'n'

c 14 = roff(i,j) = runoff(i,j)
c     print_var(14) = 'n'

c 15 = glmt(i,j) = glacier_melt(i,j)
c     print_var(15) = 'n'

c VALUES SAVED AT THE END OF THE PERIOD.
c VALUES SAVED AT THE END OF THE PERIOD.
c 16 = snod(i,j) = snow_depth(i,j)
c     print_var(16) = 'y'

c 17 = sden(i,j) = xro_snow(i,j)
c     print_var(17) = 'y'

c 18 = swed(i,j) = swe_depth(i,j)
c     print_var(18) = 'y'

c 19 = sspr(i,j) = sum_sprec(i,j)
c     print_var(19) = 'y'

c 20 = ssmt(i,j) = sum_swemelt(i,j)
c     print_var(20) = 'y'

c NEW VARIABLES ADDED TO THE OUTPUT DATA BLOCK.
c NEW VARIABLES ADDED TO THE OUTPUT DATA BLOCK.
c   (you have to modify the code below to
c    do the processing you want for these)
c 21 = cldf(i,j) = cloud_fraction_grid(i,j)
c     print_var(21) = 'n'

c Extra variables.
c     print_var(22) = 'n'
c     print_var(23) = 'n'
c     print_var(24) = 'n'
c     print_var(25) = 'n'
c     print_var(26) = 'n'
c     print_var(27) = 'n'
c     print_var(28) = 'n'
c     print_var(29) = 'n'
c     print_var(30) = 'n'

c Note that this data output implementation is currently configured
c   to mask out the ocean points (vegtype.eq.24.0) if this is a
c   land run (seaice_run = 0.0); mask out all land points (vegtype.
c   ne.24.0) if this is an ocean/sea ice run (seaice_run = 1.0, 3.0,
c   or 4.0); and to not mask out anything if this is a combined land
c   and sea ice run (seaice_run = 2.0).

c END USER EDIT SECTION.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Define the constants used in the wind-direction averaging.
      pi = 2.0 * acos(0.0)
      rad2deg = 180.0 / pi

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Use individual output files for each variable.
      if (individual_files.eq.1) then

        if (iter.eq.iter_start) then
          nrecs = nx * ny
          if (nrecs.gt.nrecs_max) then
            print *,'Your simulation domain has too many grid cells'
            print *,'to print the .gdat files like the write statements'
            print *,'are currently configured.  You will have to change'
            print *,'them to look like:'
            print *,'    do j=1,ny'
            print *,'      write (51,rec=j) (var(i,j),i=1,nx)'
            print *,'    enddo'
            stop
          endif
        endif

c Open individual output files for each variable.
        if (iter.eq.iter_start) then
          if (icorr_factor_loop.eq.1) then
            do k=1,n_print_vars
              if (print_var(k).eq.'y') then
                open (220+k,
     &file=output_path_wo_assim(1:i_len_wo)//print_outvars(k)//'.gdat',
     &            form='unformatted',access='direct',recl=4*nx*ny,
     &            status='replace')
              endif
            enddo
          endif

          if (icorr_factor_loop.eq.2) then
            do k=1,n_print_vars
              if (print_var(k).eq.'y') then
                open (320+k,
     &file=output_path_wi_assim(1:i_len_wi)//print_outvars(k)//'.gdat',
     &            form='unformatted',access='direct',recl=4*nx*ny,
     &            status='replace')
              endif
            enddo
          endif
        endif

        if (iter.eq.iter_start) then
c Initialize the averaging and summing arrays.
          do j=1,ny
            do i=1,nx
              do k=1,n_print_vars
                vars(i,j,k) = 0.0
              enddo
              uwnd(i,j) = 0.0
              vwnd(i,j) = 0.0
            enddo
          enddo
        endif

c Perform the avaraging, summing, etc.
        do j=1,ny
          do i=1,nx
c Values averaged over the period.
            vars(i,j,1) = vars(i,j,1) + (Tair_grid(i,j) - 273.15) /
     &        print_inc
            vars(i,j,2) = vars(i,j,2) + rh_grid(i,j) / print_inc
            vars(i,j,3) = vars(i,j,3) + windspd_grid(i,j) / print_inc
            vars(i,j,4) = vars(i,j,4) + Qsi_grid(i,j) / print_inc
            vars(i,j,5) = vars(i,j,5) + Qli_grid(i,j) / print_inc
            vars(i,j,6) = vars(i,j,6) + Qle(i,j) / print_inc
            vars(i,j,7) = vars(i,j,7) + albedo(i,j) / print_inc

            uwnd(i,j) = uwnd(i,j) + uwind_grid(i,j) / print_inc
            vwnd(i,j) = vwnd(i,j) + vwind_grid(i,j) / print_inc

c Some compilers do not allow both u and v to be 0.0 in
c   the atan2 computation.
            if (abs(uwnd(i,j)).lt.1e-10) uwnd(i,j) = 1e-10

            vars(i,j,8) = rad2deg * atan2(uwnd(i,j),vwnd(i,j))
            if (vars(i,j,8).ge.180.0) then
              vars(i,j,8) = vars(i,j,8) - 180.0
            else
              vars(i,j,8) = vars(i,j,8) + 180.0
            endif

c Values summed over the period.
            vars(i,j,9) = vars(i,j,9) + prec_grid(i,j)
            vars(i,j,10) = vars(i,j,10) + rain(i,j)
            vars(i,j,11) = vars(i,j,11) + sprec(i,j)
            vars(i,j,12) = vars(i,j,12) + swemelt(i,j)
            vars(i,j,13) = vars(i,j,13) + swesublim(i,j)
            vars(i,j,14) = vars(i,j,14) + runoff(i,j)
            vars(i,j,15) = vars(i,j,15) + glacier_melt(i,j)

c Values saved at the end of the day.
            vars(i,j,16) = snow_depth(i,j)
            vars(i,j,17) = xro_snow(i,j)
            vars(i,j,18) = swe_depth(i,j)
            vars(i,j,19) = sum_sprec(i,j)
            vars(i,j,20) = sum_swemelt(i,j)

c Values averaged over the period.
          vars(i,j,21) = vars(i,j,21) + cloud_frac_grid(i,j) / print_inc

c New variables.
            vars(i,j,22) = vars(i,j,22) + Qsubl_depth(i,j)
            vars(i,j,23) = sum_trans(i,j)
            vars(i,j,24) = vars(i,j,24) + Qsalt(i,j)
            vars(i,j,25) = vars(i,j,25) + Qsusp(i,j)

          enddo
        enddo

c Check to see whether this is the data-write time step.
        if (mod(iter,nint(print_inc)).eq.0) then

c Mask out the ocean points (vegtype.eq.24.0) if this is
c   a land run (seaice_run = 0.0).  Mask out all land points
c   (vegtype.ne.24.0) if this is an ocean/sea ice run
c   (seaice_run = 1.0, 3.0, or 4.0).  Do not mask out anything
c   if this this is a combined land and sea ice run (seaice_run
c   = 2.0).
          if (seaice_run.eq.0.0) then
            do j=1,ny
              do i=1,nx
                if (vegtype(i,j).eq.24.0) then
                  do k=1,n_print_vars
                    vars(i,j,k) = undef
                  enddo
                endif
              enddo
            enddo
          elseif (seaice_run.eq.1.0 .or. seaice_run.eq.3.0 .or.
     &      seaice_run.eq.4.0) then
            do j=1,ny
              do i=1,nx
                if (vegtype(i,j).ne.24.0) then
                  do k=1,n_print_vars
                    vars(i,j,k) = undef
                  enddo
                endif
              enddo
            enddo
          endif

c Write out the data.
          irec = iter / nint(print_inc)
          if (icorr_factor_loop.eq.1) then
            do k=1,n_print_vars
              if (print_var(k).eq.'y') then
                write (220+k,rec=irec) ((vars(i,j,k),i=1,nx),j=1,ny)
              endif
            enddo
          elseif (icorr_factor_loop.eq.2) then
            do k=1,n_print_vars
              if (print_var(k).eq.'y') then
                write (320+k,rec=irec) ((vars(i,j,k),i=1,nx),j=1,ny)
              endif
            enddo
          endif

c Reinitialize the averaging and summing arrays.
          do j=1,ny
            do i=1,nx
              do k=1,n_print_vars
                vars(i,j,k) = 0.0
              enddo
              uwnd(i,j) = 0.0
              vwnd(i,j) = 0.0
            enddo
          enddo
        endif

c Use more than one variable in an output file.
      else

        print *,'Use more than one variable in an output file:'
        print *,'  THIS HAS NOT BEEN IMPLEMENTED YET'

      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c THIS IS AN EXAMPLE OF SAVING DATA IN ASCII/TEXT FORMAT.

c I have completely removed this example; I now do this with
c   improved codes as part of post-processing steps.  It is
c   just too slow to do it as part of the model simulation.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c THE CODE BELOW WAS USED TO SAVE AVERAGES OVER POLYGONS.

c I have completely removed this example; if I were to do this
c   again I would do it as a post-processing step.  If you really
c   want to see what I did here, you can look in one of the pre-
c   2018 code distributions.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c preprocess_code.f

c Perform a variety of preprocessing steps, like read in topography
c   and vegetation arrays, open input and output files, etc.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE PREPROCESS_CODE(topoveg_fname,const_veg_flag,
     &  vegtype,veg_z0,vegsnowdepth,fetch,xmu,C_z,h_const,
     &  wind_min,Up_const,dz_susp,ztop_susp,fall_vel,Ur_const,
     &  ro_water,ro_air,gravity,vonKarman,pi,twopio360,snow_z0,
     &  nx,ny,sum_sprec,sum_qsubl,sum_trans,sum_unload,topo,
     &  topo_land,snow_d,topoflag,snow_d_init,snow_d_init_const,
     &  soft_snow_d,met_input_fname,igrads_metfile,deltax,deltay,
     &  snowtran_output_fname,micromet_output_fname,
     &  enbal_output_fname,snowpack_output_fname,print_micromet,
     &  print_enbal,print_snowpack,print_snowtran,run_micromet,
     &  run_enbal,run_snowpack,run_snowtran,ro_snow_grid,swe_depth,
     &  sum_runoff,sum_prec,ro_snow,twolayer_flag,sum_Qcs,
     &  canopy_int,ascii_topoveg,topo_ascii_fname,icorr_factor_loop,
     &  veg_ascii_fname,undef,isingle_stn_flag,max_iter,
     &  i_tair_flag,i_rh_flag,i_wind_flag,i_prec_flag,sum_glacmelt,
     &  snow_depth,sum_d_canopy_int,corr_factor,icorr_factor_index,
     &  sum_sfcsublim,barnes_lg_domain,n_stns_used,k_stn,xmn,ymn,
     &  ro_soft_snow_old,sum_swemelt,xlat,lat_solar_flag,xlat_grid,
     &  xlon_grid,UTC_flag,dt,swe_depth_old,canopy_int_old,
     &  vegsnowd_xy,iveg_ht_flag,ihrestart_flag,i_dataassim_loop,
     &  multilayer_snowpack,max_layers,multilayer_output_fname,
     &  print_multilayer,KK,tslsnowfall,tsls_threshold,
     &  irun_data_assim,izero_snow_date,iclear_mn,iclear_dy,
     &  xclear_hr,snod_layer,swed_layer,ro_layer,T_old,gamma,
     &  icond_flag,curve_lg_scale_flag,curve_wt_lg,check_met_data,
     &  seaice_run,snowmodel_line_flag,xg_line,yg_line,print_user,
     &  cf_precip_flag,cf_precip,print_inc,xhour_init,Tabler_1_flag,
     &  Tabler_2_flag,iyear_init,imonth_init,iday_init,print_var,
     &  output_path_wo_assim,output_path_wi_assim,nrecs_max,
     &  tabler_sfc_path_name,print_outvars,diam_layer)

      implicit none

      include 'snowmodel.inc'

      integer i,j,k,nx,ny,igrads_metfile,n_recs_out,iheader,
     &  isingle_stn_flag,max_iter,i_tair_flag,i_rh_flag,i_wind_flag,
     &  i_prec_flag,iter,iobs_num,n_stns_used,nveg,iveg_ht_flag,
     &  lat_solar_flag,ihrestart_flag,nstns_orig,i_dataassim_loop,
     &  multilayer_snowpack,max_layers,irun_data_assim,
     &  izero_snow_date,iclear_mn,iclear_dy,icond_flag,
     &  iyear_init,imonth_init,iday_init

      real ro_water,ro_air,gravity,vonKarman,snow_z0,
     &  fetch,xmu,C_z,h_const,wind_min,Up_const,check_met_data,
     &  dz_susp,ztop_susp,fall_vel,Ur_const,pi,twopio360,topoflag,
     &  snow_d_init_const,const_veg_flag,ro_snow,twolayer_flag,
     &  ascii_topoveg,undef,barnes_lg_domain,xlat,UTC_flag,dt,
     &  print_multilayer,xclear_hr,curve_lg_scale_flag,seaice_run,
     &  snowmodel_line_flag,print_user,print_inc,xhour_init,
     &  Tabler_1_flag,Tabler_2_flag

      real topo_land(nx_max,ny_max)
      real topo(nx_max,ny_max)
      real vegtype(nx_max,ny_max)
      real xlat_grid(nx_max,ny_max)
      real xlon_grid(nx_max,ny_max)

      real snow_d(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real snow_d_init(nx_max,ny_max)
      real canopy_int(nx_max,ny_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)

      real sum_sprec(nx_max,ny_max)
      real sum_qsubl(nx_max,ny_max)
      real sum_trans(nx_max,ny_max)
      real sum_unload(nx_max,ny_max)
      real soft_snow_d(nx_max,ny_max)
      real ro_soft_snow_old(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real sum_prec(nx_max,ny_max)
      real sum_runoff(nx_max,ny_max)
      real sum_Qcs(nx_max,ny_max)
      real sum_glacmelt(nx_max,ny_max)
      real sum_swemelt(nx_max,ny_max)
      real sum_d_canopy_int(nx_max,ny_max)
      real sum_sfcsublim(nx_max,ny_max)

      real vegsnowdepth(nvegtypes)
      real veg_z0(nx_max,ny_max)
      real vegsnowd_xy(nx_max,ny_max)

      real curve_wt_lg(nx_max,ny_max)

      real corr_factor(nx_max,ny_max,max_obs_dates+1)
      integer icorr_factor_index(max_time_steps)
      integer icorr_factor_loop

      integer k_stn(nx_max,ny_max,9)
      double precision xmn,ymn
      double precision nrecs_max,nrecs
      real deltax,deltay
      integer icount,iii,jjj
      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)

      real run_micromet,run_enbal,run_snowpack,run_snowtran
      real print_micromet,print_enbal,print_snowpack,print_snowtran

      character*80 topoveg_fname,met_input_fname,topo_ascii_fname,
     &  veg_ascii_fname
      character*80 snowtran_output_fname,micromet_output_fname,
     &  enbal_output_fname,snowpack_output_fname,
     &  multilayer_output_fname

      character*80 tabler_sfc_path_name
      character*80 output_path_wo_assim,output_path_wi_assim

      character*1 print_var(n_print_vars)
      character*4 print_outvars(n_print_vars)

      integer KK(nx_max,ny_max)
      real tslsnowfall(nx_max,ny_max)
      real tsls_threshold
      real snod_layer(nx_max,ny_max,nz_max)
      real swed_layer(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real gamma(nx_max,ny_max,nz_max)
      real diam_layer(nx_max,ny_max,nz_max)

      real cf_precip(nx_max,ny_max)
      real cf_precip_flag,cf_precip_scalar

      integer ipath_length,i_len_wo,i_len_wi,trailing_blanks
      character*80 vege_ht_fname

      integer nyears,nyear,nobs_total,nobs_dates,nstns,krec

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c seaice_run = 3.0 is no longer supported.  If seaice_run has been
c   set to 3.0 in the .par file, send a message describing your
c   options.
      if (seaice_run.eq.3.0) then
        print *, 'Eulerian sea ice runs are not longer supported'
        print *, '(seaice_run = 3.0).  If you want to restart this'
        print *, 'option, see the notes in here:'
        print *, '/sm/misc_programs/Eulerian_incremental_remapper/'
        stop
      endif

c Check to see whether the maximum array dimensions defined in
c   snowmodel.inc are adequate for the simulation domain defined
c   in snowmodel.par.
      if (snowmodel_line_flag.eq.0.0) then
        if (seaice_run.eq.3.0) then
          if (nx.ne.nx_max .or. ny.ne.ny_max) then
            print *, 'For a sea ice remapping run, nx==nx_max'
            print *, '  and ny==ny_max in the snowmodel.par and'
            print *, '  snowmodel.inc.'
            stop
          endif
        else
          if (nx+1.gt.nx_max .or. ny+1.gt.ny_max) then
            print *, 'Must increase the value of nx_max or ny_max'
            print *, '  in snowmodel.inc to be greater than nx+1'
            print *, '  and/or ny+1.'
            print *, 'nx_max = ',nx_max,'  ny_max = ',ny_max
            print *, 'nx = ',nx,'  ny = ',ny
            stop
          endif
        endif
      else
        if (nx.ge.nx_max .or. ny.ne.1 .or. ny_max.ne.2) then
          print *, 'For snowmodel_line_flag = 1.0, we suggest setting'
          print *, 'nx = number of grid cells, ny = 1, nx_max = nx+1,'
          print *, 'and ny_max = ny+1 = 2 in snowmodel.inc.'
          print *, '  The current values are:'
          print *, '    nx_max = ',nx_max,'  ny_max = ',ny_max
          print *, '    nx = ',nx,'  ny = ',ny
          stop
        endif
      endif

      if (multilayer_snowpack.eq.1) then
        if (max_layers+1.gt.nz_max) then
          print *, 'nz_max in snowmodel.inc must be at least 1 greater'
          print *, '  than max_layers in the snowmodel.inc file.  So,'
          print *, '  if you want to run the multi-layer snowpack model'
          print *, '  with a single snow layer, set nz_max=2.  If you'
          print *, '  want to run the original single-layer snowpack'
          print *, '  model, you can set nz_max=1 in snowmodel.inc.'
          print *, 'nz_max = ',nz_max
          print *, 'max_layers = ',max_layers
          stop
        endif
      endif

      if (max_iter.gt.max_time_steps) then
        print *, 'Must increase the value of max_time_steps'
        print *, '  in snowmodel.inc to be greater than max_iter.'
        print *, 'max_time_steps = ',max_time_steps
        print *, 'max_iter = ',max_iter
        stop
      endif

c If running the concatenated configuration of the model, check to
c   make sure the rest of the model is configured correctly.
      if (snowmodel_line_flag.eq.1.0) then
        if (run_snowtran.eq.1.0 .and. seaice_run.ne.4.0) then
          print *, 'You cannot run snowmodel_line_flag = 1.0 with'
          print *, 'run_snowtran = 1.0'
        stop
        endif
        if (barnes_lg_domain.eq.0.0) then
          print *, 'If snowmodel_line_flag = 1.0, then you must run'
          print *, 'the model with barnes_lg_domain = 1.0'
        stop
        endif
      endif

c Make sure the time since last snowfall treshold is not less
c   than the model time step.
      if (multilayer_snowpack.eq.1) then
        if (tsls_threshold.lt.dt/3600.0) then
          print *,'Need to correct tsls_threshold to'
          print *,'  be >= dt (in hours).'
          stop
        endif
      endif

c Check the model dt value, and send an error message if dt < 3600.
      if (dt.lt.3600.0) then
        print *, 'You must modify the hour fraction calculation'
        print *, '  in get_model_time subroutine to handle'
        print *, '  dt values less that 3600.0 seconds.'
        print *, 'dt = ',dt
        stop
      endif

c Define the date on which the snow arrays will be zeroed out.
      iclear_mn = izero_snow_date / 10000
      iclear_dy = (izero_snow_date - iclear_mn * 10000) / 100
      xclear_hr =
     &  real((izero_snow_date - iclear_mn * 10000) - iclear_dy * 100)

c Check to see whether there is enough snow layers to calculate
c   conductive surface fluxes.
      if (icond_flag.eq.1) then
        if (multilayer_snowpack.eq.0 .or. max_layers.lt.2) then
          print *,'If icond_flag = 1, then multilayer_snowpack = 1'
          print *,'  and max_layers >= 2.'
          stop
        endif
      endif

c Read in the topography array.
      if (ascii_topoveg.eq.0.0) then

        open (unit=37,file=topoveg_fname,
     &    form='unformatted',access='direct',recl=4*nx*ny)
        read (37,rec=1) ((topo_land(i,j),i=1,nx),j=1,ny)

      elseif (ascii_topoveg.eq.1.0) then

c Read off the header lines.  I will assume that all of this
c   information was input in the .par file correctly.
        open (37,file=topo_ascii_fname,form='formatted')
        iheader = 6
        do k=1,iheader
          read (37,*)
        enddo
c Read the data in as real numbers, and do the yrev.
        do j=ny,1,-1
          read (37,*) (topo_land(i,j),i=1,nx)
        enddo

      endif

c If vegetation data is not available on the topography grid,
c   define the vegetation to be constant.
      if (const_veg_flag.ne.0.0) then
        do i=1,nx
          do j=1,ny
            vegtype(i,j) = const_veg_flag
          enddo
        enddo

c Read in the vegetation array.
      else

        if (ascii_topoveg.eq.0.0) then
          read (37,rec=2) ((vegtype(i,j),i=1,nx),j=1,ny)

        elseif (ascii_topoveg.eq.1.0) then

c Read off the header lines.  I will assume that all of this
c   information was input in the .par file correctly.
          open (38,file=veg_ascii_fname,form='formatted')
          do k=1,iheader
            read (38,*)
          enddo
c Read the data in as real numbers, and do the yrev.
          do j=ny,1,-1
            read (38,*) (vegtype(i,j),i=1,nx)
          enddo

        endif

      endif

c Now that we have read in the topo and veg data arrays, check
c   whether all of the values look like valid numbers.
      do i=1,nx
        do j=1,ny
          if (vegtype(i,j).lt.1.0 .or. vegtype(i,j).gt.30.0) then
            print *, 'Found Invalid Vegetation-Type Value'
            print *, '     Value =',vegtype(i,j),'  at ',i,j
            stop
          endif

          if (topo_land(i,j).lt.0.0 .or. topo_land(i,j).gt.9000.0) then
            print *, 'Found Invalid Topography Value'
            print *, '     Value =',topo_land(i,j),'  at ',i,j
            stop
          endif
        enddo
      enddo

c Fill the the vegetation snow-holding depth array for vegetation
c   types 1 through 24 (types 25 through 30 were filled from the
c   .par file.
      call fill_veg_shd(nvegtypes,vegsnowdepth)

c Use vegsnowdepth to fill the 2D spatial array, or read in the
c   user-provided file of the vegetation heights (in m).
      if (iveg_ht_flag.eq.-1) then

c Find the last occurance of '/' in the topo_vege path.
        ipath_length = 0
        do k=1,len(topoveg_fname)
          if (topoveg_fname(k:k).eq.'/') then
            ipath_length = k
          endif
        enddo

        if (ipath_length.eq.0) then
          print *,'vege_ht_fname path not found; looking'
          print *,'  in the SnowModel run directory'
        endif

        vege_ht_fname =
     &    topoveg_fname(1:ipath_length)//'veg_ht.gdat'

        open (191,file=vege_ht_fname,
     &    form='unformatted',access='direct',recl=4*nx*ny)
        read (191,rec=1) ((vegsnowd_xy(i,j),i=1,nx),j=1,ny)
        close(191)

      elseif (iveg_ht_flag.eq.1) then

c Find the last occurance of '/' in the veg_ascii_fname path.
        ipath_length = 0
        do k=1,len(veg_ascii_fname)
          if (veg_ascii_fname(k:k).eq.'/') then
            ipath_length = k
          endif
        enddo

        if (ipath_length.eq.0) then
          print *,'vege_ht_fname path not found; looking'
          print *,'  in the SnowModel run directory'
        endif

        vege_ht_fname =
     &    veg_ascii_fname(1:ipath_length)//'veg_ht.asc'

        open (191,file=vege_ht_fname,form='formatted')
        iheader = 6
        do k=1,iheader
          read (191,*)
        enddo
c Read the data in as real numbers, and do the yrev.
        do j=ny,1,-1
          read (191,*) (vegsnowd_xy(i,j),i=1,nx)
        enddo
        close(191)

      elseif (iveg_ht_flag.eq.0) then

        do i=1,nx
          do j=1,ny
            nveg = nint(vegtype(i,j))
            vegsnowd_xy(i,j) = vegsnowdepth(nveg)
          enddo
        enddo

      endif

c Define the roughness lengths for each of the vegetation types.
c   Note that this is pretty arbitrary, because these values are
c   really only used when there is no blowing snow, and thus have
c   no impact on the simulation except to provide a non-zero value
c   for any such parts of the domain.
      do i=1,nx
        do j=1,ny
          veg_z0(i,j) = 0.25 * vegsnowd_xy(i,j)
        enddo
      enddo

c Read in the large-scale curvature weighting array, if the run
c   requires it.
      if (curve_lg_scale_flag.eq.1.0) then
        open (444,file='extra_met/large_curvature_wt.gdat',
     &    form='unformatted',access='direct',recl=4*nx*ny)
        read (444,rec=1) ((curve_wt_lg(i,j),i=1,nx),j=1,ny)
        close (444)
      endif

c If this is a sea ice run, open the sea ice concentration file.
      if (seaice_run.ne.0.0) then
        open (445,file='seaice/ice_conc.gdat',
     &    form='unformatted',access='direct',recl=4*nx*ny)
      endif

c If this is a Lagrangian sea ice parcel trajectory simulation,
c   do some setup checking.
      if (seaice_run.eq.4.0) then
        if (barnes_lg_domain.ne.1.0 .or. n_stns_used.ne.1.0 .or.
     &    snowmodel_line_flag.ne.1.0) then
          print *
          print *,'if seaice_run = 4.0, then'
          print *,'  barnes_lg_domain = 1.0'
          print *,'  n_stns_used = 1'
          print *,'  snowmodel_line_flag = 1.0'
          print *
          stop
        endif
      endif

c Check to make sure that if you are running SnowTran-3D and the
c   EnBal and SnowPack models, you have also set the flag to run
c   the SnowTran-3D two-layer submodel.
      if (run_enbal.eq.1.0 .and. run_snowpack.eq.1.0 .and.
     &  run_snowtran.eq.1.0) then
        if (twolayer_flag.ne.1.0) then
          print *, 'For SnowTran-3D with EnBal and SnowPack,'
          print *, '  twolayer_flag must equal 1.0'
          stop
        endif
      endif

c Check to see that the defined domain is large enough to be
c   running SnowTran-3D.
      if (run_snowtran.eq.1.0 .and. seaice_run.ne.4.0) then
        if (nx.lt.3 .or. ny.lt.3) then
          print *, 'To run SnowTran-3D, nx and ny must both be 3'
          print *, '  or greater (see SnowTran-3D code/notes)'
          stop
        endif
      endif

c Check to see whether the model is configured correctly to be
c   running the multi-layer snow model.
      if (multilayer_snowpack.eq.1) then
        if (ihrestart_flag.ne.-2 .or. snow_d_init_const.ne.0.0) then
          print *, 'The multi-layer snowpack model requires:'
          print *, '  ihrestart_flag = -2'
          print *, '  snow_d_init_const = 0.0'
          stop
        endif
      endif

c Get a collection of constants that are not generally modified.
      call constants(fetch,xmu,C_z,h_const,wind_min,Up_const,
     &  dz_susp,ztop_susp,fall_vel,Ur_const,ro_water,ro_air,
     &  gravity,vonKarman,pi,twopio360,snow_z0)

c Run a check to see if SnowTran-3D is being run with grid
c   increments that are too large.
      if (deltax.gt.500.0 .or. deltay.gt.500.0) then
        if (seaice_run.eq.0.0) then
          if (run_snowtran.eq.1.0) then
            print *
            print *, '!!! deltax,y should not be greater than 500 m'
            print *, '!!!    if you are also running SnowTran-3D'
            print *
            stop
          endif
        endif
      endif

c Initialize the summing arrays, and define the initial snow-depth
c   distributions.
      call initialize(nx,ny,sum_sprec,sum_qsubl,sum_trans,
     &  sum_unload,topo,topo_land,snow_d,topoflag,snow_d_init,
     &  snow_d_init_const,soft_snow_d,ro_water,sum_sfcsublim,
     &  ro_snow_grid,swe_depth,sum_runoff,sum_prec,ro_snow,
     &  sum_Qcs,canopy_int,sum_glacmelt,snow_depth,sum_d_canopy_int,
     &  ro_soft_snow_old,sum_swemelt,swe_depth_old,canopy_int_old,
     &  ihrestart_flag,i_dataassim_loop,max_iter,corr_factor,
     &  icorr_factor_index,KK,tslsnowfall,tsls_threshold,snod_layer,
     &  swed_layer,ro_layer,T_old,gamma,diam_layer)

c Check to see whether the data assimilation has been configured
c   correctly.
      if (irun_data_assim.eq.1) then

c Check to see whether the required output files will be created.
        if (print_user.ne.1.0) then
          print *, 'For a data assimilation run print_user must = 1.0'
          stop
        endif

        if (print_var(19).ne.'y') then
          print *, 'print_var_19 == y for a data assimilation run'
          stop
        endif

        if (print_var(20).ne.'y') then
          print *, 'print_var_20 == y for a data assimilation run'
          stop
        endif

c Check to see whether the corr_factor array is defined in the
c   snowmodel.inc file to be large enough to do the assimilation.
c   max_obs_dates is used in the data assimilation routines.  It
c   must be greater than or equal to the number of observation
c   dates in the entire simulation + (plus) the number of years
c   in the simulation.  For example, for a 6-year simulation with
c   2 observation dates in each year, you would set max_obs_dates
c   to be = 2obs*6yrs+6yrs = 18 or greater.  For a 6-year run with
c   4 observation dates in 2 of the years, and 0 observation dates
c   in the other 4 years, max_obs_dates = 8obs+6yrs = 14 or
c   greater.
c     parameter (max_obs_dates=18)
        if (icorr_factor_loop.eq.1) then
          open (unit=61,file='swe_assim/swe_obs.dat')
          read(61,*) nyears
          nobs_total = 0
          do nyear=1,nyears
            read(61,*) nobs_dates
            if (nobs_dates.gt.0) then
              nobs_total = nobs_total + nobs_dates
              do iobs_num=1,nobs_dates
c               read(61,*) iiyr,iimo,iidy
                read(61,*)
                read(61,*) nstns
                do k=1,nstns
c                 read(61,*) obsid(k),xstn(k),ystn(k),swe_obs(k)
                  read(61,*)
                enddo
              enddo
            endif
          enddo
          close (61)
          krec = nobs_total + nyears
          if (krec.gt.max_obs_dates) then
          print *
          print *, 'For a DATA ASSIMILATION RUN, MAX_OBS_DATES must be'
          print *, 'defined in SNOWMODEL.INC to be greater than the'
          print *, 'number of obs dates in the entire simulation +'
          print *, '(plus) the number of years in the simulation.  For'
          print *, 'example, for a 6-year simulation with 2 observation'
          print *, 'dates in each year, you would set max_obs_dates to'
          print *, 'be = 2obs*6yrs+6yrs = 18 or greater.  For a 6-year'
          print *, 'run with 4 observation dates in 2 of the years,'
          print *, 'and 0 observation dates in the other 4 years,'
          print *, 'max_obs_dates = 8obs+6yrs = 14 or greater.'
          print *
          print *, 'max_obs_dates must be increased in snowmodel.inc'
          print *, 'It looks like you should set max_obs_dates = ',krec
          print *, 'Right now, max_obs_dates = ',max_obs_dates
          print *
          stop
          endif
        endif
      endif

c Initialize the precipitation factor for the first iteration to
c   equal 1.0.
      if (icorr_factor_loop.eq.1) then
        do iobs_num=1,max_obs_dates+1
          do j=1,ny
            do i=1,nx
              corr_factor(i,j,iobs_num) = 1.0
            enddo
          enddo
        enddo
        do iter=1,max_iter
          icorr_factor_index(iter) = 1
        enddo
      endif

c Read or build the latitude array that will be used to do the
c   latitude weighting when calculating incoming solar radiation.
      if (lat_solar_flag.eq.-1) then

        open (91,file='extra_met/grid_lat.gdat',
     &    form='unformatted',access='direct',recl=4*nx*ny)
        read (91,rec=1) ((xlat_grid(i,j),i=1,nx),j=1,ny)
        close(91)

      elseif (lat_solar_flag.eq.1) then

        open (91,file='extra_met/grid_lat.asc',form='formatted')
        iheader = 6
        do k=1,iheader
          read (91,*)
        enddo
c Read the data in as real numbers, and do the yrev.
        do j=ny,1,-1
          read (91,*) (xlat_grid(i,j),i=1,nx)
        enddo
        close(91)

      elseif (lat_solar_flag.eq.0) then

c Print an error if the y-domain is big enough to have important
c   solar radiation differences from south to north.
        if (ny*deltay.gt.500000.0) then
          print *
          print *,'YOUR DOMAIN IS PRETTY BIG TO NOT ACCOUNT FOR'
          print *,'  SOLAR RADIATION VARIATIONS WITH LATITUDE'
          print *,' see the "lat_solar_flag" in snowmodel.par'
          print *
          stop
        endif

        do i=1,nx
          do j=1,ny
            xlat_grid(i,j) = xlat
          enddo
        enddo

      endif

c Read or build the longitude array that will be used to do the
c   longitude influence when calculating incoming solar radiation.
      if (UTC_flag.eq.-1.0) then

        open (91,file='extra_met/grid_lon.gdat',
     &    form='unformatted',access='direct',recl=4*nx*ny)
        read (91,rec=1) ((xlon_grid(i,j),i=1,nx),j=1,ny)
        close(91)

      elseif (UTC_flag.eq.1.0) then

        open (91,file='extra_met/grid_lon.asc',form='formatted')
        iheader = 6
        do k=1,iheader
          read (91,*)
        enddo
c Read the data in as real numbers, and do the yrev.
        do j=ny,1,-1
          read (91,*) (xlon_grid(i,j),i=1,nx)
        enddo
        close(91)

      elseif (UTC_flag.eq.0.0) then

c Print an error if the x-domain is big enough to have important
c   solar radiation differences from east to west.
        if (nx*deltax.gt.500000.0 .and. seaice_run.ne.4) then
          print *
          print *,'YOUR DOMAIN IS PRETTY BIG TO NOT ACCOUNT FOR'
          print *,'  SOLAR RADIATION VARIATIONS WITH LONGITUDE'
          print *,'    see the "UTC_flag" in snowmodel.par'
          print *
        endif

      endif

c Open the MicroMet station data input file.
      if (igrads_metfile.eq.1) then
        open(20,file=met_input_fname,form='unformatted',
     &    access='direct',recl=4*13)
      else
        open (20,file=met_input_fname,form='formatted')
      endif

c Run a check to see whether there are any time slices with no
c   valid data.
      if (check_met_data.eq.1.0) then
        print *
        print *,'Checking for sufficient met forcing data to'
        print *,'  complete the model simulation.  This may'
        print *,'  take a while, depending on how big your met'
        print *,'  input file is.'
        print *
        call met_data_check(undef,isingle_stn_flag,igrads_metfile,
     &    max_iter,i_tair_flag,i_rh_flag,i_wind_flag,i_prec_flag)
      endif

c If the concatenated configuration of the model is used, read
c   in the x and y coordinates for the concatenated grid cells.
      if (snowmodel_line_flag.eq.1.0) then
        open (1331,file='extra_met/snowmodel_line_pts.dat')
        do j=1,ny
          do i=1,nx
            read (1331,*) icount,iii,jjj,xg_line(i,j),yg_line(i,j)
          enddo
        enddo
        close (1331)
      endif

c If the large-domain barnes oi scheme is used, generate the
c   nearest-station indexing array.
      if (barnes_lg_domain.eq.1.0) then
        print *
        print *,'You are running the large-domain Barnes oi scheme'
        print *,'  This requires:'
        print *,'  1) no missing data for the fields of interest'
        print *,'  2) no missing stations during the simulation' 
        print *,'  3) met file must list stations in the same order'
        print *,'  4) the number of nearest stations used is 9 or less'
        print *,'  5)  **** no error checking for this is done ****'
        print *
        print *,'Generating nearest-station index.  Be patient.'
        print *
        if (n_stns_used.gt.9 .or. n_stns_used.lt.1) then
          print *,'invalid n_stns_used value'
          stop
        endif
        call get_nearest_stns_1(nx,ny,xmn,ymn,deltax,deltay,
     &    n_stns_used,k_stn,snowmodel_line_flag,xg_line,yg_line)
      endif

c If this is a history restart run, advance the micromet input
c   file to the restart time.
      if (ihrestart_flag.ge.0) then
        if (igrads_metfile.eq.0) then
          do iter=1,ihrestart_flag
            if (isingle_stn_flag.eq.1) then
              nstns_orig = 1
            else
              read(20,*) nstns_orig
            endif
            do k=1,nstns_orig
              read(20,*)
            enddo
          enddo
        endif
      endif

c Open the files to be used to store model output.

c nrecs_max corresponds to the approximately 2.1 GB Fortran
c   array limit for direct access binary inputs and outputs.
c   The number listed here corresponds to nx = ny = 23170.
c   "nrecs_max * 4 bytes per number" gives the 2.1 GB limit.
      nrecs_max = 536848900

c   For MicroMet.
      if (run_micromet.eq.1.0 .and. print_micromet.eq.1.0) then
        n_recs_out = 9
        nrecs = n_recs_out * nx * ny
        if (nrecs.gt.nrecs_max) then
          print *,'Your simulation domain has too many grid cells'
          print *,'to print the micromet.gdat file.  You must set'
          print *,'print_micromet = 0.0 and use print_user = 1.0.'
          stop
        else
          if (icorr_factor_loop.eq.2) close (81)
          open (81,file=micromet_output_fname,
     &      form='unformatted',access='direct',recl=4*n_recs_out*nx*ny,
     &      status='replace')
        endif
      endif

c   For EnBal.
      if (run_enbal.eq.1.0 .and. print_enbal.eq.1.0) then
        n_recs_out = 11
        nrecs = n_recs_out * nx * ny
        if (nrecs.gt.nrecs_max) then
          print *,'Your simulation domain has too many grid cells'
          print *,'to print the enbal.gdat file.  You must set'
          print *,'print_enbal = 0.0 and use print_user = 1.0.'
          stop
        else
          if (icorr_factor_loop.eq.2) close (82)
          open (82,file=enbal_output_fname,
     &      form='unformatted',access='direct',recl=4*n_recs_out*nx*ny,
     &      status='replace')
        endif
      endif

c   For SnowPack.
      if (run_snowpack.eq.1.0 .and. print_snowpack.eq.1.0) then
        n_recs_out = 16
        nrecs = n_recs_out * nx * ny
        if (nrecs.gt.nrecs_max) then
          print *,'Your simulation domain has too many grid cells'
          print *,'to print the snowpack.gdat file.  You must set'
          print *,'print_snowpack = 0.0 and use print_user = 1.0.'
          stop
        else
          if (icorr_factor_loop.eq.2) close (83)
          open (83,file=snowpack_output_fname,
     &      form='unformatted',access='direct',recl=4*n_recs_out*nx*ny,
     &      status='replace')
        endif
      endif

c   For SnowTran-3D.
      if (run_snowtran.eq.1.0 .and. print_snowtran.eq.1.0) then
        n_recs_out = 7
        nrecs = n_recs_out * nx * ny
        if (nrecs.gt.nrecs_max) then
          print *,'Your simulation domain has too many grid cells'
          print *,'to print the snowtran.gdat file.  You must set'
          print *,'print_snowtran = 0.0 and use print_user = 1.0.'
          stop
        else
          if (icorr_factor_loop.eq.2) close (84)
          open (84,file=snowtran_output_fname,
     &      form='unformatted',access='direct',recl=4*n_recs_out*nx*ny,
     &      status='replace')
        endif
      endif

c   For Multi-Layer SnowPack.
      if (run_snowpack.eq.1.0 .and. multilayer_snowpack.eq.1 .and.
     &  print_multilayer.eq.1.0) then
        nrecs = 4 * nx * ny + 4 * nx * ny * nz_max
        if (nrecs.gt.nrecs_max) then
          print *,'Your simulation domain has too many grid cells'
          print *,'to print the multilayer.gdat file.  Since you'
          print *,'clearly want this information, it will be written'
          print *,'to the directory you defined in the .par file for'
          print *,'the parameter "output_path_wo_assim".'
        else
          if (icorr_factor_loop.eq.2) close (401)
          open (401,file=multilayer_output_fname,
     &      form='unformatted',access='direct',
     &      recl=4*(4*nx*ny+4*nx*ny*nz_max),
     &      status='replace')
        endif
      elseif (run_snowpack.eq.1.0 .and. multilayer_snowpack.eq.1 .and.
     &  print_multilayer.eq.2.0) then
        nrecs = nx * ny * nz_max
        if (nrecs.gt.nrecs_max) then
          print *,'Your simulation domain has too many grid cells'
          print *,'to print the print_multilayer = 2.0 files.  You'
          print *,'will have to restructure the write statements.'
          print *,'See the example in the outputs_user.f subroutine'
          print *,'where it does the "if (nrecs.gt.nrecs_max) then"'
          print *,'test.'
          stop
        else

         if (icorr_factor_loop.eq.1) then

          i_len_wo = 80 - trailing_blanks(output_path_wo_assim)
          open (401,
     &    file=output_path_wo_assim(1:i_len_wo)//'multilayer_2Dxy.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*(4*nx*ny),status='replace')
          open (402,
     &    file=output_path_wo_assim(1:i_len_wo)//'multilayer_snod.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (403,
     &    file=output_path_wo_assim(1:i_len_wo)//'multilayer_sden.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (404,
     &    file=output_path_wo_assim(1:i_len_wo)//'multilayer_swed.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (405,
     &    file=output_path_wo_assim(1:i_len_wo)//'multilayer_diam.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (406,
     &    file=output_path_wo_assim(1:i_len_wo)//'multilayer_flux.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (407,
     &    file=output_path_wo_assim(1:i_len_wo)//'multilayer_temp.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (408,
     &    file=output_path_wo_assim(1:i_len_wo)//'multilayer_cond.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')

         elseif (icorr_factor_loop.eq.2) then

          close (401)
          close (402)
          close (403)
          close (404)
          close (405)
          close (406)
          close (407)
          close (408)

          i_len_wi = 80 - trailing_blanks(output_path_wi_assim)
          open (401,
     &    file=output_path_wi_assim(1:i_len_wi)//'multilayer_2Dxy.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*(4*nx*ny),status='replace')
          open (402,
     &    file=output_path_wi_assim(1:i_len_wi)//'multilayer_snod.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (403,
     &    file=output_path_wi_assim(1:i_len_wi)//'multilayer_sden.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (404,
     &    file=output_path_wi_assim(1:i_len_wi)//'multilayer_swed.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (405,
     &    file=output_path_wi_assim(1:i_len_wi)//'multilayer_diam.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (406,
     &    file=output_path_wi_assim(1:i_len_wi)//'multilayer_flux.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (407,
     &    file=output_path_wi_assim(1:i_len_wi)//'multilayer_temp.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')
          open (408,
     &    file=output_path_wi_assim(1:i_len_wi)//'multilayer_cond.gdat',
     &      form='unformatted',access='direct',
     &      recl=4*nx*ny*nz_max,status='replace')

         endif
        endif
      endif

c Read in the precipitation correction factor array.
      if (cf_precip_flag.eq.1.0) then

        open (unit=144,file='precip_cf/cf_precip.gdat',
     &    form='unformatted',access='direct',recl=4*nx*ny)
        read (144,rec=1) ((cf_precip(i,j),i=1,nx),j=1,ny)

      elseif (cf_precip_flag.eq.2.0) then

c Read off the header lines.  I will assume that all of this
c   information was input in the .par file correctly.
        open (144,file='precip_cf/cf_precip.asc',form='formatted')
        iheader = 6
        do k=1,iheader
          read (144,*)
        enddo
c Read the data in as real numbers, and do the yrev.
        do j=ny,1,-1
          read (144,*) (cf_precip(i,j),i=1,nx)
        enddo

      elseif (cf_precip_flag.eq.3.0) then

        open (144,file='precip_cf/cf_precip.dat',form='formatted')
        read (144,*) cf_precip_scalar
        do j=1,ny
          do i=1,nx
            cf_precip(i,j) = cf_precip_scalar
          enddo
        enddo

      endif

c This must be closed so it can be reread if there is a (second)
c   data assimilation loop.
      close (144)

c Generate all of the GrADS control (.ctl) files that correspond
c   to all of the GrADS output (.gdat) files that were generated as
c   part of this model run.  They are (mostly) all placed in a
c   directory called "ctl_files".
      call mk_ctl_files(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  print_inc,iyear_init,imonth_init,iday_init,xhour_init,
     &  max_iter,undef,output_path_wo_assim,output_path_wi_assim,
     &  print_micromet,micromet_output_fname,print_enbal,
     &  enbal_output_fname,print_snowpack,snowpack_output_fname,
     &  print_snowtran,snowtran_output_fname,Tabler_1_flag,
     &  tabler_sfc_path_name,Tabler_2_flag,irun_data_assim,
     &  print_var,print_outvars,print_multilayer,
     &  multilayer_output_fname)

c If this is going to be a SnowTran-3D run, print the Copyright
c   header.
      if (run_snowtran.eq.1.0) then
        print *
        print *,
     & 'cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
        print *,
     & 'c     Snow-Transport Modeling System - 3D (SnowTran-3D)    c'
        print *,
     & 'c                    Copyright (C) 1998                    c'
        print *,
     & 'c          by Glen E. Liston, InterWorks Consulting        c'
        print *,
     & 'c                    All Rights Reserved                   c'
        print *,
     & 'cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
        print *
        print *
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine initialize(nx,ny,sum_sprec,sum_qsubl,sum_trans,
     &  sum_unload,topo,topo_land,snow_d,topoflag,snow_d_init,
     &  snow_d_init_const,soft_snow_d,ro_water,sum_sfcsublim,
     &  ro_snow_grid,swe_depth,sum_runoff,sum_prec,ro_snow,
     &  sum_Qcs,canopy_int,sum_glacmelt,snow_depth,sum_d_canopy_int,
     &  ro_soft_snow_old,sum_swemelt,swe_depth_old,canopy_int_old,
     &  ihrestart_flag,i_dataassim_loop,max_iter,corr_factor,
     &  icorr_factor_index,KK,tslsnowfall,tsls_threshold,snod_layer,
     &  swed_layer,ro_layer,T_old,gamma,diam_layer)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,ihrestart_flag,i_dataassim_loop,max_iter,k

      real topoflag,snow_d_init_const,ro_water,ro_snow
      real sum_sprec(nx_max,ny_max)
      real sum_qsubl(nx_max,ny_max)
      real sum_trans(nx_max,ny_max)
      real sum_unload(nx_max,ny_max)
      real topo(nx_max,ny_max)
      real topo_land(nx_max,ny_max)
      real snow_d(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real soft_snow_d(nx_max,ny_max)
      real ro_soft_snow_old(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real sum_prec(nx_max,ny_max)
      real sum_runoff(nx_max,ny_max)
      real sum_Qcs(nx_max,ny_max)
      real canopy_int(nx_max,ny_max)
      real sum_glacmelt(nx_max,ny_max)
      real sum_swemelt(nx_max,ny_max)
      real sum_d_canopy_int(nx_max,ny_max)
      real sum_sfcsublim(nx_max,ny_max)
      real snow_d_init(nx_max,ny_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)

      integer KK(nx_max,ny_max)
      real tslsnowfall(nx_max,ny_max)
      real tsls_threshold
      real snod_layer(nx_max,ny_max,nz_max)
      real swed_layer(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real gamma(nx_max,ny_max,nz_max)
      real diam_layer(nx_max,ny_max,nz_max)

      integer icorr_factor_index(max_time_steps)
      real corr_factor(nx_max,ny_max,max_obs_dates+1)

      if (ihrestart_flag.ge.0) then

c Read in the saved data.
        CALL HRESTART_READ(nx,ny,snow_d,snow_depth,
     &    canopy_int,soft_snow_d,ro_snow_grid,swe_depth,
     &    ro_soft_snow_old,snow_d_init,swe_depth_old,
     &    canopy_int_old,topo,sum_sprec,ihrestart_flag,
     &    i_dataassim_loop)

        if (i_dataassim_loop.lt.0.0) then
          CALL HRESTART_READ_DA(nx,ny,max_iter,corr_factor,
     &      icorr_factor_index,i_dataassim_loop)
        endif

        do i=1,nx
          do j=1,ny
c Fill the summing arrays.
            sum_runoff(i,j) = 0.0
            sum_prec(i,j) = 0.0
c           sum_sprec(i,j) = 0.0
            sum_qsubl(i,j) = 0.0
            sum_trans(i,j) = 0.0
            sum_unload(i,j) = 0.0
            sum_Qcs(i,j) = 0.0
            sum_glacmelt(i,j) = 0.0
            sum_swemelt(i,j) = 0.0
            sum_d_canopy_int(i,j) = 0.0
            sum_sfcsublim(i,j) = 0.0

c Define the initial snow-depth distributions.
c           snow_d_init(i,j) = snow_d_init_const
c           snow_d(i,j) = snow_d_init(i,j)
c           snow_depth(i,j) = snow_d_init(i,j)
c           canopy_int(i,j) = 0.0
c           soft_snow_d(i,j) = snow_d(i,j)
c           ro_snow_grid(i,j) = ro_snow
c           swe_depth(i,j) = snow_d(i,j) * ro_snow_grid(i,j) / ro_water
c           ro_soft_snow_old(i,j) = 50.0
c           swe_depth_old(i,j) = swe_depth(i,j)
c           canopy_int_old(i,j) = canopy_int(i,j)
          enddo
        enddo

      else

        do i=1,nx
          do j=1,ny
c Fill the summing arrays.
            sum_runoff(i,j) = 0.0
            sum_prec(i,j) = 0.0
            sum_sprec(i,j) = 0.0
            sum_qsubl(i,j) = 0.0
            sum_trans(i,j) = 0.0
            sum_unload(i,j) = 0.0
            sum_Qcs(i,j) = 0.0
            sum_glacmelt(i,j) = 0.0
            sum_swemelt(i,j) = 0.0
            sum_d_canopy_int(i,j) = 0.0
            sum_sfcsublim(i,j) = 0.0

c Define the initial snow-depth distributions.
            snow_d_init(i,j) = snow_d_init_const
            snow_d(i,j) = snow_d_init(i,j)
            snow_depth(i,j) = snow_d_init(i,j)
            canopy_int(i,j) = 0.0
            soft_snow_d(i,j) = snow_d(i,j)
            ro_snow_grid(i,j) = ro_snow
            swe_depth(i,j) = snow_d(i,j) * ro_snow_grid(i,j) / ro_water
            ro_soft_snow_old(i,j) = 50.0
            swe_depth_old(i,j) = swe_depth(i,j)
            canopy_int_old(i,j) = canopy_int(i,j)

c Initialize the multi-layer snowpack arrays.
            KK(i,j) = 0
            tslsnowfall(i,j) = tsls_threshold
          enddo
        enddo

        do i=1,nx
          do j=1,ny
            do k=1,nz_max
              snod_layer(i,j,k) = 0.0
              swed_layer(i,j,k) = 0.0
              ro_layer(i,j,k) = ro_snow
              T_old(i,j,k) = 273.15
              gamma(i,j,k) = 0.138 - 1.01 * (ro_layer(i,j,k)/1000.0) +
     &          3.233 * (ro_layer(i,j,k)/1000.0)**2
              diam_layer(i,j,k) = 0.5 / 1000.0
            enddo
          enddo
        enddo

        if (topoflag.eq.1.0) then
          do i=1,nx
            do j=1,ny
              topo(i,j) = topo_land(i,j) + snow_d(i,j)
            enddo
          enddo
        elseif (topoflag.eq.0.0) then
          do i=1,nx
            do j=1,ny
              topo(i,j) = topo_land(i,j)
            enddo
          enddo
        endif

      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine constants(fetch,xmu,C_z,h_const,wind_min,Up_const,
     &  dz_susp,ztop_susp,fall_vel,Ur_const,ro_water,ro_air,
     &  gravity,vonKarman,pi,twopio360,snow_z0)

      implicit none

      real fetch,xmu,C_z,h_const,wind_min,Up_const,
     &  dz_susp,ztop_susp,fall_vel,Ur_const,ro_water,ro_air,
     &  gravity,vonKarman,pi,twopio360,snow_z0

c These constants are not generally modified for a particular model
c   run.

c Snow surface roughness length.
      snow_z0 = 0.001

c Constants related to surface shear stress and saltation
c   transport.
      fetch = 500.0
      xmu = 3.0
      C_z = 0.12
      h_const = 1.6
      wind_min = 4.0

c Constants related to suspended snow profile.
      Up_const = 2.8
      dz_susp = 0.20
      ztop_susp = 2.0
      fall_vel = 0.3
      Ur_const = 0.5

c General constants.
      ro_water = 1000.0
      ro_air = 1.275
      gravity = 9.81
      vonKarman = 0.4
      pi = 2.0 * acos(0.0)
      twopio360 = 2.0 * pi / 360.0

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine fill_veg_shd(nvegtypes,vegsnowdepth)

      implicit none

      integer k,nvegtypes,nvegtypes_fixed

      parameter (nvegtypes_fixed=24)

      real vegsnowdepth(nvegtypes),vegsnowdepth_fixed(nvegtypes_fixed)

c Fill the the vegetation snow-holding depth array for
c   vegetation types 1 through 24 (types 25 through 30 were filled
c   from the .par file.
c
c The following summary was taken from the .par file.
c
c The vegetation types are assumed to range from 1 through 30.  The
c   last 6 types are available to be user-defined vegetation types
c   and vegetation snow-holding depth.  The first 24 vegetation
c   types, and the associated vegetation snow-holding depth
c   (meters), are hard-coded to be:
c
c code description           veg_shd  example                    class
c
c  1  coniferous forest       15.00  spruce-fir/taiga/lodgepole  forest
c  2  deciduous forest        12.00  aspen forest                forest
c  3  mixed forest            14.00  aspen/spruce-fir/low taiga  forest
c  4  scattered short-conifer  8.00  pinyon-juniper              forest
c  5  clearcut conifer         4.00  stumps and regenerating     forest
c 
c  6  mesic upland shrub       0.50  deeper soils, less rocky    shrub
c  7  xeric upland shrub       0.25  rocky, windblown soils      shrub
c  8  playa shrubland          1.00  greasewood, saltbush        shrub
c  9  shrub wetland/riparian   1.75  willow along streams        shrub
c 10  erect shrub tundra       0.65  arctic shrubland            shrub
c 11  low shrub tundra         0.30  low to medium arctic shrubs shrub
c 
c 12  grassland rangeland      0.15  graminoids and forbs        grass
c 13  subalpine meadow         0.25  meadows below treeline      grass
c 14  tundra (non-tussock)     0.15  alpine, high arctic         grass
c 15  tundra (tussock)         0.20  graminoid and dwarf shrubs  grass
c 16  prostrate shrub tundra   0.10  graminoid dominated         grass
c 17  arctic gram. wetland     0.20  grassy wetlands, wet tundra grass
c 
c 18  bare                     0.01                              bare
c
c 19  water/possibly frozen    0.01                              water
c 20  permanent snow/glacier   0.01                              water
c 
c 21  residential/urban        0.01                              human
c 22  tall crops               0.40  e.g., corn stubble          human
c 23  short crops              0.25  e.g., wheat stubble         human
c 24  ocean                    0.01                              water
c
c 25  user defined (see below)
c 26  user defined (see below)
c 27  user defined (see below)
c 28  user defined (see below)
c 29  user defined (see below)
c 30  user defined (see below)
c
c Define the vegetation snow-holding depth (meters) for each
c   of the user-defined vegetation types.  The numbers in the
c   list below correspond to the vegetation-type numbers in the
c   vegetation-type data array (veg type 25.0 -> veg_shd_25).  Note
c   that even if these are not used, they cannot be commented out
c   or you will get an error message.
c     veg_shd_25 = 0.10
c     veg_shd_26 = 0.10
c     veg_shd_27 = 0.10
c     veg_shd_28 = 0.10
c     veg_shd_29 = 0.10
c     veg_shd_30 = 0.10

      data vegsnowdepth_fixed/15.00, 12.00, 14.00,  8.00,  4.00,
     &                         0.50,  0.25,  1.00,  1.75,  0.65,  0.30,
     &                         0.15,  0.25,  0.15,  0.20,  0.10,  0.20,
     &                         0.01,  0.01,  0.01,  0.01,  0.40,  0.25,
     &                         0.01/

      do k=1,nvegtypes_fixed
        vegsnowdepth(k) = vegsnowdepth_fixed(k)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine met_data_check(undef,isingle_stn_flag,igrads_metfile,
     &  max_iter,i_tair_flag,i_rh_flag,i_wind_flag,i_prec_flag)

      implicit none

      include 'snowmodel.inc'

      integer iyr,imo,idy      ! year, month, and day of data
      real xhr                 ! decimal hour
      integer idstn            ! station id number

      integer k,nstns_orig,isingle_stn_flag,igrads_metfile,iter,
     &  n_good_Tair,n_good_rh,n_good_wspd,n_good_wdir,n_good_prec,
     &  n_notgood_vars,i_tair_flag,i_rh_flag,i_wind_flag,i_prec_flag,
     &  max_iter

      real Tair_orig(nstns_max),rh_orig(nstns_max)
      real winddir_orig(nstns_max),windspd_orig(nstns_max)
      double precision xstn_orig(nstns_max),ystn_orig(nstns_max)
      real elev_orig(nstns_max),prec_orig(nstns_max)
      real undef               ! undefined value
      real elevation_flag

      n_notgood_vars = 0
      elevation_flag = 0.0

      do iter=1,max_iter

        n_good_Tair = 0
        n_good_rh = 0
        n_good_wspd = 0
        n_good_wdir = 0
        n_good_prec = 0

        if (igrads_metfile.eq.1) then
          nstns_orig = 1
        else
          if (isingle_stn_flag.eq.1) then
            nstns_orig = 1
          else
            read(20,*) nstns_orig
          endif
        endif

        if (nstns_orig.gt.nstns_max) then
          print *, 'The number of met stations in your MicroMet'
          print *, 'input file exceeds nstns_max in snowmodel.inc.'
          print *, 'This occurs at iter =',iter
          stop
        endif

        do k=1,nstns_orig

          if (igrads_metfile.eq.1) then
            read(20,rec=iter) iyr,imo,idy,xhr,idstn,xstn_orig(k),
     &        ystn_orig(k),elev_orig(k),Tair_orig(k),rh_orig(k),
     &        windspd_orig(k),winddir_orig(k),prec_orig(k)
          else
            read(20,*) iyr,imo,idy,xhr,idstn,xstn_orig(k),
     &        ystn_orig(k),elev_orig(k),Tair_orig(k),rh_orig(k),
     &        windspd_orig(k),winddir_orig(k),prec_orig(k)
          endif

c Check for any NaN values.  They are not allowed.
          if (Tair_orig(k).ne.Tair_orig(k) .or.
     &      rh_orig(k).ne.rh_orig(k) .or.
     &      windspd_orig(k).ne.windspd_orig(k) .or.
     &      winddir_orig(k).ne.winddir_orig(k) .or.
     &      prec_orig(k).ne.prec_orig(k)) then
            print *
            print *,'  YOU HAVE NaN VALUES IN YOUR MET FORCING INPUT'
            print *,'  FILE.  THEY ARE NOT ALLOWED ANYWHERE IN THE'
            print *,'  MicroMet INPUT FILE.  THIS MUST BE CORRECTED'
            print *,'  BEFORE YOU CAN CONTINUE.'
            print *
            stop
          endif

c Count the good values at this time.
          if (Tair_orig(k).ne.undef) n_good_Tair = n_good_Tair + 1
          if (rh_orig(k).ne.undef) n_good_rh = n_good_rh + 1
          if (windspd_orig(k).ne.undef) n_good_wspd = n_good_wspd + 1
          if (winddir_orig(k).ne.undef) n_good_wdir = n_good_wdir + 1
          if (prec_orig(k).ne.undef) n_good_prec = n_good_prec + 1

          if (elev_orig(k).lt.0.0) then
            elevation_flag = 1.0
            print *,'elevation = ',elev_orig(k),'  for stn id = ',idstn
          endif

        enddo

c Check to see whether there are any variables with no valid data
c   at this time slice.
        if (n_good_Tair.eq.0 .and. i_tair_flag.eq.1) then
          n_notgood_vars = n_notgood_vars + 1
          print *,'no good Tair data at           ',iyr,imo,idy,xhr
        endif

        if (n_good_rh.eq.0 .and. i_rh_flag.eq.1) then
          n_notgood_vars = n_notgood_vars + 1
          print *,'no good rh data at             ',iyr,imo,idy,xhr
        endif

        if (n_good_wspd.eq.0 .and. i_wind_flag.eq.1) then
          n_notgood_vars = n_notgood_vars + 1
          print *,'no good wind speed data at     ',iyr,imo,idy,xhr
        endif

        if (n_good_wdir.eq.0 .and. i_wind_flag.eq.1) then
          n_notgood_vars = n_notgood_vars + 1
          print *,'no good wind direction data at ',iyr,imo,idy,xhr
        endif

        if (n_good_prec.eq.0 .and. i_prec_flag.eq.1) then
          n_notgood_vars = n_notgood_vars + 1
          print *,'no good precipitation data at  ',iyr,imo,idy,xhr
        endif

      enddo

      if (n_notgood_vars.gt.0) then
        print *
        print *,' FOUND TIMES WITH NO VALID MET OBSERVATIONS'
        print *,'NEED TO CORRECT THE PROBLEM BEFORE CONTINUING'
        stop
      endif

      if (elevation_flag.eq.1.0) then
        print *
        print *,' FOUND A NEGATIVE OR UNDEFINED STATION ELEVATION.'
        print *,'STATION ELEVATIONS CANNOT BE UNDEFINED, BUT THEY.'
        print *,'CAN BE NEGATIVE FOR A PLACE LIKE DEATH VALLEY.'
        print *,'YOU NEED TO CORRECT ANY PROBLEMS BEFORE CONTINUING.'
        stop
      endif

      if (igrads_metfile.eq.0) rewind (20)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_nearest_stns_1(nx,ny,xmn,ymn,deltax,deltay,
     &  n_stns_used,k_stn,snowmodel_line_flag,xg_line,yg_line)

      implicit none

      include 'snowmodel.inc'

      double precision xstn(nstns_max)
      double precision ystn(nstns_max)
      double precision dsq(nstns_max)
      double precision xg_line(nx_max,ny_max),yg_line(nx_max,ny_max)
      real snowmodel_line_flag

      double precision xg,yg,xmn,ymn,dist_min
      real deltax,deltay,x1,x2,x3,x4,x5,x6,x7

      integer i,j,k,kk,nstns,n_stns_used,nx,ny,i1,i2,i3,i4
      integer k_stn(nx_max,ny_max,9)

c Read the station information for the first (and all) time step(s).
      read(20,*) nstns
      do k=1,nstns
        read(20,*) i1,i2,i3,x1,i4,xstn(k),ystn(k),
     &    x2,x3,x4,x5,x6,x7
      enddo
      rewind (20)

      do j=1,ny
        do i=1,nx

c xcoords of grid nodes at index i,j
c ycoords of grid nodes at index i,j
          if (snowmodel_line_flag.eq.1.0) then
            xg = xg_line(i,j)
            yg = yg_line(i,j)
          else
            xg = xmn + deltax * (real(i) - 1.0)
            yg = ymn + deltay * (real(j) - 1.0)
          endif

c Loop through all of the stations, calculating the distance
c   between the current grid point and each of the stations.
          do k=1,nstns
            dsq(k) = (xg - xstn(k))**2 + (yg - ystn(k))**2
          enddo

c Loop through each of the station distances and find the
c   stations closest to the grid point in question.
          do kk=1,n_stns_used
            dist_min = 1.0e30
            do k=1,nstns
              if (dsq(k).le.dist_min) then
                k_stn(i,j,kk) = k
                dist_min = dsq(k)
              endif
            enddo

c Eliminate the last found minimum from the next search by making
c   its distance a big number.
            dsq(k_stn(i,j,kk)) = 1.0e30
          enddo

        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine HRESTART_SAVE(nx,ny,iter,snow_d,snow_depth,
     &  canopy_int,soft_snow_d,ro_snow_grid,swe_depth,
     &  ro_soft_snow_old,snow_d_init,swe_depth_old,
     &  canopy_int_old,topo,sum_sprec,icorr_factor_loop,
     &  max_iter)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,iter,icorr_factor_loop,max_iter
      real snow_d(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real canopy_int(nx_max,ny_max)
      real soft_snow_d(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real ro_soft_snow_old(nx_max,ny_max)
      real snow_d_init(nx_max,ny_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real topo(nx_max,ny_max)
      real sum_sprec(nx_max,ny_max)

      character*18 name1
      character*1 name2
      character*5 name3
      character*5 niter
      character*1 iloop
      character*30 fname

c Build the file name so it includes the interation number.
      name1 = 'hrestart/hrestart_'
      name2 = '_'
      name3 = '.gdat'

      write(niter,'(i5.5)') iter
      write(iloop,'(i1.1)') icorr_factor_loop
      fname = name1//niter//name2//iloop//name3

c Save the data.
      open(151,file=fname,
     &  form='unformatted',access='direct',recl=4*nx*ny)

      write (151,rec=1) ((snow_d(i,j),i=1,nx),j=1,ny)
      write (151,rec=2) ((snow_depth(i,j),i=1,nx),j=1,ny)
      write (151,rec=3) ((canopy_int(i,j),i=1,nx),j=1,ny)
      write (151,rec=4) ((soft_snow_d(i,j),i=1,nx),j=1,ny)
      write (151,rec=5) ((ro_snow_grid(i,j),i=1,nx),j=1,ny)
      write (151,rec=6) ((swe_depth(i,j),i=1,nx),j=1,ny)
      write (151,rec=7) ((ro_soft_snow_old(i,j),i=1,nx),j=1,ny)
      write (151,rec=8) ((snow_d_init(i,j),i=1,nx),j=1,ny)
      write (151,rec=9) ((swe_depth_old(i,j),i=1,nx),j=1,ny)
      write (151,rec=10) ((canopy_int_old(i,j),i=1,nx),j=1,ny)
      write (151,rec=11) ((topo(i,j),i=1,nx),j=1,ny)
      write (151,rec=12) ((sum_sprec(i,j),i=1,nx),j=1,ny)

      close (151)

c Save a copy that can be used as the initial condition for the
c   start of the second data assimilation loop.
      if (iter.eq.max_iter) then
        write(niter,'(i5.5)') 0
        write(iloop,'(i1.1)') 2
        fname = name1//niter//name2//iloop//name3

c Save the data.
        open(151,file=fname,
     &    form='unformatted',access='direct',recl=4*nx*ny)

        write (151,rec=1) ((snow_d(i,j),i=1,nx),j=1,ny)
        write (151,rec=2) ((snow_depth(i,j),i=1,nx),j=1,ny)
        write (151,rec=3) ((canopy_int(i,j),i=1,nx),j=1,ny)
        write (151,rec=4) ((soft_snow_d(i,j),i=1,nx),j=1,ny)
        write (151,rec=5) ((ro_snow_grid(i,j),i=1,nx),j=1,ny)
        write (151,rec=6) ((swe_depth(i,j),i=1,nx),j=1,ny)
        write (151,rec=7) ((ro_soft_snow_old(i,j),i=1,nx),j=1,ny)
        write (151,rec=8) ((snow_d_init(i,j),i=1,nx),j=1,ny)
        write (151,rec=9) ((swe_depth_old(i,j),i=1,nx),j=1,ny)
        write (151,rec=10) ((canopy_int_old(i,j),i=1,nx),j=1,ny)
        write (151,rec=11) ((topo(i,j),i=1,nx),j=1,ny)
        write (151,rec=12) ((sum_sprec(i,j),i=1,nx),j=1,ny)

        close (151)
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine HRESTART_SAVE_DA(nx,ny,max_iter,corr_factor,
     &  icorr_factor_index,nobs_dates)

      implicit none

      include 'snowmodel.inc'

      integer iobs_num,nobs_dates,nx,ny,i,j,max_iter,iter
      integer icorr_factor_index(max_time_steps)
      real corr_factor(nx_max,ny_max,max_obs_dates+1)


c Save the correction factors for each observation date.
      open(152,file='hrestart/hrestart_corr_factor.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      do iobs_num=1,nobs_dates+1
        write(152,rec=iobs_num)
     &    ((corr_factor(i,j,iobs_num),i=1,nx),j=1,ny)
      enddo

      close (152)

c Save the correction factor index.
      open(153,file='hrestart/hrestart_corr_factor_index.dat')

      do iter=1,max_iter
        write (153,*) iter,icorr_factor_index(iter)
      enddo

      close (153)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine HRESTART_READ(nx,ny,snow_d,snow_depth,
     &  canopy_int,soft_snow_d,ro_snow_grid,swe_depth,
     &  ro_soft_snow_old,snow_d_init,swe_depth_old,
     &  canopy_int_old,topo,sum_sprec,ihrestart_flag,
     &  i_dataassim_loop)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,ihrestart_flag,i_dataassim_loop,
     &  i_dataassim_loop_tmp
      real snow_d(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real canopy_int(nx_max,ny_max)
      real soft_snow_d(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real ro_soft_snow_old(nx_max,ny_max)
      real snow_d_init(nx_max,ny_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real topo(nx_max,ny_max)
      real sum_sprec(nx_max,ny_max)

      character*18 name1
      character*1 name2
      character*5 name3
      character*5 niter
      character*1 iloop
      character*30 fname

c Build the file name so it includes the interation number.
      name1 = 'hrestart/hrestart_'
      name2 = '_'
      name3 = '.gdat'

      if (i_dataassim_loop.lt.0.0) then
        i_dataassim_loop_tmp = 2
      else
        i_dataassim_loop_tmp = 1
      endif

      write(niter,'(i5.5)') ihrestart_flag
      write(iloop,'(i1.1)') i_dataassim_loop_tmp
      fname = name1//niter//name2//iloop//name3

c Save the data.
      open(152,file=fname,
     &  form='unformatted',access='direct',recl=4*nx*ny)

      read (152,rec=1) ((snow_d(i,j),i=1,nx),j=1,ny)
      read (152,rec=2) ((snow_depth(i,j),i=1,nx),j=1,ny)
      read (152,rec=3) ((canopy_int(i,j),i=1,nx),j=1,ny)
      read (152,rec=4) ((soft_snow_d(i,j),i=1,nx),j=1,ny)
      read (152,rec=5) ((ro_snow_grid(i,j),i=1,nx),j=1,ny)
      read (152,rec=6) ((swe_depth(i,j),i=1,nx),j=1,ny)
      read (152,rec=7) ((ro_soft_snow_old(i,j),i=1,nx),j=1,ny)
      read (152,rec=8) ((snow_d_init(i,j),i=1,nx),j=1,ny)
      read (152,rec=9) ((swe_depth_old(i,j),i=1,nx),j=1,ny)
      read (152,rec=10) ((canopy_int_old(i,j),i=1,nx),j=1,ny)
      read (152,rec=11) ((topo(i,j),i=1,nx),j=1,ny)
      read (152,rec=12) ((sum_sprec(i,j),i=1,nx),j=1,ny)

      close (152)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine HRESTART_READ_DA(nx,ny,max_iter,corr_factor,
     &  icorr_factor_index,i_dataassim_loop)

      implicit none

      include 'snowmodel.inc'

      integer iobs_num,nobs_dates,nx,ny,i,j,max_iter,iter,dummy,
     &  i_dataassim_loop
      integer icorr_factor_index(max_time_steps)
      real corr_factor(nx_max,ny_max,max_obs_dates+1)

c Read the correction factors for each observation date.
      open(152,file='hrestart/hrestart_corr_factor.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      if (i_dataassim_loop.lt.0.0) nobs_dates = - i_dataassim_loop

      do iobs_num=1,nobs_dates+1
        read(152,rec=iobs_num)
     &    ((corr_factor(i,j,iobs_num),i=1,nx),j=1,ny)
      enddo

      close (152)

c Read the correction factor index.
      open(153,file='hrestart/hrestart_corr_factor_index.dat')

      do iter=1,max_iter
        read (153,*) dummy,icorr_factor_index(iter)
      enddo

      close (153)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_ctl_files(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  print_inc,iyear_init,imonth_init,iday_init,xhour_init,
     &  max_iter,undef,output_path_wo_assim,output_path_wi_assim,
     &  print_micromet,micromet_output_fname,print_enbal,
     &  enbal_output_fname,print_snowpack,snowpack_output_fname,
     &  print_snowtran,snowtran_output_fname,Tabler_1_flag,
     &  tabler_sfc_path_name,Tabler_2_flag,irun_data_assim,
     &  print_var,print_outvars,print_multilayer,
     &  multilayer_output_fname)

      implicit none

      include 'snowmodel.inc'

      real deltax,deltay,dt,print_inc,xhour_init,print_micromet,
     &  print_enbal,print_snowpack,print_snowtran,Tabler_1_flag,
     &  Tabler_2_flag,undef,print_multilayer

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  irun_data_assim,k,kk

      double precision xmn,ymn

      character*1 print_var(n_print_vars)

      character*80 micromet_output_fname
      character*80 enbal_output_fname
      character*80 snowpack_output_fname
      character*80 snowtran_output_fname
      character*80 multilayer_output_fname
      character*80 tabler_sfc_path_name
      character*80 output_path
      character*80 output_path_wo_assim,output_path_wi_assim

      character*4 print_outvars(n_print_vars)
      character*80 description(n_print_vars)

c This is done below to avoid some array definition conflict (you
c   cannot use the "data" statement like this when the array is
c   being passed in and out of differet subroutines.
c     data print_outvars /'tair','relh','wspd','qsin','qlin',
c    &                    'qlem','albd','wdir','prec','rpre',
c    &                    'spre','smlt','ssub','roff','glmt',
c    &                    'snod','sden','swed','sspr','ssmt',
c    &                    'cldf','var1','var2','var3','var4',
c    &                    'var5','var6','var7','var8','var9'/

      data description /
     &  'tair  0  0 air temperature (deg C)',
     &  'relh  0  0 relative humidity (%)',
     &  'wspd  0  0 wind speed (m/s)',
     &  'qsin  0  0 incoming solar rad at the surface (W/m2)',
     &  'qlin  0  0 incoming longwave rad at the surface (W/m2)',

     &  'qlem  0  0 emitted longwave radiation (W/m2)',
     &  'albd  0  0 albedo (0-1)',
     &  'wdir  0  0 wind direction (0-360, true N)',
     &  'prec  0  0 water-equivalent precipitation (m/time_step)',
     &  'rpre  0  0 liquid precipitation, rain (m/time_step)',

     &  'spre  0  0 solid precipitation, snowfall (m/time_step)',
     &  'smlt  0  0 snow-water-equivalent melt (m)',
     &  'ssub  0  0 static-surface sublimation (m)',
     &  'roff  0  0 runoff from snowpack base (m/time_step)',
     &  'glmt  0  0 snow-water-equivalent melt from glacier ice (m)',

     &  'snod  0  0 snow depth (m)',
     &  'sden  0  0 snow density (kg/m3)',
     &  'swed  0  0 snow-water-equivalent depth (m)',
     &  'sspr  0  0 summed snow precip during year (m)',
     &  'ssmt  0  0 summed snow-water-equivalent melt (m)',

     &  'cldf  0  0 cloud fraction (0-1)',
     &  'var1  0  0 to be defined in future applications',
     &  'var2  0  0 to be defined in future applications',
     &  'var3  0  0 to be defined in future applications',
     &  'var4  0  0 to be defined in future applications',

     &  'var5  0  0 to be defined in future applications',
     &  'var6  0  0 to be defined in future applications',
     &  'var7  0  0 to be defined in future applications',
     &  'var8  0  0 to be defined in future applications',
     &  'var9  0  0 to be defined in future applications'/

      character*14 den_outvars(3)
      character*80 den_description(3)

      data den_outvars /
     &  'dden_den_assim','sden_den_assim','snod_den_assim'/

      data den_description /
     &  'dden  0  0 assimilated snow density difference (kg/m^3)',
     &  'sden  0  0 snow density after the assimilation (kg/m^3)',
     &  'snod  0  0 snow depth with assimilated snow density (m)'/

      print_outvars(1) = 'tair'
      print_outvars(2) = 'relh'
      print_outvars(3) = 'wspd'
      print_outvars(4) = 'qsin'
      print_outvars(5) = 'qlin'
      print_outvars(6) = 'qlem'
      print_outvars(7) = 'albd'
      print_outvars(8) = 'wdir'
      print_outvars(9) = 'prec'
      print_outvars(10) = 'rpre'
      print_outvars(11) = 'spre'
      print_outvars(12) = 'smlt'
      print_outvars(13) = 'ssub'
      print_outvars(14) = 'roff'
      print_outvars(15) = 'glmt'
      print_outvars(16) = 'snod'
      print_outvars(17) = 'sden'
      print_outvars(18) = 'swed'
      print_outvars(19) = 'sspr'
      print_outvars(20) = 'ssmt'
      print_outvars(21) = 'cldf'
      print_outvars(22) = 'var1'
      print_outvars(23) = 'var2'
      print_outvars(24) = 'var3'
      print_outvars(25) = 'var4'
      print_outvars(26) = 'var5'
      print_outvars(27) = 'var6'
      print_outvars(28) = 'var7'
      print_outvars(29) = 'var8'
      print_outvars(30) = 'var9'

c Create the GrADS .ctl (control) files to go with the GrADS
c   .gdat output files that were generated by this model run.
      if (print_micromet.eq.1.0) then
        call mk_micromet_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &    iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &    undef,micromet_output_fname)
      endif

      if (print_enbal.eq.1.0) then
        call mk_enbal_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &    iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &    undef,enbal_output_fname)
      endif

      if (print_snowpack.eq.1.0) then
        call mk_snowpack_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &    iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &    undef,snowpack_output_fname)
      endif

      if (print_snowtran.eq.1.0) then
        call mk_snowtran_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &    iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &    undef,snowtran_output_fname)
      endif

      if (Tabler_1_flag.eq.1.0) then
        call mk_tabler_1_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &    undef,tabler_sfc_path_name)
      endif

      if (print_multilayer.eq.1.0) then
        call mk_multilayer_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &    iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &    undef,multilayer_output_fname)
      endif

c If you are not doing data assimilation, then you just need .ctl
c   files for the wo_assim directory.  If you are also doing the
c   data assimilation, then you also need .ctl files for the .gdat
c   data files that are in the wi_assim directory.
      do k=1,irun_data_assim+1
        if (k.eq.1) then
          output_path = output_path_wo_assim
        elseif (k.eq.2) then
          output_path = output_path_wi_assim
        endif

        if (Tabler_2_flag.eq.1.0) then
          call mk_tabler_2_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &      iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k,print_inc)
        endif

        do kk=1,n_print_vars
          if (print_var(kk).eq.'y') then
            call mk_4char_vars_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &        iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &        undef,output_path,k,print_outvars(kk),description(kk),
     &        print_inc)
          endif
        enddo

        if (print_multilayer.eq.2.0) then
          call mk_multilayer_2Dxy_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k)

          call mk_multilayer_snod_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k)
          call mk_multilayer_sden_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k)
          call mk_multilayer_swed_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k)
          call mk_multilayer_diam_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k)
          call mk_multilayer_flux_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k)
          call mk_multilayer_temp_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k)
          call mk_multilayer_cond_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &      undef,output_path,k)
        endif

      enddo

c     if (irun_density_assim.eq.1) then
c       call mk_density_assim_sfc_ctl(nx,ny,deltax,deltay,xmn,ymn,
c    &    undef,den_outvars(1),den_description(1))
c       do kk=2,3
c         call mk_density_assim_ctl(nx,ny,deltax,deltay,xmn,ymn,
c    &      dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
c    &      undef,output_path_wi_assim,print_inc,den_outvars(kk),
c    &      den_description(kk))
c       enddo
c     endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_density_assim_sfc_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  undef,den_outvars,den_description)

      implicit none

      integer nx,ny,igrads_dt
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,undef

      integer len_name,len_path,len_desc,trailing_blanks

      character*80 output_fname,filename,den_description
      character*80 output_path_tmp
      character*14 den_outvars
      character*3 cmo
      character*2 cdt

      data cmo /'jan'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

      igrads_dt = 1
      cdt = 'yr'

      filename = 'data/'//den_outvars//'.ctl'

      output_path_tmp = '^'

      len_path = 80 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//den_outvars//'.gdat'
      len_name = 80 - trailing_blanks(output_fname)
      len_desc = 80 - trailing_blanks(den_description)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) 1,12,1,cmo,9999,igrads_dt,cdt
      write (71,58)
      write (71,59) den_description(1:len_desc)
      write (71,60)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE SnowModel Density Assimilation correction file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS     1')
   59 format (a)
   60 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_density_assim_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,print_inc,den_outvars,
     &  den_description)

      implicit none

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef,
     &  print_inc

      integer len_name,len_path,len_desc,trailing_blanks

      character*105 output_fname
      character*80 filename,den_description
      character*80 output_path
      character*86 output_path_tmp
      character*14 den_outvars
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert the write interval from seconds to hours or a day.
      if (dt*print_inc.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt*print_inc.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt*print_inc.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'This mk_ctl program has not been set up to deal'
        print *, 'with this combination of dt and print_inc values:'
        print *, 'dt =',dt,'   print_inc =',print_inc
        stop
      endif

      filename = 'ctl_files/wi_assim/'//den_outvars//'.ctl'

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//den_outvars//'.gdat'
      len_name = 105 - trailing_blanks(output_fname)
      len_desc = 80 - trailing_blanks(den_description)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59) den_description(1:len_desc)
      write (71,60)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE SnowModel Density Assimilation output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS     1')
   59 format (a)
   60 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_4char_vars_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k,print_outvars,description,
     &  print_inc)

      implicit none

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef,
     &  print_inc

      integer len_name,len_path,len_desc,trailing_blanks

      character*95 output_fname
      character*80 filename,description
      character*80 output_path
      character*86 output_path_tmp
      character*4 print_outvars
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert the write interval from seconds to hours or a day.
      if (dt*print_inc.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt*print_inc.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt*print_inc.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'This mk_ctl program has not been set up to deal'
        print *, 'with this combination of dt and print_inc values:'
        print *, 'dt =',dt,'   print_inc =',print_inc
        stop
      endif

      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/'//print_outvars//'.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/'//print_outvars//'.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//print_outvars//'.gdat'
      len_name = 95 - trailing_blanks(output_fname)
      len_desc = 80 - trailing_blanks(description)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) int(real(max_iter)/print_inc),nint(xhour_init),
     &  iday_init,cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59) description(1:len_desc)
      write (71,60)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE SnowModel single-variable output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS     1')
   59 format (a)
   60 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_micromet_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_fname)

      implicit none

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*80 output_fname,filename
      character*83 output_fname_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

      filename = 'ctl_files/micromet.ctl'

c Deal with the case with relative paths.
      if (output_fname(1:1).ne.'/') then
        output_fname_tmp = '../'//output_fname
      else
        output_fname_tmp = output_fname//'   '
      endif

      len_name = 83 - trailing_blanks(output_fname_tmp)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname_tmp(1:len_name)
      else 
        write (71,51) output_fname_tmp(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59)
      write (71,60)
      write (71,61)
      write (71,62)
      write (71,63)
      write (71,64)
      write (71,65)
      write (71,66)
      write (71,67)
      write (71,68)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE MicroMet output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS     9')
   59 format ('tair  0  0 air temperature (deg C)')
   60 format ('relh  0  0 relative humidity (%)')
   61 format ('uwnd  0  0 meridional wind component (m/s)')
   62 format ('vwnd  0  0 zonal wind component (m/s)')
   63 format ('wspd  0  0 wind speed (m/s)')
   64 format ('wdir  0  0 wind direction (0-360, true N)')
   65 format ('qsin  0  0 incoming solar rad at the surface (W/m2)')
   66 format ('qlin  0  0 incoming longwave rad at the surface (W/m2)')
   67 format ('prec  0  0 precipitation (m/time_step)')
   68 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_enbal_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_fname)

      implicit none

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*80 output_fname,filename
      character*83 output_fname_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

      filename = 'ctl_files/enbal.ctl'

c Deal with the case with relative paths.
      if (output_fname(1:1).ne.'/') then
        output_fname_tmp = '../'//output_fname
      else
        output_fname_tmp = output_fname//'   '
      endif

      len_name = 83 - trailing_blanks(output_fname_tmp)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname_tmp(1:len_name)
      else 
        write (71,51) output_fname_tmp(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59)
      write (71,60)
      write (71,61)
      write (71,62)
      write (71,63)
      write (71,64)
      write (71,65)
      write (71,66)
      write (71,67)
      write (71,68)
      write (71,69)
      write (71,70)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE EnBal output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    11')
   59 format ('tair  0  0 air temperature (deg C)')
   60 format ('tsfc  0  0 surface (skin) temperature (deg C)')
   61 format ('qsin  0  0 incoming solar rad at the surface (W/m2)')
   62 format ('qlin  0  0 incoming longwave rad at the surface (W/m2)')
   63 format ('qlem  0  0 emitted longwave radiation (W/m2)')
   64 format ('qh    0  0 sensible heat flux (W/m2)')
   65 format ('qe    0  0 latent heat flux (W/m2)')
   66 format ('qc    0  0 conductive heat flux (W/m2)')
   67 format ('qm    0  0 melt energy flux (W/m2)')
   68 format ('albd  0  0 albedo (0-1)')
   69 format ('ebal  0  0 energy balance error (W/m2)')
   70 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_snowpack_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_fname)

      implicit none

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*80 output_fname,filename
      character*83 output_fname_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

      filename = 'ctl_files/snowpack.ctl'

c Deal with the case with relative paths.
      if (output_fname(1:1).ne.'/') then
        output_fname_tmp = '../'//output_fname
      else
        output_fname_tmp = output_fname//'   '
      endif

      len_name = 83 - trailing_blanks(output_fname_tmp)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname_tmp(1:len_name)
      else 
        write (71,51) output_fname_tmp(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59)
      write (71,60)
      write (71,61)
      write (71,62)
      write (71,63)
      write (71,64)
      write (71,65)
      write (71,66)
      write (71,67)
      write (71,68)
      write (71,69)
      write (71,70)
      write (71,71)
      write (71,72)
      write (71,73)
      write (71,74)
      write (71,75)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE SnowPack output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    16')
   59 format ('snod       0  0 snow depth (m)')
   60 format ('sden       0  0 snow density (kg/m3)')
   61 format ('swed       0  0 snow-water-equivalent depth (m)')
   62 format ('roff       0  0 runoff from snowpack base (m/time_step)')
   63 format ('rain       0  0 liquid precipitation (m/time_step)')
   64 format ('spre       0  0 solid precipitation (m/time_step)')
   65 format ('qcs        0  0 canopy sublimation (m/time_step)')
   66 format ('canopy     0  0 canopy interception store (m)')
   67 format ('sumqcs     0  0 summed canopy sublim during year (m)')
   68 format ('sumprec    0  0 summed precipitation during year (m)')
   69 format ('sumsprec   0  0 summed snow precip during year (m)')
   70 format ('sumunload  0  0 summed canopy unloading during year (m)')
   71 format ('sumroff    0  0 summed runoff during the year (m)')
   72 format ('sumswemelt 0  0 summed snow-water-equivalent melt (m)')
   73 format ('sumsublim  0  0 summed static-surface sublimation (m)')
   74 format ('wbal       0  0 summed water bal error during year (m)')
   75 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_snowtran_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_fname)

      implicit none

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*80 output_fname,filename
      character*83 output_fname_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

      filename = 'ctl_files/snowtran.ctl'

c Deal with the case with relative paths.
      if (output_fname(1:1).ne.'/') then
        output_fname_tmp = '../'//output_fname
      else
        output_fname_tmp = output_fname//'   '
      endif

      len_name = 83 - trailing_blanks(output_fname_tmp)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname_tmp(1:len_name)
      else 
        write (71,51) output_fname_tmp(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59)
      write (71,60)
      write (71,61)
      write (71,62)
      write (71,63)
      write (71,64)
      write (71,65)
      write (71,66)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE SnowTran output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS     7')
   59 format ('snod    0  0 snow depth (m)')
   60 format ('subl    0  0 sublimation at this time step (m)')
   61 format ('salt    0  0 saltation transport at this time step (m)')
   62 format ('susp    0  0 suspended transport at this time step (m)')
   63 format ('subgrid 0  0 tabler snow redist at this time step (m)')
   64 format ('sumsubl 0  0 summed sublimation during the year (m)')
   65 format ('sumtran 0  0 summed blowing-snow transport for year (m)')
   66 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_tabler_1_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  undef,output_path)

      implicit none

      integer nx,ny,igrads_dt
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,undef

      integer len_name,len_path,trailing_blanks

      character*99 output_fname
      character*80 filename
      character*80 output_path
      character*83 output_path_tmp
      character*3 cmo
      character*2 cdt

      data cmo /'jan'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

      igrads_dt = 1
      cdt = 'yr'

      filename = 'ctl_files/tabler_sfcs.ctl'

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../'//output_path
      else
        output_path_tmp = output_path//'   '
      endif

      len_path = 83 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'tabler_sfcs.gdat'
      len_name = 99 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) 1,12,1,cmo,9999,igrads_dt,cdt
      write (71,58)
      write (71,59)
      write (71,60)
      write (71,61)
      write (71,62)
      write (71,63)
      write (71,64)
      write (71,65)
      write (71,66)
      write (71,67)
      write (71,68)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Tabler Equilibrium Surfaces for snow-free land')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS     9')
   59 format ('nn    0  0  tabler surface from north winds (m)')
   60 format ('ne    0  0  tabler surface from northeast winds (m)')
   61 format ('ee    0  0  tabler surface from east winds (m)')
   62 format ('se    0  0  tabler surface from southeast winds (m)')
   63 format ('ss    0  0  tabler surface from south winds (m)')
   64 format ('sw    0  0  tabler surface from southwest winds (m)')
   65 format ('ww    0  0  tabler surface from west winds (m)')
   66 format ('nw    0  0  tabler surface from northwest winds (m)')
   67 format ('topo  0  0  topography (m)')
   68 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_tabler_2_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k,print_inc)

      implicit none

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef,
     &  print_inc

      integer len_name,len_path,trailing_blanks

      character*97 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert the write interval from seconds to hours or a day.
      if (dt*print_inc.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt*print_inc.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt*print_inc.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'This mk_ctl program has not been set up to deal'
        print *, 'with this combination of dt and print_inc values:'
        print *, 'dt =',dt,'   print_inc =',print_inc
        stop
      endif

      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/tabler_sfc_iter.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/tabler_sfc_iter.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname=output_path_tmp(1:len_path)//'tabler_sfcs_iter.gdat'
      len_name = 97 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif


      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56)
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59)
      write (71,60)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Tabler Equilibrium Surface output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF         1 LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS     1')
   59 format ('tablersfc  0  0 Tabler equilibrium surface (m)')
   60 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_ctl(nx,ny,deltax,deltay,xmn,ymn,dt,
     &  iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_fname)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*80 output_fname,filename
      character*83 output_fname_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

      filename = 'ctl_files/multilayer.ctl'

c Deal with the case with relative paths.
      if (output_fname(1:1).ne.'/') then
        output_fname_tmp = '../'//output_fname
      else
        output_fname_tmp = output_fname//'   '
      endif

      len_name = 83 - trailing_blanks(output_fname_tmp)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname_tmp(1:len_name)
      else 
        write (71,51) output_fname_tmp(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59)
      write (71,60)
      write (71,61)
      write (71,62)
      write (71,63) nz_max
      write (71,64) nz_max
      write (71,65) nz_max
      write (71,66) nz_max
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    8')
   59 format ('KK         1  0 number of snow layers')
   60 format ('snod       1  0 snow depth (m)')
   61 format ('sden       1  0 snow density (kg/m3)')
   62 format ('swed       1  0 snow-water-equivalent depth (m)')
   63 format ('snodz ',i6,'  0 layer-specific snow depth (m)')
   64 format ('sdenz ',i6,'  0 layer-specific snow density (kg/m3)')
   65 format ('swedz ',i6,'  0 layer-specific swe depth (m)')
   66 format ('diamz ',i6,'  0 layer-specific grain diameter (m)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_2Dxy_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,len_path,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*106 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

c Define the name and location of the .ctl file.
      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/multilayer_2Dxy.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/multilayer_2Dxy.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'multilayer_2Dxy.gdat'
      len_name = 106 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,59)
      write (71,60)
      write (71,61)
      write (71,62)
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    4')
   59 format ('KK         1  0 number of snow layers')
   60 format ('snod       1  0 snow depth (m)')
   61 format ('sden       1  0 snow density (kg/m3)')
   62 format ('swed       1  0 snow-water-equivalent depth (m)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_snod_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,len_path,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*106 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

c Define the name and location of the .ctl file.
      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/multilayer_snod.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/multilayer_snod.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'multilayer_snod.gdat'
      len_name = 106 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,63) nz_max
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    1')
   63 format ('snodz ',i6,'  0 layer-specific snow depth (m)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_sden_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,len_path,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*106 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

c Define the name and location of the .ctl file.
      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/multilayer_sden.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/multilayer_sden.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'multilayer_sden.gdat'
      len_name = 106 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,64) nz_max
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    1')
   64 format ('sdenz ',i6,'  0 layer-specific snow density (kg/m3)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_swed_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,len_path,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*106 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

c Define the name and location of the .ctl file.
      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/multilayer_swed.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/multilayer_swed.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'multilayer_swed.gdat'
      len_name = 106 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,65) nz_max
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    1')
   65 format ('swedz ',i6,'  0 layer-specific swe depth (m)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_diam_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,len_path,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*106 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

c Define the name and location of the .ctl file.
      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/multilayer_diam.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/multilayer_diam.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'multilayer_diam.gdat'
      len_name = 106 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,66) nz_max
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    1')
   66 format ('diamz ',i6,'  0 layer-specific grain diameter (m)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_flux_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,len_path,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*106 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

c Define the name and location of the .ctl file.
      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/multilayer_flux.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/multilayer_flux.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'multilayer_flux.gdat'
      len_name = 106 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,66) nz_max
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    1')
   66 format ('fluxz ',i6,'  0 layer-specific vapor flux (kg m-2 dt-1)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_temp_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,len_path,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*106 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

c Define the name and location of the .ctl file.
      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/multilayer_temp.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/multilayer_temp.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'multilayer_temp.gdat'
      len_name = 106 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,66) nz_max
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    1')
   66 format ('tempz ',i6,'  0 layer-specific snow temperature (deg K)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_multilayer_cond_ctl(nx,ny,deltax,deltay,xmn,ymn,
     &  dt,iyear_init,imonth_init,iday_init,xhour_init,max_iter,
     &  undef,output_path,k)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iyear_init,imonth_init,iday_init,max_iter,
     &  igrads_dt,len_path,k
      double precision xmn,ymn,xmn_km,ymn_km
      real deltax,deltay,deltax_km,deltay_km,xhour_init,dt,undef

      integer len_name,trailing_blanks

      character*106 output_fname
      character*80 filename
      character*80 output_path
      character*86 output_path_tmp
      character*3 cmo(12)
      character*2 cdt

      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

      deltax_km = deltax / 1000.0
      deltay_km = deltay / 1000.0
      xmn_km = xmn / 1000.0
      ymn_km = ymn / 1000.0

c Convert dt from seconds to hours or a day.
      if (dt.eq.86400.0) then
        igrads_dt = 1
        cdt = 'dy'
      elseif (dt.eq.10800.0) then
        igrads_dt = 3
        cdt = 'hr'
      elseif (dt.eq.3600.0) then
        igrads_dt = 1
        cdt = 'hr'
      else
        print *, 'the mk_ctl program cannot deal with this dt value'
        stop
      endif

c Define the name and location of the .ctl file.
      if (k.eq.1) then
        filename = 'ctl_files/wo_assim/multilayer_cond.ctl'
      elseif (k.eq.2) then
        filename = 'ctl_files/wi_assim/multilayer_cond.ctl'
      endif

c Deal with the case with relative paths.
      if (output_path(1:1).ne.'/') then
        output_path_tmp = '../../'//output_path
      else
        output_path_tmp = output_path//'      '
      endif

      len_path = 86 - trailing_blanks(output_path_tmp)
      output_fname = output_path_tmp(1:len_path)//'multilayer_cond.gdat'
      len_name = 106 - trailing_blanks(output_fname)

      open (71,file=filename)

      if (output_fname(1:1).eq.'/') then 
        write (71,50) output_fname(1:len_name)
      else 
        write (71,51) output_fname(1:len_name)
      endif

      write (71,52)
      write (71,53) undef
c (i,j) indexing.
      write (71,54) nx,1.0,1.0
      write (71,55) ny,1.0,1.0
c (meters,meters) indexing, with (zero,zero) origin.
      write (71,541) nx,0.0,deltax
      write (71,551) ny,0.0,deltay
c (km,km) indexing, with (zero,zero) origin.
      write (71,542) nx,0.0,deltax_km
      write (71,552) ny,0.0,deltay_km
c (meters,meters) indexing, with (xmn,ymn) origin.
      write (71,543) nx,xmn,deltax
      write (71,553) ny,ymn,deltay
c (km,km) indexing, with (xmn,ymn) origin.
      write (71,544) nx,xmn_km,deltax_km
      write (71,554) ny,ymn_km,deltay_km

      write (71,56) nz_max
      write (71,57) max_iter,nint(xhour_init),iday_init,
     &  cmo(imonth_init),iyear_init,igrads_dt,cdt
      write (71,58)
      write (71,66) nz_max
      write (71,67)

      close (71)

c This "a" by itself clips the trailing blanks in the a80 string.
   50 format ('DSET ',a)
   51 format ('DSET ^',a)
   52 format ('TITLE Multi-Layer output file')
   53 format ('UNDEF ',f10.1)

   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
  541 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  551 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  542 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  552 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  543 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  553 format ('#YDEF ',i8,' LINEAR ',2f20.8)
  544 format ('#XDEF ',i8,' LINEAR ',2f20.8)
  554 format ('#YDEF ',i8,' LINEAR ',2f20.8)

   56 format ('ZDEF    ',i6,' LINEAR 1 1')
c This i2.2 puts a zero in front of single digit numbers like 1.
   57 format ('TDEF ',i8,' LINEAR ',i2.2,'Z',i2.2,a3,i4,' ',i2,a2)
   58 format ('VARS    1')
   66 format ('condz ',i6,'  0 layer thermal conductivity (W m-1 K-1)')
   67 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c readparam_code.f

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine READPARAM_CODE(dt,deltax,deltay,Utau_t_flag,
     &  subgrid_flag,twolayer_flag,snowmodel_dot_par_fname,
     &  bc_flag,curve_len_scale,slopewt,curvewt,ht_windobs,
     &  ht_rhobs,ro_snow,snow_d_init_const,const_veg_flag,
     &  vegsnowdepth,nx,ny,max_iter,met_input_fname,xmn,ymn,
     &  iyear_init,imonth_init,iday_init,xhour_init,undef,ifill,
     &  iobsint,dn,xlat,i_tair_flag,i_rh_flag,i_wind_flag,
     &  i_solar_flag,i_prec_flag,isingle_stn_flag,igrads_metfile,
     &  windspd_min,icond_flag,run_micromet,run_enbal,run_snowpack,
     &  run_snowtran,topoflag,topoveg_fname,snowtran_output_fname,
     &  micromet_output_fname,enbal_output_fname,Utau_t_const,
     &  snowpack_output_fname,print_micromet,print_enbal,
     &  print_snowpack,print_snowtran,i_longwave_flag,print_user,
     &  ascii_topoveg,topo_ascii_fname,veg_ascii_fname,
     &  irun_data_assim,lapse_rate_user_flag,
     &  iprecip_lapse_rate_user_flag,use_shortwave_obs,
     &  use_longwave_obs,use_sfc_pressure_obs,calc_subcanopy_met,
     &  sfc_sublim_flag,gap_frac,cloud_frac_factor,
     &  albedo_snow_forest,albedo_snow_clearing,albedo_glacier,
     &  barnes_lg_domain,n_stns_used,tabler_dir,slope_adjust,
     &  lat_solar_flag,UTC_flag,iveg_ht_flag,ihrestart_flag,
     &  ihrestart_inc,i_dataassim_loop,tsls_threshold,dz_snow_min,
     &  print_multilayer,multilayer_snowpack,max_layers,
     &  multilayer_output_fname,izero_snow_date,curve_lg_scale_flag,
     &  check_met_data,seaice_run,snowmodel_line_flag,wind_lapse_rate,
     &  iprecip_scheme,cf_precip_flag,snowfall_frac,print_inc,
     &  output_path_wo_assim,output_path_wi_assim,Tabler_1_flag,
     &  Tabler_2_flag,tabler_sfc_path_name,print_var)

c These programs read and process the input parameter data.
c
c The following must be true:
c
c   All comment lines start with a ! in the first position.
c
c   Blank lines are permitted.
c
c   All parameter statements start with the parameter name, followed
c   by a space, followed by an =, followed by a space, followed by
c   the actual value, with nothing after that.  These statements can
c   have leading blanks, and must fit within 120 columns, and the
c   parameter value cannot be longer than 80 characters (so, for
c   example, your paths/file_names to the right of the "=" sign
c   cannot be longer than 80 characters).
c
c   It is set up to deal with integer, real, and string (character)
c   input values.

      implicit none

      include 'snowmodel.inc'

c Put parameter names here:
      real dt,deltax,deltay,Utau_t_flag,topoflag,Utau_t_const,
     &  subgrid_flag,twolayer_flag,
     &  bc_flag,curve_len_scale,slopewt,curvewt,ht_windobs,
     &  ht_rhobs,ro_snow,snow_d_init_const,const_veg_flag,
     &  windspd_min,ascii_topoveg,gap_frac,cloud_frac_factor,
     &  albedo_snow_forest,albedo_snow_clearing,albedo_glacier,
     &  barnes_lg_domain,tabler_dir,slope_adjust,UTC_flag,
     &  tsls_threshold,dz_snow_min,print_multilayer,
     &  curve_lg_scale_flag,check_met_data,seaice_run,
     &  snowmodel_line_flag,wind_lapse_rate,cf_precip_flag,
     &  snowfall_frac,Tabler_1_flag,Tabler_2_flag

      character*1 print_var(n_print_vars)

      character*1 print_var_01,print_var_02,print_var_03,
     &  print_var_04,print_var_05,print_var_06,print_var_07,
     &  print_var_08,print_var_09,print_var_10,print_var_11,
     &  print_var_12,print_var_13,print_var_14,print_var_15,
     &  print_var_16,print_var_17,print_var_18,print_var_19,
     &  print_var_20,print_var_21,print_var_22,print_var_23,
     &  print_var_24,print_var_25,print_var_26,print_var_27,
     &  print_var_28,print_var_29,print_var_30

      real vegsnowdepth(nvegtypes)
      real run_micromet,run_enbal,run_snowpack,run_snowtran,
     &  print_micromet,print_enbal,print_snowpack,print_snowtran,
     &  print_user,use_shortwave_obs,use_longwave_obs,print_inc,
     &  use_sfc_pressure_obs,calc_subcanopy_met,sfc_sublim_flag

      integer nx,ny,max_iter,icond_flag,irun_data_assim

      character*80 topoveg_fname,met_input_fname,topo_ascii_fname,
     &  veg_ascii_fname

      character*80 snowtran_output_fname,micromet_output_fname,
     &  enbal_output_fname,snowpack_output_fname,multilayer_output_fname

      character*80 output_path_wo_assim,output_path_wi_assim,
     &  tabler_sfc_path_name

      integer iyear_init,imonth_init,iday_init,
     &  i_tair_flag,i_rh_flag,i_wind_flag,i_solar_flag,i_prec_flag,
     &  i_longwave_flag,isingle_stn_flag,igrads_metfile,
     &  lapse_rate_user_flag,iprecip_lapse_rate_user_flag,
     &  n_stns_used,iveg_ht_flag,lat_solar_flag,ihrestart_inc,
     &  ihrestart_flag,i_dataassim_loop,multilayer_snowpack,max_layers,
     &  izero_snow_date,iprecip_scheme

      double precision xmn,ymn
      real xhour_init,dn
      real undef               ! undefined value
      integer ifill    ! flag (=1) forces a value in every cell
      integer iobsint  ! flag (=1) use dn value from .par file
      real xlat      ! approx. latitude of domain center, decimal deg

      character*100 snowmodel_dot_par_fname

c Working parameters.
      character*120 input_string
      character*80 c_param
      character*80 c_value
      integer k,max_par_lines,i_param_chars,i_value_chars,
     &  icomment_flag,npars,ipar_flag

      parameter (npars=139)
      integer ipar_count
      character*40 cpar_name(npars)

      max_par_lines = 1500

c     open (40,file='snowmodel.par')
      open (40,file=snowmodel_dot_par_fname)

c Initialize the input-parameter counting array.  This is used to
c   make sure all of the input parameters are read in correctly.
c   Also initialize a array that will collect the variable names
c   that have been read in, so it can be compared against the .par
c   file to help figure out which variables have not been defined
c   before the model simulation starts.
      ipar_count = 0
      do k=1,npars
        cpar_name(k) = '  BLANK: SOMETHING IS NOT DEFINED'
      enddo

      do k=1,max_par_lines
        read (40,'(a120)',end=99) input_string

        call get_param_data(input_string,c_param,c_value,
     &    i_param_chars,i_value_chars,icomment_flag)

c Process the string if it is not a comment.
        if (icomment_flag.eq.0) then

c GENERAL MODEL SETUP.
          if (c_param(1:i_param_chars).eq.'nx') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(nx,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (nx.le.0) then
              print *,'nx cannot be less than or equal to 0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'ny') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(ny,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (ny.le.0) then
              print *,'ny cannot be less than or equal to 0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'deltax') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(deltax,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (deltax.le.0.0) then
              print *,'deltax cannot be less than or equal to 0.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'deltay') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(deltay,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (deltay.le.0.0) then
              print *,'deltay cannot be less than or equal to 0.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'xmn') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2double(xmn,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'ymn') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2double(ymn,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'dt') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(dt,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (dt.le.0.0 .or. dt.gt.86400.0) then
              print *,'dt out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'iyear_init') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(iyear_init,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (iyear_init.lt.1900 .or. iyear_init.gt.2100) then
              print *,'iyear_init out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'imonth_init') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(imonth_init,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (imonth_init.lt.1 .or. imonth_init.gt.12) then
              print *,'imonth_init out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'iday_init') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(iday_init,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (iday_init.lt.1 .or. iday_init.gt.31) then
              print *,'iday_init out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'xhour_init') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(xhour_init,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (xhour_init.lt.0.0 .or. xhour_init.ge.24.0) then
              print *,'xhour_init out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'max_iter') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(max_iter,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (max_iter.le.0 .or. max_iter.gt.300000) then
              print *,'max_iter out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'isingle_stn_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(isingle_stn_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (isingle_stn_flag.ne.0 .and. isingle_stn_flag.ne.1) then
              print *,'isingle_stn_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'igrads_metfile') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(igrads_metfile,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (igrads_metfile.ne.0 .and. igrads_metfile.ne.1) then
              print *,'igrads_metfile not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'met_input_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(met_input_fname,c_value,i_value_chars,
     &        c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'undef') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(undef,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'ascii_topoveg') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(ascii_topoveg,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (ascii_topoveg.ne.0.0 .and. ascii_topoveg.ne.1.0) then
              print *,'ascii_topoveg not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'topoveg_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(topoveg_fname,c_value,i_value_chars,
     &        c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'topo_ascii_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(topo_ascii_fname,c_value,i_value_chars,
     &        c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'veg_ascii_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(veg_ascii_fname,c_value,i_value_chars,
     &        c_param(1:i_param_chars))
          endif

c         if (c_param(1:i_param_chars).eq.'veg_shd_24') then
c           call char2real(vegsnowdepth(24),i_value_chars,c_value,
c    &        c_param(1:i_param_chars))
c           if (vegsnowdepth(24).lt.0.0 .or. vegsnowdepth(24).gt.20.0)
c    &        then
c             print *,'veg_shd_24 out of range'
c             stop
c           endif
c         endif

          if (c_param(1:i_param_chars).eq.'veg_shd_25') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(vegsnowdepth(25),i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (vegsnowdepth(25).lt.0.0 .or. vegsnowdepth(25).gt.20.0)
     &        then
              print *,'veg_shd_25 out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'veg_shd_26') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(vegsnowdepth(26),i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (vegsnowdepth(26).lt.0.0 .or. vegsnowdepth(26).gt.20.0)
     &        then
              print *,'veg_shd_26 out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'veg_shd_27') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(vegsnowdepth(27),i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (vegsnowdepth(27).lt.0.0 .or. vegsnowdepth(27).gt.20.0)
     &        then
              print *,'veg_shd_27 out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'veg_shd_28') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(vegsnowdepth(28),i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (vegsnowdepth(28).lt.0.0 .or. vegsnowdepth(28).gt.20.0)
     &        then
              print *,'veg_shd_28 out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'veg_shd_29') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(vegsnowdepth(29),i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (vegsnowdepth(29).lt.0.0 .or. vegsnowdepth(29).gt.20.0)
     &        then
              print *,'veg_shd_29 out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'veg_shd_30') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(vegsnowdepth(30),i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (vegsnowdepth(30).lt.0.0 .or. vegsnowdepth(30).gt.20.0)
     &        then
              print *,'veg_shd_30 out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'const_veg_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(const_veg_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (const_veg_flag.lt.0.0 .or. const_veg_flag.gt.30.0) then
              print *,'const_veg_flag out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'iveg_ht_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(iveg_ht_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (iveg_ht_flag.lt.-1 .or. iveg_ht_flag.gt.1) then
              print *,'iveg_ht_flag out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'xlat') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(xlat,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (xlat.lt.-90.0 .or. xlat.gt.90.0) then
              print *,'xlat out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'lat_solar_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(lat_solar_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (lat_solar_flag.lt.-1 .or. lat_solar_flag.gt.1) then
              print *,'lat_solar_flag out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'UTC_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(UTC_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (UTC_flag.lt.-1 .or. UTC_flag.gt.1) then
              print *,'UTC_flag out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'run_micromet') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(run_micromet,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (run_micromet.ne.0.0 .and. run_micromet.ne.1.0) then
              print *,'run_micromet not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'run_enbal') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(run_enbal,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (run_enbal.ne.0.0 .and. run_enbal.ne.1.0) then
              print *,'run_enbal not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'run_snowpack') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(run_snowpack,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (run_snowpack.ne.0.0 .and. run_snowpack.ne.1.0) then
              print *,'run_snowpack not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'run_snowtran') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(run_snowtran,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (run_snowtran.ne.0.0 .and. run_snowtran.ne.1.0) then
              print *,'run_snowtran not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'irun_data_assim') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(irun_data_assim,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (irun_data_assim.ne.0 .and. irun_data_assim.ne.1) then
              print *,'irun_data_assim not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'ihrestart_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(ihrestart_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (ihrestart_flag.lt.-2 .or. ihrestart_flag.gt.300000) then
              print *,'ihrestart_flag out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'i_dataassim_loop') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(i_dataassim_loop,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (i_dataassim_loop.lt.-366 .or. i_dataassim_loop.gt.1)
     &        then
              print *,'i_dataassim_loop out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'ihrestart_inc') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(ihrestart_inc,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (ihrestart_inc.lt.0 .or. ihrestart_inc.gt.8784) then
              print *,'ihrestart_inc out of range'
              stop
            endif
          endif

c MICROMET MODEL SETUP.
          if (c_param(1:i_param_chars).eq.'i_tair_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(i_tair_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (i_tair_flag.ne.0 .and. i_tair_flag.ne.1) then
              print *,'i_tair_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'i_rh_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(i_rh_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (i_rh_flag.ne.0 .and. i_rh_flag.ne.1) then
              print *,'i_rh_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'i_wind_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(i_wind_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (i_wind_flag.ne.-1 .and. i_wind_flag.ne.0 .and.
     &        i_wind_flag.ne.1) then
              print *,'i_wind_flag not -1, 0, or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'i_solar_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(i_solar_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (i_solar_flag.ne.0 .and. i_solar_flag.ne.1) then
              print *,'i_solar_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'i_longwave_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(i_longwave_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (i_longwave_flag.ne.0 .and. i_longwave_flag.ne.1) then
              print *,'i_longwave_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'i_prec_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(i_prec_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (i_prec_flag.ne.0 .and. i_prec_flag.ne.1) then
              print *,'i_prec_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'ifill') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(ifill,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (ifill.ne.0 .and. ifill.ne.1) then
              print *,'ifill not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'iobsint') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(iobsint,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (iobsint.ne.0 .and. iobsint.ne.1) then
              print *,'iobsint not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'dn') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(dn,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (dn.lt.1.0 .or. dn.gt.10000.0) then
              print *,'dn out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'barnes_lg_domain') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(barnes_lg_domain,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (barnes_lg_domain.ne.0.0 .and. barnes_lg_domain.ne.1.0
     &        .and. barnes_lg_domain.ne.2.0) then
              print *,'barnes_lg_domain not 0.0 or 1.0 or 2.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'n_stns_used') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(n_stns_used,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (n_stns_used.lt.1 .or. n_stns_used.gt.9) then
              print *,'n_stns_used out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'snowmodel_line_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(snowmodel_line_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (snowmodel_line_flag.ne.0.0 .and.
     &        snowmodel_line_flag.ne.1.0)
     &        then
              print *,'snowmodel_line_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'check_met_data') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(check_met_data,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (check_met_data.ne.0.0 .and. check_met_data.ne.1.0)
     &        then
              print *,'check_met_data not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'curve_len_scale') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(curve_len_scale,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (curve_len_scale.le.0.0 .or. curve_len_scale.gt.5000.0)
     &        then
              print *,'curve_len_scale out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'slopewt') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(slopewt,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (slopewt.lt.0.0 .or. slopewt.gt.1.0) then
              print *,'slopewt out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'curvewt') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(curvewt,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (curvewt.lt.0.0 .or. curvewt.gt.1.0) then
              print *,'curvewt out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'curve_lg_scale_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(curve_lg_scale_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (curve_lg_scale_flag.ne.0.0 .and.
     &        curve_lg_scale_flag.ne.1.0) then
              print *,'curve_lg_scale_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'windspd_min') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(windspd_min,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (windspd_min.lt.0.1 .or. windspd_min.gt.2.0) then
              print *,'windspd_min out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'lapse_rate_user_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(lapse_rate_user_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (lapse_rate_user_flag.ne.0 .and.
     &        lapse_rate_user_flag.ne.1) then
              print *,'lapse_rate_user_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.
     &      'iprecip_lapse_rate_user_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(iprecip_lapse_rate_user_flag,i_value_chars,
     &        c_value,c_param(1:i_param_chars))
            if (iprecip_lapse_rate_user_flag.ne.0 .and.
     &        iprecip_lapse_rate_user_flag.ne.1) then
              print *,'iprecip_lapse_rate_user_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'iprecip_scheme') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(iprecip_scheme,i_value_chars,
     &        c_value,c_param(1:i_param_chars))
            if (iprecip_scheme.ne.1 .and. iprecip_scheme.ne.2) then
              print *,'iprecip_scheme not 1 or 2'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'snowfall_frac') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(snowfall_frac,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (snowfall_frac.ne.1.0 .and. snowfall_frac.ne.2.0
     &        .and. snowfall_frac.ne.3.0) then
              print *,'snowfall_frac must be = 1.0, 2.0, or 3.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'wind_lapse_rate') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(wind_lapse_rate,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (wind_lapse_rate.lt.0.0) then
              print *,'wind_lapse_rate must be >= 0.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'calc_subcanopy_met') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(calc_subcanopy_met,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (calc_subcanopy_met.ne.0.0 .and.
     &        calc_subcanopy_met.ne.1.0) then
              print *,'calc_subcanopy_met not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'gap_frac') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(gap_frac,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (gap_frac.lt.0.0 .or. gap_frac.gt.1.0) then
              print *,'gap_frac out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'cloud_frac_factor') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(cloud_frac_factor,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (cloud_frac_factor.lt.0.0 .or. cloud_frac_factor.gt.1.0)
     &        then
              print *,'cloud_frac_factor out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'use_shortwave_obs') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(use_shortwave_obs,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (use_shortwave_obs.ne.0.0 .and. use_shortwave_obs.ne.1.0)
     &        then
              print *,'use_shortwave_obs not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'use_longwave_obs') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(use_longwave_obs,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (use_longwave_obs.ne.0.0 .and. use_longwave_obs.ne.1.0)
     &        then
              print *,'use_longwave_obs not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'use_sfc_pressure_obs') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(use_sfc_pressure_obs,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (use_sfc_pressure_obs.ne.0.0 .and.
     &        use_sfc_pressure_obs.ne.1.0) then
              print *,'use_sfc_pressure_obs not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'cf_precip_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(cf_precip_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (cf_precip_flag.ne.0.0 .and. cf_precip_flag.ne.1.0 .and.
     &        cf_precip_flag.ne.2.0 .and. cf_precip_flag.ne.3.0) then
              print *,'cf_precip_flag not 0.0, 1.0, 2.0, or 3.0'
              stop
            endif
          endif

c SNOWTRAN-3D MODEL SETUP.
          if (c_param(1:i_param_chars).eq.'Utau_t_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(Utau_t_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (Utau_t_flag.ne.0.0 .and. Utau_t_flag.ne.1.0) then
              print *,'Utau_t_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'Utau_t_const') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(Utau_t_const,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (Utau_t_const.le.0.0 .and. Utau_t_const.gt.1.0) then
              print *,'Utau_t_const out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'subgrid_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(subgrid_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (subgrid_flag.ne.0.0 .and. subgrid_flag.ne.1.0) then
              print *,'subgrid_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'tabler_dir') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(tabler_dir,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (tabler_dir.lt.-360.0 .or. tabler_dir.gt.360.0) then
              print *,'tabler_dir out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'slope_adjust') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(slope_adjust,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (slope_adjust.lt.0.0 .or. slope_adjust.gt.3.0) then
              print *,'slope_adjust out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'twolayer_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(twolayer_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (twolayer_flag.ne.0.0 .and. twolayer_flag.ne.1.0) then
              print *,'twolayer_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'bc_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(bc_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (bc_flag.ne.0.0 .and. bc_flag.ne.1.0) then
              print *,'bc_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'ht_windobs') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(ht_windobs,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (ht_windobs.le.0.0 .or. ht_windobs.gt.20.0) then
              print *,'ht_windobs out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'ht_rhobs') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(ht_rhobs,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (ht_rhobs.le.0.0 .or. ht_rhobs.gt.20.0) then
              print *,'ht_rhobs out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'ro_snow') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(ro_snow,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (ro_snow.le.100.0 .or. ro_snow.gt.550.0) then
              print *,'ro_snow out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'snow_d_init_const') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(snow_d_init_const,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (snow_d_init_const.lt.0.0 .or.
     &        snow_d_init_const.gt.5.0) then
              print *,'snow_d_init_const out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'topoflag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(topoflag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (topoflag.ne.0.0 .and. topoflag.ne.1.0) then
              print *,'topoflag not 0.0 or 1.0'
              stop
            endif
          endif

c ENBAL MODEL SETUP.
          if (c_param(1:i_param_chars).eq.'icond_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(icond_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (icond_flag.ne.0 .and. icond_flag.ne.1) then
              print *,'icond_flag not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'albedo_snow_forest') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(albedo_snow_forest,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (albedo_snow_forest.lt.0.3 .or.
     &        albedo_snow_forest.gt.0.8) then
              print *,'albedo_snow_forest out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'albedo_snow_clearing') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(albedo_snow_clearing,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (albedo_snow_clearing.lt.0.4 .or.
     &        albedo_snow_clearing.gt.0.8) then
              print *,'albedo_snow_clearing out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'albedo_glacier') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(albedo_glacier,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (albedo_glacier.lt.0.3 .or. albedo_glacier.gt.0.8) then
              print *,'albedo_glacier out of range'
              stop
            endif
          endif

c SNOWPACK MODEL SETUP.
          if (c_param(1:i_param_chars).eq.'sfc_sublim_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(sfc_sublim_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (sfc_sublim_flag.ne.0.0 .and. sfc_sublim_flag.ne.1.0)
     &        then
              print *,'sfc_sublim_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'multilayer_snowpack') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(multilayer_snowpack,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (multilayer_snowpack.ne.0 .and. multilayer_snowpack.ne.1)
     &        then
              print *,'multilayer_snowpack not 0 or 1'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'tsls_threshold') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(tsls_threshold,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (tsls_threshold.lt.1.0 .or. tsls_threshold.gt.8760.0)
     &        then
              print *,'tsls_threshold out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'max_layers') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(max_layers,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (max_layers.lt.1 .or. max_layers.gt.100) then
              print *,'max_layers out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'dz_snow_min') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(dz_snow_min,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (dz_snow_min.lt.0.0 .or. dz_snow_min.gt.5.0) then
              print *,'dz_snow_min out of range'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'izero_snow_date') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2int(izero_snow_date,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (izero_snow_date.lt.10100 .or.
     &        izero_snow_date.gt.999999) then
              print *,'izero_snow_date out of range'
              stop
            endif
          endif

c SEAICE MODEL SETUP.
          if (c_param(1:i_param_chars).eq.'seaice_run') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(seaice_run,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (seaice_run.ne.0.0 .and. seaice_run.ne.1.0 .and.
     &        seaice_run.ne.2.0 .and. seaice_run.ne.3.0 .and.
     &        seaice_run.ne.4.0) then
              print *,'seaice_run not 0.0, 1.0, 2.0, 3.0, or 4.0'
              stop
            endif
          endif

c PRINTING OUTPUT FILES.
          if (c_param(1:i_param_chars).eq.'print_micromet') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(print_micromet,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (print_micromet.ne.0.0 .and. print_micromet.ne.1.0) then
              print *,'print_micromet not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'micromet_output_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(micromet_output_fname,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'print_snowtran') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(print_snowtran,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (print_snowtran.ne.0.0 .and. print_snowtran.ne.1.0) then
              print *,'print_snowtran not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'snowtran_output_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(snowtran_output_fname,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'Tabler_1_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(Tabler_1_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (Tabler_1_flag.ne.0.0 .and. Tabler_1_flag.ne.1.0) then
              print *,'Tabler_1_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'tabler_sfc_path_name') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(tabler_sfc_path_name,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'Tabler_2_flag') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(Tabler_2_flag,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (Tabler_2_flag.ne.0.0 .and. Tabler_2_flag.ne.1.0) then
              print *,'Tabler_2_flag not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_enbal') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(print_enbal,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (print_enbal.ne.0.0 .and. print_enbal.ne.1.0) then
              print *,'print_enbal not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'enbal_output_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(enbal_output_fname,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'print_snowpack') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(print_snowpack,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (print_snowpack.ne.0.0 .and. print_snowpack.ne.1.0) then
              print *,'print_snowpack not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'snowpack_output_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(snowpack_output_fname,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'print_multilayer') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(print_multilayer,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (print_multilayer.ne.0.0 .and. print_multilayer.ne.1.0
     &        .and. print_multilayer.ne.2.0)
     &        then
              print *,'print_multilayer not 0.0 or 1.0 or 2.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.
     &      'multilayer_output_fname') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(multilayer_output_fname,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'print_user') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(print_user,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (print_user.ne.0.0 .and. print_user.ne.1.0) then
              print *,'print_user not 0.0 or 1.0'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_inc') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2real(print_inc,i_value_chars,c_value,
     &        c_param(1:i_param_chars))
            if (print_inc.ne.1.0 .and. print_inc.ne.8.0 .and.
     &       print_inc.ne.24.0) then
              print *,'print_inc not 1.0 or 8.0 or 24.0.  You are'
              print *,'using some number that is not typical and'
              print *,'should make certain you and the code are' 
              print *,'doing exactly what you think it is and want'
              print *,'to do.'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'output_path_wo_assim') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(output_path_wo_assim,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
          endif

          if (c_param(1:i_param_chars).eq.'output_path_wi_assim') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(output_path_wi_assim,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
          endif

c THESE ARE THE 30 PRINT_VAR_## FLAGS.
          if (c_param(1:i_param_chars).eq.'print_var_01') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_01,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_01.ne.'y' .and. print_var_01.ne.'n') then
              print *,'print_var_01 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_02') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_02,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_02.ne.'y' .and. print_var_02.ne.'n') then
              print *,'print_var_02 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_03') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_03,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_03.ne.'y' .and. print_var_03.ne.'n') then
              print *,'print_var_03 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_04') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_04,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_04.ne.'y' .and. print_var_04.ne.'n') then
              print *,'print_var_04 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_05') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_05,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_05.ne.'y' .and. print_var_05.ne.'n') then
              print *,'print_var_05 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_06') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_06,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_06.ne.'y' .and. print_var_06.ne.'n') then
              print *,'print_var_06 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_07') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_07,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_07.ne.'y' .and. print_var_07.ne.'n') then
              print *,'print_var_07 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_08') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_08,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_08.ne.'y' .and. print_var_08.ne.'n') then
              print *,'print_var_08 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_09') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_09,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_09.ne.'y' .and. print_var_09.ne.'n') then
              print *,'print_var_09 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_10') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_10,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_10.ne.'y' .and. print_var_10.ne.'n') then
              print *,'print_var_10 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_11') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_11,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_11.ne.'y' .and. print_var_11.ne.'n') then
              print *,'print_var_11 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_12') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_12,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_12.ne.'y' .and. print_var_12.ne.'n') then
              print *,'print_var_12 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_13') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_13,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_13.ne.'y' .and. print_var_13.ne.'n') then
              print *,'print_var_13 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_14') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_14,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_14.ne.'y' .and. print_var_14.ne.'n') then
              print *,'print_var_14 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_15') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_15,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_15.ne.'y' .and. print_var_15.ne.'n') then
              print *,'print_var_15 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_16') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_16,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_16.ne.'y' .and. print_var_16.ne.'n') then
              print *,'print_var_16 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_17') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_17,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_17.ne.'y' .and. print_var_17.ne.'n') then
              print *,'print_var_17 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_18') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_18,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_18.ne.'y' .and. print_var_18.ne.'n') then
              print *,'print_var_18 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_19') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_19,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_19.ne.'y' .and. print_var_19.ne.'n') then
              print *,'print_var_19 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_20') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_20,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_20.ne.'y' .and. print_var_20.ne.'n') then
              print *,'print_var_20 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_21') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_21,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_21.ne.'y' .and. print_var_21.ne.'n') then
              print *,'print_var_21 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_22') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_22,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_22.ne.'y' .and. print_var_22.ne.'n') then
              print *,'print_var_22 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_23') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_23,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_23.ne.'y' .and. print_var_23.ne.'n') then
              print *,'print_var_23 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_24') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_24,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_24.ne.'y' .and. print_var_24.ne.'n') then
              print *,'print_var_24 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_25') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_25,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_25.ne.'y' .and. print_var_25.ne.'n') then
              print *,'print_var_25 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_26') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_26,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_26.ne.'y' .and. print_var_26.ne.'n') then
              print *,'print_var_26 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_27') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_27,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_27.ne.'y' .and. print_var_27.ne.'n') then
              print *,'print_var_27 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_28') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_28,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_28.ne.'y' .and. print_var_28.ne.'n') then
              print *,'print_var_28 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_29') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_29,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_29.ne.'y' .and. print_var_29.ne.'n') then
              print *,'print_var_29 not y or n'
              stop
            endif
          endif

          if (c_param(1:i_param_chars).eq.'print_var_30') then
            ipar_count = ipar_count + 1
            cpar_name(ipar_count) = c_param(1:i_param_chars)
            call char2char(print_var_30,c_value,
     &        i_value_chars,c_param(1:i_param_chars))
            if (print_var_30.ne.'y' .and. print_var_30.ne.'n') then
              print *,'print_var_30 not y or n'
              stop
            endif
          endif

c Real example
c         if (c_param(1:i_param_chars).eq.'')
c    &      call char2real(,i_value_chars,c_value,
c    &        c_param(1:i_param_chars))

c Integer example.
c         if (c_param(1:i_param_chars).eq.'nx')
c    &      call char2int(nx,i_value_chars,c_value,
c    &        c_param(1:i_param_chars))

c Character example.
c         if (c_param(1:i_param_chars).eq.'fname_out')
c    &      call char2char(fname_out,c_value,i_value_chars,
c    &        c_param(1:i_param_chars))

        endif
      enddo

  99  continue

c Use the print_var_## parameters to build the print control
c   array "print_var(n_print_vars)" used in output_user.f.
      print_var(1) = print_var_01
      print_var(2) = print_var_02
      print_var(3) = print_var_03
      print_var(4) = print_var_04
      print_var(5) = print_var_05
      print_var(6) = print_var_06
      print_var(7) = print_var_07
      print_var(8) = print_var_08
      print_var(9) = print_var_09
      print_var(10) = print_var_10
      print_var(11) = print_var_11
      print_var(12) = print_var_12
      print_var(13) = print_var_13
      print_var(14) = print_var_14
      print_var(15) = print_var_15
      print_var(16) = print_var_16
      print_var(17) = print_var_17
      print_var(18) = print_var_18
      print_var(19) = print_var_19
      print_var(20) = print_var_20
      print_var(21) = print_var_21
      print_var(22) = print_var_22
      print_var(23) = print_var_23
      print_var(24) = print_var_24
      print_var(25) = print_var_25
      print_var(26) = print_var_26
      print_var(27) = print_var_27
      print_var(28) = print_var_28
      print_var(29) = print_var_29
      print_var(30) = print_var_30

c Check the input-parameter counting array to be sure that all
c   parameters have been defined.
      ipar_flag = npars - ipar_count
      if (ipar_flag.ne.0) then
        print *
        print *
        print *,'THERE ARE MISSING VARIABLES IN THE .PAR FILE.'
        print *
        print *,'  THIS MANY VARIABLES ARE MISSING:',ipar_flag
        print *
        print *,'  THESE ARE THE VARIABLES THAT HAVE BEEN READ IN:'

        do k=1,npars
          print *,'VARIABLE NAME =',k,'   ',cpar_name(k)
        enddo

        print *
        print *,'ALL .PAR VARIABLES MUST BE DEFINED BEFORE'
        print *,'  YOU CAN CONTINUE.'
        print *

        stop
      endif

c Put some space at the end of the parameter printouts.
      print *

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine char2char(cvar,c_value,i_value_chars,
     &  c_param)

      implicit none

      character*(*) c_value,c_param,cvar
      integer i_value_chars

      cvar = c_value(1:i_value_chars)

      print *,c_param,' = ',cvar(1:i_value_chars)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine char2real(rvar,i_value_chars,c_value,
     &  c_param)

      implicit none

      character*(*) c_value,c_param
      integer i_value_chars
      real rvar
      character*8 form

c Read an real value (rvar) from the character string (c_value).
      write (form,90) i_value_chars
   90 format ('(f',i2,'.0)')
      read (c_value,form) rvar

      print *,c_param,' =',rvar

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine char2double(dvar,i_value_chars,c_value,
     &  c_param)

      implicit none

      character*(*) c_value,c_param
      integer i_value_chars
      double precision dvar
      character*8 form

c Read an double precision value (dvar) from the character
c   string (c_value).
      write (form,90) i_value_chars
   90 format ('(f',i2,'.0)')
      read (c_value,form) dvar

      print *,c_param,' =',dvar

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine char2int(ivar,i_value_chars,c_value,
     &  c_param)

      implicit none

      character*(*) c_value,c_param
      integer i_value_chars,ivar
      character*8 form

c Read an integer value (ivar) from the character string (c_value).
      write (form,90) i_value_chars
   90 format ('(i',i2,')')
      read (c_value,form) ivar

      print *,c_param,' =',ivar

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_param_data(input_string,c_param,c_value,
     &  i_param_chars,i_value_chars,icomment_flag)

      implicit none

      character*(*) input_string,c_param,c_value

      integer leading_blanks,trailing_blanks
      integer i,icomment_flag
      integer i_param_start,i_equals_position,i_param_end,
     &  i_value_start,i_value_end,i_param_chars,i_value_chars,
     &  i_loc,i_leading_blanks,i_trailing_blanks

c If the input string is not a comment line, process the data.
      if (input_string(1:1).ne.'!') then

c First count the number of leading and trailing blanks.
        i_leading_blanks = leading_blanks(input_string)
        i_trailing_blanks = trailing_blanks(input_string)

c If the input string is not completely blank, process the data.
        if (i_leading_blanks.ne.len(input_string)) then
          icomment_flag = 0

c Define the starting and ending points of the parameter name and
c   parameter value.
          i_param_start = i_leading_blanks + 1
          i_equals_position = index(input_string,'=')
          i_param_end = i_equals_position - 2
          i_value_start = i_equals_position + 2
          i_value_end =  len(input_string) - i_trailing_blanks
          i_param_chars = i_param_end - i_param_start + 1
          i_value_chars = i_value_end - i_value_start + 1

c Pull out the parameter name and value.
          do i=1,i_param_chars
            i_loc = i + i_param_start - 1
            c_param(i:i) = input_string(i_loc:i_loc)
          enddo

          do i=1,i_value_chars
            i_loc = i + i_value_start - 1
            c_value(i:i) = input_string(i_loc:i_loc)
          enddo

c Check to see whether we have found any paths/fnames that are
c   longer than 80 characters.
          if (i_value_chars.gt.80) then
            print *
            print *, input_string
            print *, 'Number of characters found =',i_value_chars
            print *
            print *, 'Found path/fname longer than 80 characters;'
            print *, 'the limit is 80 characters for the string'
            print *, 'starting in the second character position'
            print *, 'after the "=" sign.'
            print *
            stop
          endif

        else
          icomment_flag = 1
        endif

      else
        icomment_flag = 1
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer function leading_blanks(input_string)

      implicit none

      integer k
      character*(*) input_string

c Count the number of blanks preceeding the first non-blank
c   character.
      leading_blanks = 0
      do k=1,len(input_string)
        if (input_string(k:k).eq.' ') then
          leading_blanks = leading_blanks + 1
        else
          return
        endif
      enddo

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

      subroutine GET_DOT_PAR_PATH_LOCATION(snowmodel_dot_par_fname)

c The following provides the ability to read in a 'snowmodel.par'
c   file that is called something other than 'snowmodel.par' and/or
c   located somewhere different than the directory where the
c   'snowmodel' executable file is located.  If you run:
c     snowmodel
c   it assumes the .par file is called 'snowmodel.par' and is
c   located in the default location.  You can also run the code as
c   follows:
c     snowmodel parpath/parname.par
c   and it will look for the .par file in the different location
c   and/or the different name.

      implicit none

      integer nargs,iargc
      character*100 snowmodel_dot_par_fname

      snowmodel_dot_par_fname = "snowmodel.par"

      nargs = iargc()

      if (nargs.eq.1) then
        call getarg (1,snowmodel_dot_par_fname)
      endif

      print *
      print *, snowmodel_dot_par_fname

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c snowmodel_main.f
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c All units are in m, kg, s, K.
c
c The author of this code is:
c
c   Dr. Glen E. Liston
c   Cooperative Institute for Research
c     in the Atmosphere (CIRA)         |
c   Colorado State University
c   Fort Collins, Colorado 80523-1375
c
c   Voice: (970) 491-8220
c   FAX: (970) 491-8241
c
c   glen.liston@colostate.edu

      implicit none

      include 'snowmodel.inc'

      include 'snowmodel_vars.inc'

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccc  INITIALIZE THE MODEL  cccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c The following provides the ability to read in a 'snowmodel.par'
c   file that is called something other than 'snowmodel.par' and/or
c   located somewhere different than the directory where the
c   'snowmodel' executable file is located.  If you run:
c     snowmodel
c   it assumes the .par file is called 'snowmodel.par' and is
c   located in the default location.  You can also run the code as
c   follows:
c     snowmodel parpath/parname.par
c   and it will look for the .par file in the different location
c   and/or the different name.
      CALL GET_DOT_PAR_PATH_LOCATION(snowmodel_dot_par_fname)

c Read the input parameters.
      CALL READPARAM_CODE(dt,deltax,deltay,Utau_t_flag,
     &  subgrid_flag,twolayer_flag,snowmodel_dot_par_fname,
     &  bc_flag,curve_len_scale,slopewt,curvewt,ht_windobs,
     &  ht_rhobs,ro_snow,snow_d_init_const,const_veg_flag,
     &  vegsnowdepth,nx,ny,max_iter,met_input_fname,xmn,ymn,
     &  iyear_init,imonth_init,iday_init,xhour_init,undef,ifill,
     &  iobsint,dn,xlat,i_tair_flag,i_rh_flag,i_wind_flag,
     &  i_solar_flag,i_prec_flag,isingle_stn_flag,igrads_metfile,
     &  windspd_min,icond_flag,run_micromet,run_enbal,run_snowpack,
     &  run_snowtran,topoflag,topoveg_fname,snowtran_output_fname,
     &  micromet_output_fname,enbal_output_fname,Utau_t_const,
     &  snowpack_output_fname,print_micromet,print_enbal,
     &  print_snowpack,print_snowtran,i_longwave_flag,print_user,
     &  ascii_topoveg,topo_ascii_fname,veg_ascii_fname,
     &  irun_data_assim,lapse_rate_user_flag,
     &  iprecip_lapse_rate_user_flag,use_shortwave_obs,
     &  use_longwave_obs,use_sfc_pressure_obs,calc_subcanopy_met,
     &  sfc_sublim_flag,gap_frac,cloud_frac_factor,
     &  albedo_snow_forest,albedo_snow_clearing,albedo_glacier,
     &  barnes_lg_domain,n_stns_used,tabler_dir,slope_adjust,
     &  lat_solar_flag,UTC_flag,iveg_ht_flag,ihrestart_flag,
     &  ihrestart_inc,i_dataassim_loop,tsls_threshold,dz_snow_min,
     &  print_multilayer,multilayer_snowpack,max_layers,
     &  multilayer_output_fname,izero_snow_date,curve_lg_scale_flag,
     &  check_met_data,seaice_run,snowmodel_line_flag,wind_lapse_rate,
     &  iprecip_scheme,cf_precip_flag,snowfall_frac,print_inc,
     &  output_path_wo_assim,output_path_wi_assim,Tabler_1_flag,
     &  Tabler_2_flag,tabler_sfc_path_name,print_var)

c This loop runs the correction/data assimilation adjustment
c   iterations.
      if (ihrestart_flag.ge.0) then
        if (i_dataassim_loop.lt.0.0) then
          i_corr_start = 2
        else
          i_corr_start = 1
        endif
      else
        i_corr_start = 1
      endif

      do icorr_factor_loop=i_corr_start,irun_data_assim+1

c Perform the correction (precipitation and melt) factor
c   calculations.
        if (irun_data_assim.eq.1 .and. icorr_factor_loop.eq.2) then
          CALL DATAASSIM_USER(nx,ny,icorr_factor_index,
     &      corr_factor,max_iter,deltax,deltay,xmn,ymn,nobs_dates,
     &      print_inc,iday_init,imonth_init,iyear_init,dt,
     &      output_path_wo_assim,xhour_init)
          if (ihrestart_flag.ge.-1) then
            CALL HRESTART_SAVE_DA(nx,ny,max_iter,corr_factor,
     &        icorr_factor_index,nobs_dates)
          endif
        endif

c Perform a variety of preprocessing and model setup steps, like
c   read in topography and vegetation arrays, open input and output
c   files, etc.
        CALL PREPROCESS_CODE(topoveg_fname,const_veg_flag,
     &    vegtype,veg_z0,vegsnowdepth,fetch,xmu,C_z,h_const,
     &    wind_min,Up_const,dz_susp,ztop_susp,fall_vel,Ur_const,
     &    ro_water,ro_air,gravity,vonKarman,pi,twopio360,snow_z0,
     &    nx,ny,sum_sprec,sum_qsubl,sum_trans,sum_unload,topo,
     &    topo_land,snow_d,topoflag,snow_d_init,snow_d_init_const,
     &    soft_snow_d,met_input_fname,igrads_metfile,deltax,deltay,
     &    snowtran_output_fname,micromet_output_fname,
     &    enbal_output_fname,snowpack_output_fname,print_micromet,
     &    print_enbal,print_snowpack,print_snowtran,run_micromet,
     &    run_enbal,run_snowpack,run_snowtran,ro_snow_grid,swe_depth,
     &    sum_runoff,sum_prec,ro_snow,twolayer_flag,sum_Qcs,
     &    canopy_int,ascii_topoveg,topo_ascii_fname,icorr_factor_loop,
     &    veg_ascii_fname,undef,isingle_stn_flag,max_iter,
     &    i_tair_flag,i_rh_flag,i_wind_flag,i_prec_flag,sum_glacmelt,
     &    snow_depth,sum_d_canopy_int,corr_factor,icorr_factor_index,
     &    sum_sfcsublim,barnes_lg_domain,n_stns_used,k_stn,xmn,ymn,
     &    ro_soft_snow_old,sum_swemelt,xlat,lat_solar_flag,xlat_grid,
     &    xlon_grid,UTC_flag,dt,swe_depth_old,canopy_int_old,
     &    vegsnowd_xy,iveg_ht_flag,ihrestart_flag,i_dataassim_loop,
     &    multilayer_snowpack,max_layers,multilayer_output_fname,
     &    print_multilayer,KK,tslsnowfall,tsls_threshold,
     &    irun_data_assim,izero_snow_date,iclear_mn,iclear_dy,
     &    xclear_hr,snod_layer,swed_layer,ro_layer,T_old,gamma,
     &    icond_flag,curve_lg_scale_flag,curve_wt_lg,check_met_data,
     &    seaice_run,snowmodel_line_flag,xg_line,yg_line,print_user,
     &    cf_precip_flag,cf_precip,print_inc,xhour_init,Tabler_1_flag,
     &    Tabler_2_flag,iyear_init,imonth_init,iday_init,print_var,
     &    output_path_wo_assim,output_path_wi_assim,nrecs_max,
     &    tabler_sfc_path_name,print_outvars,diam_layer)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccc  RUN THE MODEL  cccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Loop through the number of time steps (iterations) in the
c   simulation.
        if (ihrestart_flag.ge.0) then
          iter_start = ihrestart_flag + 1
        else
          iter_start = 1
        endif

        do iter=iter_start,max_iter

c Distribute the meteorological station data.
          if (run_micromet.eq.1.0) then
            CALL MICROMET_CODE(nx,ny,xmn,ymn,deltax,deltay,
     &        iyear_init,imonth_init,iday_init,xhour_init,dt,undef,
     &        ifill,iobsint,dn,iter,curve_len_scale,slopewt,curvewt,
     &        topo,curvature,terrain_slope,slope_az,Tair_grid,
     &        rh_grid,uwind_grid,vwind_grid,Qsi_grid,prec_grid,
     &        i_tair_flag,i_rh_flag,i_wind_flag,i_solar_flag,
     &        i_prec_flag,isingle_stn_flag,igrads_metfile,
     &        windspd_grid,winddir_grid,windspd_flag,winddir_flag,
     &        sprec,windspd_min,Qli_grid,i_longwave_flag,vegtype,
     &        forest_LAI,iyear,imonth,iday,xhour,corr_factor,
     &        icorr_factor_index,lapse_rate_user_flag,
     &        iprecip_lapse_rate_user_flag,use_shortwave_obs,
     &        use_longwave_obs,use_sfc_pressure_obs,sfc_pressure,
     &        run_enbal,run_snowpack,calc_subcanopy_met,vegsnowd_xy,
     &        gap_frac,cloud_frac_factor,barnes_lg_domain,n_stns_used,
     &        k_stn,xlat_grid,xlon_grid,UTC_flag,icorr_factor_loop,
     &        snowmodel_line_flag,xg_line,yg_line,irun_data_assim,
     &        wind_lapse_rate,iprecip_scheme,cf_precip_flag,cf_precip,
     &        cloud_frac_grid,snowfall_frac,seaice_run)

            if (print_micromet.eq.1.0) then
              write(81,rec=iter)
     &          ((Tair_grid(i,j)-273.15,i=1,nx),j=1,ny),
     &          ((rh_grid(i,j),i=1,nx),j=1,ny),
     &          ((uwind_grid(i,j),i=1,nx),j=1,ny),
     &          ((vwind_grid(i,j),i=1,nx),j=1,ny),
     &          ((windspd_grid(i,j),i=1,nx),j=1,ny),
     &          ((winddir_grid(i,j),i=1,nx),j=1,ny),
     &          ((Qsi_grid(i,j),i=1,nx),j=1,ny),
     &          ((Qli_grid(i,j),i=1,nx),j=1,ny),
     &          ((prec_grid(i,j),i=1,nx),j=1,ny)
            endif
          endif

c Perform a surface energy balance over the domain.
          if (run_enbal.eq.1.0) then
            CALL ENBAL_CODE(nx,ny,Tair_grid,uwind_grid,sfc_pressure,
     &        vwind_grid,rh_grid,Tsfc,Qsi_grid,Qli_grid,Qle,Qh,Qe,
     &        Qc,Qm,e_balance,Qf,snow_d,ht_windobs,icond_flag,
     &        albedo,snow_z0,veg_z0,vegtype,undef,albedo_snow_forest,
     &        albedo_snow_clearing,albedo_glacier,snod_layer,T_old,
     &        gamma,KK)

            if (print_enbal.eq.1.0) then
              write(82,rec=iter)
     &          ((Tair_grid(i,j)-273.15,i=1,nx),j=1,ny),
     &          ((Tsfc(i,j)-273.15,i=1,nx),j=1,ny),
     &          ((Qsi_grid(i,j),i=1,nx),j=1,ny),
     &          ((Qli_grid(i,j),i=1,nx),j=1,ny),
     &          ((Qle(i,j),i=1,nx),j=1,ny),
     &          ((Qh(i,j),i=1,nx),j=1,ny),
     &          ((Qe(i,j),i=1,nx),j=1,ny),
     &          ((Qc(i,j),i=1,nx),j=1,ny),
     &          ((Qm(i,j),i=1,nx),j=1,ny),
     &          ((albedo(i,j),i=1,nx),j=1,ny),
     &          ((e_balance(i,j),i=1,nx),j=1,ny)
            endif
          endif

c Evolve the snowpack according to the defined melt and
c   precipitation inputs.
          if (run_snowpack.eq.1.0) then
            CALL SNOWPACK_CODE(nx,ny,Tair_grid,rh_grid,ro_nsnow,
     &        dt,swe_depth,Tsfc,snow_d,prec_grid,runoff,Qm,rain,
     &        sprec,iter,w_balance,sum_prec,sum_runoff,xro_snow,
     &        undef,ro_snow,ro_snow_grid,soft_snow_d,sum_sprec,
     &        snow_depth,windspd_grid,Qsi_grid,sum_Qcs,canopy_int,
     &        Qcs,vegtype,forest_LAI,albedo,glacier_melt,
     &        canopy_unload,sum_unload,sum_glacmelt,run_snowtran,
     &        swemelt,d_canopy_int,sum_d_canopy_int,snow_d_init,
     &        sfc_pressure,Qe,sfc_sublim_flag,sum_sfcsublim,
     &        sum_swemelt,corr_factor,icorr_factor_index,swesublim,
     &        swe_depth_old,canopy_int_old,KK,max_layers,melt_flag,
     &        ro_snowmax,tsls_threshold,dz_snow_min,tslsnowfall,
     &        change_layer,snod_layer,swed_layer,ro_layer,T_old,gamma,
     &        multilayer_snowpack,seaice_run,seaice_conc,ht_windobs,
     &        windspd_2m_grid,diam_layer,flux_layer,sum_trans)
          endif

c Run the blowing-snow model.
          if (run_snowtran.eq.1.0) then
            CALL SNOWTRAN_CODE(bc_flag,bs_flag,C_z,
     &        conc_salt,deltax,deltay,dh_salt,dh_salt_u,dh_salt_v,
     &        dh_susp,dh_susp_u,dh_susp_v,dt,dz_susp,fall_vel,fetch,
     &        gravity,h_const,h_star,ht_rhobs,ht_windobs,index_ue,
     &        index_uw,index_vn,index_vs,iter,nx,ny,pi,Qsalt,Qsalt_max,
     &        Qsalt_maxu,Qsalt_maxv,Qsalt_u,Qsalt_v,Qsubl,Qsusp,
     &        Qsusp_u,Qsusp_v,rh_grid,ro_air,ro_snow,ro_water,snow_d,
     &        snow_d_init,snow_z0,soft_snow_d,sprec,sum_glacmelt,
     &        subgrid_flag,wbal_salt,wbal_susp,wbal_qsubl,sum_sprec,
     &        tabler_ee,tabler_ne,tabler_nn,tabler_nw,tabler_se,
     &        tabler_ss,tabler_sw,tabler_ww,tair_grid,topo,topo_land,
     &        topoflag,twolayer_flag,Up_const,Ur_const,Utau,
     &        Utau_t,uwind_grid,veg_z0,vegsnowd_xy,vegtype,vonKarman,
     &        vwind_grid,wind_min,winddir_flag,winddir_grid,
     &        windspd_flag,windspd_grid,xmu,z_0,ztop_susp,max_iter,
     &        run_enbal,run_snowpack,wbal_subgrid,sum_qsubl,sum_trans,
     &        swe_depth,snow_depth,ro_snow_grid,sum_prec,sum_runoff,
     &        sum_Qcs,canopy_int,w_balance,sum_sfcsublim,tabler_dir,
     &        slope_adjust,Utau_t_const,Utau_t_flag,ro_soft_snow_old,
     &        ro_soft_snow,ro_nsnow,prec_grid,Qcs,runoff,d_canopy_int,
     &        glacier_melt,swe_depth_old,swesublim,canopy_unload,
     &        canopy_int_old,iter_start,multilayer_snowpack,swed_layer,
     &        KK,snod_layer,ro_layer,curve_lg_scale_flag,curve_wt_lg,
     &        seaice_run,seaice_conc,tslsnowfall,T_old,tsls_threshold,
     &        curve_len_scale,Tabler_1_flag,Tabler_2_flag,undef,
     &        tabler_sfc_path_name,output_path_wo_assim,
     &        output_path_wi_assim,icorr_factor_loop,windspd_2m_grid,
     &        Qsubl_depth)
          endif

c If this is a sea ice run with incremental remapping, perform the
c   remapping here, before any data is written out for this time step.
c   Note that the incremental remapping programs must be compiled with
c   pgf90 or gfortran.
c         if (seaice_run.eq.3.0) then
c           call remapper_main(iter,nx,ny,seaice_conc,swe_depth,dt,
c    &        deltax,deltay)
c         endif

c Save the outputs from the SNOWPACK and SNOWTRAN routines.
          if (run_snowpack.eq.1.0 .and. print_snowpack.eq.1.0) then
            write(83,rec=iter)
     &        ((snow_depth(i,j),i=1,nx),j=1,ny),
     &        ((xro_snow(i,j),i=1,nx),j=1,ny),
     &        ((swe_depth(i,j),i=1,nx),j=1,ny),
     &        ((runoff(i,j),i=1,nx),j=1,ny),
     &        ((rain(i,j),i=1,nx),j=1,ny),
     &        ((sprec(i,j),i=1,nx),j=1,ny),
     &        ((Qcs(i,j),i=1,nx),j=1,ny),
     &        ((canopy_int(i,j),i=1,nx),j=1,ny),
     &        ((sum_Qcs(i,j),i=1,nx),j=1,ny),
     &        ((sum_prec(i,j),i=1,nx),j=1,ny),
     &        ((sum_sprec(i,j),i=1,nx),j=1,ny),
     &        ((sum_unload(i,j),i=1,nx),j=1,ny),
     &        ((sum_runoff(i,j),i=1,nx),j=1,ny),
     &        ((sum_swemelt(i,j),i=1,nx),j=1,ny),
     &        ((sum_sfcsublim(i,j),i=1,nx),j=1,ny),
     &        ((w_balance(i,j),i=1,nx),j=1,ny)
          endif

          if (run_snowtran.eq.1.0 .and. print_snowtran.eq.1.0) then
            write(84,rec=iter)
     &        ((snow_d(i,j),i=1,nx),j=1,ny),
     &        ((wbal_qsubl(i,j),i=1,nx),j=1,ny),
     &        ((wbal_salt(i,j),i=1,nx),j=1,ny),
     &        ((wbal_susp(i,j),i=1,nx),j=1,ny),
     &        ((wbal_subgrid(i,j),i=1,nx),j=1,ny),
     &        ((sum_qsubl(i,j),i=1,nx),j=1,ny),
     &        ((sum_trans(i,j),i=1,nx),j=1,ny)
          endif

c Note that here we write out the entire potential vertical domain
c   (nz_max), because it may have been filled with snow at some point
c   during the simulation.
          if (run_snowpack.eq.1.0 .and. multilayer_snowpack.eq.1 .and.
     &      print_multilayer.eq.1.0) then
            write(401,rec=iter)
     &        ((real(KK(i,j)),i=1,nx),j=1,ny),
     &        ((snow_depth(i,j),i=1,nx),j=1,ny),
     &        ((xro_snow(i,j),i=1,nx),j=1,ny),
     &        ((swe_depth(i,j),i=1,nx),j=1,ny),
     &        (((snod_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max),
     &        (((ro_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max),
     &        (((swed_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max),
     &        (((diam_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max)
          elseif (run_snowpack.eq.1.0 .and. multilayer_snowpack.eq.1
     &      .and. print_multilayer.eq.2.0) then
            write(401,rec=iter)
     &        ((real(KK(i,j)),i=1,nx),j=1,ny),
     &        ((snow_depth(i,j),i=1,nx),j=1,ny),
     &        ((xro_snow(i,j),i=1,nx),j=1,ny),
     &        ((swe_depth(i,j),i=1,nx),j=1,ny)
            write(402,rec=iter)
     &        (((snod_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max)
            write(403,rec=iter)
     &        (((ro_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max)
            write(404,rec=iter)
     &        (((swed_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max)
            write(405,rec=iter)
     &        (((diam_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max)
            write(406,rec=iter)
     &        (((flux_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max)
            write(407,rec=iter)
     &        (((T_old(i,j,k)-273.15,i=1,nx),j=1,ny),k=1,nz_max)
            write(408,rec=iter)
     &        (((gamma(i,j,k),i=1,nx),j=1,ny),k=1,nz_max)
          endif

c The call to outputs_user is available to provide user-defined
c   outputs.  These might be special-case situations, like just
c   writing out data at the end of every day, writing out a few
c   grid cells, saving each data arrays to individual files, etc.
          if (print_user.eq.1.0) then
            CALL OUTPUTS_USER(nx,ny,iter,Tair_grid,rh_grid,
     &        uwind_grid,vwind_grid,windspd_grid,winddir_grid,
     &        Qsi_grid,Qli_grid,prec_grid,Tsfc,Qle,Qh,Qe,Qc,Qm,Qf,
     &        e_balance,snow_depth,xro_snow,swe_depth,ro_nsnow,
     &        runoff,rain,sprec,sum_prec,sum_runoff,w_balance,
     &        snow_d,topo_land,wbal_qsubl,sum_sprec,wbal_salt,
     &        wbal_susp,ro_snow_grid,sum_Qcs,canopy_int,Qcs,
     &        iyear,imonth,iday,xhour,undef,deltax,xmn,ymn,
     &        wbal_subgrid,canopy_unload,sum_qsubl,sum_trans,
     &        sum_unload,sum_glacmelt,glacier_melt,swemelt,
     &        sfc_pressure,sum_swemelt,albedo,nrecs_max,
     &        icorr_factor_loop,swesublim,vegtype,iter_start,
     &        seaice_run,print_inc,cloud_frac_grid,
     &        output_path_wo_assim,output_path_wi_assim,print_var,
     &        print_outvars,Qsubl_depth,Qsalt,Qsusp)
          endif

c For multi-year simulations, sometimes it is desirable to zero
c   out the snow cover arrays on a certain summer date, to prevent
c   glaciers from forming.
          if (imonth.eq.iclear_mn .and. iday.eq.iclear_dy .and.
     &      xhour.eq.xclear_hr) then

            if (seaice_run.eq.4.0) then
              CALL ZERO_SNOW_SEAICE_4(nx,ny,snow_depth,ro_snow_grid,
     &          ro_snow,swe_depth,swe_depth_old,canopy_int_old,KK,
     &          sum_swemelt,tslsnowfall,snod_layer,swed_layer,
     &          ro_layer,T_old,sum_sprec,multilayer_snowpack,
     &          tsls_threshold,iyear,diam_layer,output_path_wo_assim)
            else
              CALL ZERO_SNOW(nx,ny,snow_depth,ro_snow_grid,ro_snow,
     &          swe_depth,swe_depth_old,canopy_int_old,KK,sum_swemelt,
     &          tslsnowfall,snod_layer,swed_layer,ro_layer,T_old,
     &          sum_sprec,multilayer_snowpack,tsls_threshold,
     &          sum_trans)
            endif

          endif

c Save the history restart information.
          if (ihrestart_flag.ge.-1) then
            if (mod(iter,ihrestart_inc).eq.0
     &        .or. iter.eq.max_iter) then
              CALL HRESTART_SAVE(nx,ny,iter,snow_d,snow_depth,
     &          canopy_int,soft_snow_d,ro_snow_grid,swe_depth,
     &          ro_soft_snow_old,snow_d_init,swe_depth_old,
     &          canopy_int_old,topo,sum_sprec,icorr_factor_loop,
     &          max_iter)
            endif
          endif

        enddo

      enddo

c Print a banner when the model run is finished.
        print *
        print *,
     & 'ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
        print *,
     & 'ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
        print *,
     & '                 The SnowModel Run Has Finished                '
        print *,
     & 'ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
        print *,
     & 'ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
        print *

      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c snowpack_code.f

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SNOWPACK_CODE(nx,ny,Tair_grid,rh_grid,ro_nsnow,
     &  dt,swe_depth,Tsfc,snow_d,prec_grid,runoff,Qm,rain,
     &  sprec,iter,w_balance,sum_prec,sum_runoff,xro_snow,
     &  undef,ro_snow,ro_snow_grid,soft_snow_d,sum_sprec,
     &  snow_depth,windspd_grid,Qsi_grid,sum_Qcs,canopy_int,
     &  Qcs,vegtype,forest_LAI,albedo,glacier_melt,
     &  canopy_unload,sum_unload,sum_glacmelt,run_snowtran,
     &  swemelt,d_canopy_int,sum_d_canopy_int,snow_d_init,
     &  sfc_pressure,Qe,sfc_sublim_flag,sum_sfcsublim,
     &  sum_swemelt,corr_factor,icorr_factor_index,swesublim,
     &  swe_depth_old,canopy_int_old,KK,max_layers,melt_flag,
     &  ro_snowmax,tsls_threshold,dz_snow_min,tslsnowfall,
     &  change_layer,snod_layer,swed_layer,ro_layer,T_old,gamma,
     &  multilayer_snowpack,seaice_run,seaice_conc,ht_windobs,
     &  windspd_2m_grid,diam_layer,flux_layer,sum_trans)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iter,i,j

      integer max_layers,multilayer_snowpack,k,n_tsteps_in_day,irec
      integer KK(nx_max,ny_max)
      integer melt_flag(nx_max,ny_max,nz_max)

      real ro_snowmax,tsls_threshold,dz_snow_min,Cp_snow
      real tslsnowfall(nx_max,ny_max)
      real change_layer(nx_max,ny_max)
      real snod_layer(nx_max,ny_max,nz_max)
      real swed_layer(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real gamma(nx_max,ny_max,nz_max)
      real diam_layer(nx_max,ny_max,nz_max)
      real flux_layer(nx_max,ny_max,nz_max)

      integer melt_flag_z(nz_max)
      real snod_layer_z(nz_max)
      real swed_layer_z(nz_max)
      real ro_layer_z(nz_max)
      real T_old_z(nz_max)
      real gamma_z(nz_max)
      real diam_z(nz_max)
      real flux_z(nz_max)

      real Tair_grid(nx_max,ny_max)
      real rh_grid(nx_max,ny_max)
      real prec_grid(nx_max,ny_max)
      real windspd_grid(nx_max,ny_max)
      real windspd_2m_grid(nx_max,ny_max)
      real Qsi_grid(nx_max,ny_max)
      real vegtype(nx_max,ny_max)
      real albedo(nx_max,ny_max)
      real glacier_melt(nx_max,ny_max)
      real canopy_unload(nx_max,ny_max)
      real sum_unload(nx_max,ny_max)
      real sum_glacmelt(nx_max,ny_max)
      real sum_swemelt(nx_max,ny_max)
      real swemelt(nx_max,ny_max)
      real swesublim(nx_max,ny_max)
      real snow_d_init(nx_max,ny_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real seaice_conc(nx_max,ny_max)
      real sum_trans(nx_max,ny_max)

      real ro_nsnow(nx_max,ny_max),snow_d(nx_max,ny_max),
     &  runoff(nx_max,ny_max),rain(nx_max,ny_max),
     &  sprec(nx_max,ny_max),w_balance(nx_max,ny_max),
     &  sum_prec(nx_max,ny_max),sum_runoff(nx_max,ny_max),
     &  xro_snow(nx_max,ny_max),sfc_pressure(nx_max,ny_max),
     &  ro_snow_grid(nx_max,ny_max),swe_depth(nx_max,ny_max),
     &  Tsfc(nx_max,ny_max),Qm(nx_max,ny_max),
     &  soft_snow_d(nx_max,ny_max),sum_sprec(nx_max,ny_max),
     &  ro_snow,snow_depth(nx_max,ny_max),sum_Qcs(nx_max,ny_max),
     &  canopy_int(nx_max,ny_max),Qcs(nx_max,ny_max),
     &  d_canopy_int(nx_max,ny_max),sum_d_canopy_int(nx_max,ny_max),
     &  Qe(nx_max,ny_max),sum_sfcsublim(nx_max,ny_max)

      real dt,undef,Cp,xLf,Tf,A1,A2,ro_water,xLs,ro_ice,Twb,
     &  run_snowtran,sfc_sublim_flag,seaice_run,ht_windobs

      real corr_factor(nx_max,ny_max,max_obs_dates+1)
      real corr_factor_ij
      integer icorr_factor_index(max_time_steps)

      integer nftypes
      parameter (nftypes=5)
      real forest_LAI(nftypes)

c     print *,'   solving the snow-cover evolution'

c Define the constants used in the computations.
      CALL CONSTS_SNOWPACK(Cp,xLs,ro_ice,xLf,Tf,A1,A2,ro_water,
     &  Cp_snow,ro_snowmax)

c Run the snowpack evolution sub-model.
      do j=1,ny
        do i=1,nx

c Extract the vertical column for this i,j point, and send it
c   to the subroutine. *** Note that I should use f95, then I would
c   not have to do this (I could pass in subsections of the arrays).
          if (multilayer_snowpack.eq.1) then
            do k=1,nz_max
              melt_flag_z(k) = melt_flag(i,j,k)
              snod_layer_z(k) = snod_layer(i,j,k)
              swed_layer_z(k) = swed_layer(i,j,k)
              ro_layer_z(k) = ro_layer(i,j,k)
              T_old_z(k) = T_old(i,j,k)
              gamma_z(k) = gamma(i,j,k)
              diam_z(k) = diam_layer(i,j,k)
            enddo
          endif

c Extract the correction factor from the data assimilation array
c   so it can be passed into the SnowPack routines without the
c   negative array index.
          if (icorr_factor_index(iter).lt.0) then
            k = -icorr_factor_index(iter)
            corr_factor_ij = corr_factor(i,j,k)
          else
            corr_factor_ij = undef
          endif

          CALL SNOWPACK_CORE(Twb,Tf,Tair_grid(i,j),rh_grid(i,j),xLs,
     &      Cp,sfc_pressure(i,j),ro_nsnow(i,j),dt,ro_snow,
     &      swe_depth(i,j),Tsfc(i,j),A1,A2,snow_d(i,j),ro_water,
     &      ro_ice,prec_grid(i,j),runoff(i,j),Qm(i,j),xLf,rain(i,j),
     &      sprec(i,j),iter,w_balance(i,j),sum_prec(i,j),
     &      sum_runoff(i,j),xro_snow(i,j),undef,
     &      soft_snow_d(i,j),sum_sprec(i,j),ro_snow_grid(i,j),
     &      snow_depth(i,j),windspd_grid(i,j),Qsi_grid(i,j),
     &      sum_Qcs(i,j),canopy_int(i,j),Qcs(i,j),vegtype(i,j),
     &      forest_LAI,albedo(i,j),canopy_unload(i,j),
     &      sum_unload(i,j),sum_glacmelt(i,j),run_snowtran,
     &      swemelt(i,j),d_canopy_int(i,j),sum_d_canopy_int(i,j),
     &      snow_d_init(i,j),Qe(i,j),glacier_melt(i,j),
     &      sfc_sublim_flag,sum_sfcsublim(i,j),sum_swemelt(i,j),
     &      corr_factor_ij,icorr_factor_index(iter),swesublim(i,j),
     &      swe_depth_old(i,j),canopy_int_old(i,j),KK(i,j),
     &      max_layers,melt_flag_z,ro_snowmax,tsls_threshold,
     &      dz_snow_min,tslsnowfall(i,j),change_layer(i,j),snod_layer_z,
     &      swed_layer_z,ro_layer_z,T_old_z,gamma_z,multilayer_snowpack,
     &      Cp_snow,seaice_run,ht_windobs,windspd_2m_grid(i,j),
     &      diam_z,flux_z)

c Re-build the 3-D arrays.  See note above about using f95 to avoid this.
          if (multilayer_snowpack.eq.1) then
            do k=1,nz_max
              melt_flag(i,j,k) = melt_flag_z(k)
              snod_layer(i,j,k) = snod_layer_z(k)
              swed_layer(i,j,k) = swed_layer_z(k)
              ro_layer(i,j,k) = ro_layer_z(k)
              T_old(i,j,k) = T_old_z(k)
              gamma(i,j,k) = gamma_z(k)
              diam_layer(i,j,k) = diam_z(k)
              flux_layer(i,j,k) = flux_z(k)
            enddo
          endif

        enddo
      enddo

      if (run_snowtran.eq.0.0) then
        do j=1,ny
          do i=1,nx
          swe_depth_old(i,j) = swe_depth(i,j)
          canopy_int_old(i,j) = canopy_int(i,j)
          enddo
        enddo
      endif

c Read in the sea ice concentration.  These are daily data, so
c   first calculate which record in the data file this time step
c   corresponds to.
      if (seaice_run.ne.0.0) then
        n_tsteps_in_day = nint(86400.0 / dt)
        if (mod(iter-1,n_tsteps_in_day).eq.0) then
          irec = int((real(iter) - 0.5) * dt / 86400.0) + 1
          print *,'sea ice irec =',irec
          read (445,rec=irec) ((seaice_conc(i,j),i=1,nx),j=1,ny)
        endif
      endif

c If this simulation is not running SnowTran-3D, then zero out
c   the ocean grid cells that have no sea ice here.  If it is
c   running with SnowTran-3D, then do this in the SnowTran-3D
c   subroutine.
      if (run_snowtran.eq.0.0) then
        if (seaice_run.ne.0.0) then
          CALL ZERO_SEAICE_SNOW(nx,ny,snow_depth,ro_snow_grid,
     &      ro_snow,swe_depth,swe_depth_old,canopy_int_old,KK,
     &      tslsnowfall,snod_layer,swed_layer,ro_layer,T_old,
     &      multilayer_snowpack,tsls_threshold,seaice_conc,
     &      sum_sprec,sum_trans)
        endif
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SNOWPACK_CORE(Twb,Tf,Tair,rh,xLs,
     &  Cp,sfc_pressure,ro_nsnow,dt,ro_snow,
     &  swe_depth,Tsfc,A1,A2,snow_d,ro_water,
     &  ro_ice,prec,runoff,Qm,xLf,rain,
     &  sprec,iter,w_balance,sum_prec,
     &  sum_runoff,xro_snow,undef,
     &  soft_snow_d,sum_sprec,ro_snow_grid,
     &  snow_depth,windspd,Qsi,
     &  sum_Qcs,canopy_int,Qcs,vegtype,
     &  forest_LAI,albedo,canopy_unload,
     &  sum_unload,sum_glacmelt,run_snowtran,
     &  swemelt,d_canopy_int,sum_d_canopy_int,
     &  snow_d_init,Qe,glacier_melt,
     &  sfc_sublim_flag,sum_sfcsublim,sum_swemelt,
     &  corr_factor,icorr_factor_index,swesublim,
     &  swe_depth_old,canopy_int_old,KK,
     &  max_layers,melt_flag,ro_snowmax,tsls_threshold,
     &  dz_snow_min,tslsnowfall,change_layer,snod_layer,
     &  swed_layer,ro_layer,T_old,gamma,multilayer_snowpack,
     &  Cp_snow,seaice_run,ht_windobs,windspd_2m,
     &  diam_layer,flux_layer)

      implicit none

      include 'snowmodel.inc'

      integer iter,icorr_factor_index

      integer KK,max_layers,multilayer_snowpack
      
      real ro_snowmax,tsls_threshold,dz_snow_min,tslsnowfall,Cp_snow

      integer melt_flag(nz_max)
      real change_layer
      real snod_layer(nz_max)
      real swed_layer(nz_max)
      real ro_layer(nz_max)
      real T_old(nz_max)
      real gamma(nz_max)
      real diam_layer(nz_max)
      real flux_layer(nz_max)

      real Twb,Tf,Tair,rh,xLs,Cp,ro_nsnow,dt,ro_snow,swe_depth,
     &  Tsfc,A1,A2,snow_d,ro_water,ro_ice,prec,runoff,Qm,xLf,rain,
     &  sprec,w_balance,sum_prec,sum_runoff,xro_snow,undef,
     &  soft_snow_d,sum_sprec,ro_snow_grid,snow_depth,sprec_grnd,
     &  windspd,Qsi,sum_Qcs,canopy_int,Qcs,canopy_unload,
     &  vegtype,albedo,glacier_melt,sum_unload,sum_glacmelt,
     &  run_snowtran,swemelt,d_canopy_int,sfc_pressure,
     &  sum_d_canopy_int,snow_d_init,Qe,sfc_sublim_flag,
     &  sum_sfcsublim,sum_swemelt,corr_factor,swesublim,
     &  swe_depth_old,canopy_int_old,sprec_grnd_ml,seaice_run,
     &  ro_nsnow_wind,ht_windobs,windspd_2m

      integer nftypes
      parameter (nftypes=5)
      real forest_LAI(nftypes)

c Calculate the canopy sublimation, loading and unloading.  Note
c   that here I have assumed that trees are type 1-5.
      if (vegtype.le.5.0) then
        CALL CANOPY_SNOW(rh,Tair,windspd,Qsi,sum_Qcs,albedo,
     &    canopy_int,sprec,Qcs,dt,canopy_unload,
     &    forest_LAI(nint(vegtype)),sum_unload,d_canopy_int,
     &    sum_d_canopy_int)
        sprec_grnd = sprec + canopy_unload - d_canopy_int
        sprec_grnd_ml = sprec - d_canopy_int
      else
        Qcs = 0.0
        sprec_grnd = sprec
        sprec_grnd_ml = sprec
      endif

c Calculate the wind speed at 2 meters.
      CALL WINDSPEED_2M(windspd,ht_windobs,windspd_2m)

c Solve for the wet bulb temperature.
      CALL SOLVEWB(Twb,Tf,Tair,rh,xLs,Cp,sfc_pressure)

c Compute the new snow density.
      CALL NSNOWDEN(ro_nsnow,Twb,Tf,dt)

c Call the subroutine that increases the snow density due to
c   blowing snow wind speeds during a snowfall event.  Here we
c   are only calculating a increment to the previous NSNOWDEN
c   new snow density calculation.
      CALL NSNOW_DENSITY_FROM_BLOWING_SNOW(windspd_2m,sprec,dt,
     &  ro_nsnow_wind)

c Update the new snow density with the wind contribution.
      ro_nsnow = ro_nsnow + ro_nsnow_wind

c Make sure the snow density falls within reasonable limits.
      ro_nsnow = min(ro_nsnow,ro_snowmax)

c Call the multi-layer snowpack model.
      if (multilayer_snowpack.eq.1) then

        CALL MULTI_LAYER_SNOW(KK,ro_layer,Tf,dt,ro_water,
     &    ro_ice,T_old,snod_layer,swed_layer,Qm,ro_snowmax,rain,
     &    xLf,Cp_snow,melt_flag,runoff,tslsnowfall,ro_nsnow,
     &    sprec,Tsfc,tsls_threshold,gamma,max_layers,change_layer,
     &    dz_snow_min,snow_depth,swe_depth,undef,canopy_unload,
     &    vegtype,glacier_melt,sum_glacmelt,sum_swemelt,snow_d,
     &    Qe,sfc_sublim_flag,sum_sfcsublim,soft_snow_d,ro_snow,
     &    sum_sprec,sprec_grnd_ml,sum_prec,prec,sum_runoff,
     &    ro_snow_grid,xro_snow,swesublim,A1,A2,windspd_2m,
     &    sfc_pressure,diam_layer,flux_layer,corr_factor,
     &    icorr_factor_index)

c Call the original single-layer snowpack model.
      else

c Compute the snow density change due to settling.
        CALL DDENSITY(ro_snow_grid,swe_depth,Tf,Tsfc,dt,A1,A2,
     &    snow_depth,ro_water,ro_snowmax)

c Compute the melt, rain, and snow contributions to modifying
c   the snowpack depth, density, and snow water equivalent.
        CALL SNOWPACK(swe_depth,snow_d,ro_snow_grid,
     &    prec,ro_water,ro_nsnow,runoff,Qm,xLf,dt,rain,sprec,
     &    sum_prec,sum_runoff,soft_snow_d,sum_sprec,ro_snow,
     &    snow_depth,sprec_grnd,vegtype,glacier_melt,sum_glacmelt,
     &    swemelt,canopy_unload,Qe,sfc_sublim_flag,sum_sfcsublim,
     &    sum_swemelt,corr_factor,icorr_factor_index,swesublim,
     &    ro_snowmax)

c Post process the data for output.
        CALL POSTPROC(ro_snow_grid,xro_snow,snow_depth,undef)

      endif

c Perform a water balance check (see notes in this subroutine).
      if (seaice_run.eq.0.0) then
        if (run_snowtran.eq.0.0) then
          CALL WATERBAL_SNOWPACK(w_balance,prec,Qcs,runoff,
     &    d_canopy_int,swe_depth,glacier_melt,swe_depth_old,iter,
     &    swesublim,canopy_unload,canopy_int_old,canopy_int)
        endif
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CANOPY_SNOW(rh,Tair,windspd,Qsi,sum_Qcs,albedo,
     &  canopy_int,sprec,Qcs,dt,canopy_unload,
     &  forest_LAI,sum_unload,d_canopy_int,
     &  sum_d_canopy_int)

      implicit none

      real rh,Tair,windspd,V_s,Qsi,forest_LAI,dt,xImax,canopy_int,
     &  d_canopy_int,Qcs,Ce,sprec,C_0,unload_melt,canopy_unload,
     &  sum_Qcs,albedo,sum_unload,sum_d_canopy_int

c Note that all of this must deal with the (kg/m2)=(mm), => (m)
c   issues.  Precip is in (m), all of these equations are in
c   (kg/m2), and I want the outputs to be in (m).

c Compute the sublimation loss rate coefficient for canopy snow.
      CALL SUBLIM_COEF(rh,Tair,windspd,V_s,Qsi,albedo)

c Maximum interception storage.
      xImax = 4.4 * forest_LAI

c Change in canopy load due to snow precipitation during this time
c   step.  Convert the canopy interception to mm.
      canopy_int = 1000.0 * canopy_int
      d_canopy_int = 0.7 * (xImax - canopy_int) *
     &  ( 1.0 - exp((- sprec)*1000.0/xImax))

c Update the interception load.
      canopy_int = canopy_int + d_canopy_int

c Canopy exposure coefficient.
      if (canopy_int.eq.0.0) then
        Ce = 0.0
      else
c Pomeroy's k_c value
c       Ce = 0.0114 * (canopy_int/xImax)**(-0.4)
c My k_c value.
        Ce = 0.00995 * (canopy_int/xImax)**(-0.4)
      endif

c Canopy sublimation (kg/m2), (a negative mumber).  Make sure that
c   you don't sublimate more than is available.
      Qcs = Ce * canopy_int * V_s * dt
      Qcs = -min(canopy_int,-Qcs)

c Remove the sublimated moisture from the canopy store.
      canopy_int = canopy_int + Qcs

c Save the sublimation in (m).
      Qcs = Qcs / 1000.0
      sum_Qcs = sum_Qcs + Qcs

c Perform a second unloading due to melt.  Assume an unloading rate
c   of 5.0 mm/day/C.
      C_0 = 5.0 / 86400.0
      unload_melt = C_0 * max(0.0,Tair-273.15) * dt
      unload_melt = min(canopy_int,unload_melt)
      canopy_int = canopy_int - unload_melt

c Keep track of the unloaded snow that reached the ground during
c   this time step (m) (this will add to the snow depth).
      canopy_unload = unload_melt / 1000.0
      d_canopy_int = d_canopy_int / 1000.0

c Save a summing array of this unloaded snow.
      sum_unload = sum_unload + canopy_unload

c Save a summing array of the change in canopy load.
      sum_d_canopy_int = sum_d_canopy_int + d_canopy_int

c Save the interception load for the next time step.  Convert to m.
      canopy_int = canopy_int / 1000.0

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SUBLIM_COEF(rh,Tair,windspd,V_s,Qsi,albedo)

c Compute the sublimation loss rate coefficient for canopy snow.

      implicit none

      real pi,ro_ice,xM,R,R_dryair,vonKarman,visc_air,h_s,xlamdaT,
     &  D,ro_sat,sigma,V_s,radius,xmass,windspd,rh,Tair,Qsi,Sp,
     &  xN_r,xNu,xSh,top,bottom,omega,albedo

c Constants.
      pi = 2.0 * acos(0.0)
      ro_ice = 917.0
      xM = 18.01
      R = 8313.
      R_dryair = 287.
      vonKarman = 0.4
      visc_air = 13.e-6
      h_s = 2.838e6
      xlamdaT = 0.024

c Particle radius.
      radius = 5.0e-4

c Particle mass.
      xmass = 4.0/3.0 * pi * ro_ice * radius**3

c Diffusivity of water vapor in the atmosphere.
      D = 2.06e-5 * (Tair/273.0)**(1.75)

c Saturation density of water vapor.
      ro_sat = 0.622 / (R_dryair * Tair) *
     &  611.15 * exp(22.452 * (Tair - 273.15) / (Tair - 0.61))

c Humidity deficit.
      sigma = 0.01 * rh - 1.0
      sigma = min(0.0,sigma)
      sigma = max(-1.0,sigma)

c Reynolds, Nusselt, and Sherwood numbers.
      xN_r = 2.0 * radius * windspd / visc_air
      xNu = 1.79 + 0.606 * xN_r**(0.5)
      xSh = xNu

c Solar radiation absorbed by the snow particle.  Here assume that
c   the general snow albedo is the same as the snow particle albedo.
      Sp = pi * radius**2 * (1.0 - albedo) * Qsi

c Sublimation-loss rate coefficient for an ice sphere.
      omega = ((h_s * xM)/(R * Tair) - 1.0) / (xlamdaT * Tair * xNu)
      top = 2.0 * pi * radius * sigma - Sp * omega
      bottom = h_s * omega + 1.0/(D * ro_sat * xSh)
      V_s = (top/bottom)/xmass

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE WATERBAL_SNOWPACK(w_balance,prec,Qcs,runoff,
     &  d_canopy_int,swe_depth,glacier_melt,swe_depth_old,iter,
     &  swesublim,canopy_unload,canopy_int_old,canopy_int)

      implicit none

      integer iter

      real w_balance,prec,Qcs,runoff,d_canopy_int,swe_depth_old,
     &  swe_depth,glacier_melt,swesublim,canopy_unload,canopy_int_old,
     &  canopy_int

c Note that the following balances should hold.  These aren't quite
c   right, but it is a place to start.
c   Canopy Balance (forest):
c     canopy = sprec - unload + Qcs ==> unload = sprec - canopy + Qcs
c
c   Snowpack Balance (forest):
c     swe_d = unload + rain - runoff ==>
c       canopy + swe_d = sprec + rain + Qcs - runoff
c     prec = sprec + rain
c     sum_rain  = sum_sprec - sum_prec
c
c   Snowpack Balance (non-forest):
c     swe_d = sprec + rain - runoff + subl + salt + susp + subgrid +
c       glaciermelt
c
c   Everywhere:
c     w_balance = sum_prec + sum_Qcs - sum_runoff + sum_subl +
c       sum_trans - canopy_int - swe_depth + sum_glacmelt
c
c   The related variables that would need to be brought in are:
c      d_canopy_int,sum_d_canopy_int,sum_unload

c This subroutine is called for the case where SnowTran-3D is not
c   run.  The subroutine WATERBAL_SNOWTRAN is used if the model
c   simulation includes SnowTran-3D.
c     w_balance = swe_depth_old - swe_depth + prec - runoff +
c    &  glacier_melt - swesublim + canopy_int_old - canopy_int -
c    &  d_canopy_int + Qcs + canopy_unload

c Do the snowpack.
c     w_balance = swe_depth_old - swe_depth + prec - runoff -
c    &  glacier_melt - swesublim

c Do the canopy.
c     w_balance = canopy_int_old - canopy_int + d_canopy_int +
c    &  Qcs - canopy_unload

c Do the snowpack and canopy store.
      w_balance = swe_depth_old - swe_depth + prec - runoff +
     &  glacier_melt - swesublim + canopy_int_old - canopy_int +
     &  Qcs

      if (abs(w_balance).gt.1.0e-5)
     &  print*,'water imbalance found, iter =',iter,' ',w_balance

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SNOWPACK(swe_depth,snow_d,ro_snow_grid,
     &  prec,ro_water,ro_nsnow,runoff,Qm,xLf,dt,rain,sprec,
     &  sum_prec,sum_runoff,soft_snow_d,sum_sprec,ro_snow,
     &  snow_depth,sprec_grnd,vegtype,glacier_melt,sum_glacmelt,
     &  swemelt,canopy_unload,Qe,sfc_sublim_flag,sum_sfcsublim,
     &  sum_swemelt,corr_factor,icorr_factor_index,swesublim,
     &  ro_snowmax)

      implicit none

      real ro_snowmax,runoff,Qm,swe_depth,potmelt,swemelt,dt,
     &  ro_water,xLf,snow_depth,ro_snow_grid,snow_d_melt,dz_water,
     &  soft_snow_d,prec,rain,snow_d,sum_sprec,sum_prec,
     &  sum_runoff,ro_nsnow,sprec,ro_snow,snow_d_new,sprec_grnd,
     &  vegtype,glacier_melt,sum_glacmelt,canopy_unload,Qe,
     &  xLsublim,potsublim,swesublim,snow_d_sublim,sfc_sublim_flag,
     &  sum_sfcsublim,sum_swemelt,corr_factor,potmelt_tmp
      integer icorr_factor_index

      runoff = 0.0

c SURFACE SUBLIMATION.

c Whether static-surface (non-blowing snow) sublimation is included
c   in the model calculations is controlled by the sfc_sublim_flag.
c   I am waiting for the flux-tower data Matthew and I are collecting
c   in Alaska, to compare with the model simulations, before
c   including this part of the model in all simulations.

c If the sfc_sublim_flag is turned on, the latent heat flux (Qe)
c   calculated in ENBAL is used to add/remove snow from the snowpack.
c   xLsublim = xLf + xLv = 2.5104x10^6 J/kg + 3.334x10^5 J/kg, and
c   potsublim is in m swe.

      if (swe_depth.gt.0.0  .and.  sfc_sublim_flag.eq.1.0) then
        if (Qe.lt.0.0) then

c Compute the snow-surface sublimation (m, swe).
          xLsublim = 2.844e6
          potsublim = (- dt) * Qe / (ro_water * xLsublim)
          swesublim = min(potsublim,swe_depth)

c Save a summing array of the static surface snow sublimation.
          sum_sfcsublim = sum_sfcsublim + swesublim

c Compute the change in snow depth.  Assume that this sublimated
c   snow does not change the snow density and does not change the
c   soft snow depth.  It only reduces the snow depth and the
c   associated swe depth.
          swe_depth = swe_depth - swesublim
          if (swe_depth.eq.0.0) then
            snow_depth = 0.0
          else
            snow_d_sublim = swesublim * ro_water / ro_snow_grid
            snow_depth = snow_depth - snow_d_sublim
          endif
        else
          swesublim = 0.0
        endif
      else
        swesublim = 0.0
      endif

c MELTING.

c If melting occurs, decrease the snow depth, and place the melt
c   water in the 'runoff' variable.  Keep track of the liquid water
c   produced.

      if (Qm.gt.0.0) then

c Compute the snow melt (m).
        potmelt = dt * Qm / (ro_water * xLf)

c Account for any snowmelt data assimilation.
        if (icorr_factor_index.lt.0) then
          potmelt_tmp = potmelt * corr_factor
          swemelt = min(potmelt_tmp,swe_depth)
c Handle the case of no snowmelt data assimilation.
        else
          swemelt = min(potmelt,swe_depth)
        endif

c Compute any glacier or permanent snow-field melt (m water equiv.).
        if (vegtype.eq.20.0) then
          glacier_melt = potmelt - swemelt
        else
          glacier_melt = 0.0
        endif

c Save a summing array of the glacier melt.
        sum_glacmelt = sum_glacmelt + glacier_melt

c Save the runoff contribution.
        runoff = runoff + glacier_melt

c Save a summing array of the snow melt.
        sum_swemelt = sum_swemelt + swemelt

c Compute the change in snow depth.
        snow_d_melt = swemelt * ro_water / ro_snow_grid
        snow_depth = snow_depth - snow_d_melt
        snow_depth = max(0.0,snow_depth)

c Compute the changes in snow density resulting from the melt.
c   Assume that the melted snow is redistributed through the new
c   snow depth up to a maximum density.  Any additional melt water
c   is added to the runoff.
        if (snow_depth.eq.0.0) then
          ro_snow_grid = ro_snowmax
          runoff = runoff + swemelt
        else
          ro_snow_grid = swe_depth * ro_water / snow_depth
        endif

        if (ro_snow_grid.gt.ro_snowmax) then
          dz_water = snow_depth *
     &      (ro_snow_grid - ro_snowmax) / ro_water
          ro_snow_grid = ro_snowmax
          swe_depth = snow_depth * ro_snow_grid / ro_water
          runoff = runoff + dz_water
        else
          swe_depth = snow_depth * ro_snow_grid / ro_water
        endif

        soft_snow_d = 0.0

      else

c These prevent values from the previous time step from being
c   carried through to the next time step.
        swemelt = 0.0
        glacier_melt = 0.0

      endif

c PRECIPITATION.

c Precipitation falling as rain on snow contributes to a snow
c   density increase, precipitation falling as snow adds to the
c   snow depth, and rain falling on bare ground contributes to the
c   runoff.  This latest version of this section follows Justin
c   Pflug's code updates.

c We have precipitation.
      if (prec.gt.0.0) then
        rain = prec - sprec

c If there is snow on the ground, all of the precipitation (rain
c   and snowfall) is added to the snowpack.  If there is no
c   snowpack, then snowfall builds a new snowpack, and rain goes
c   into runoff.
        if (snow_depth.gt.0.0) then
          swe_depth = swe_depth + rain + sprec_grnd
        else
          swe_depth = sprec_grnd
          runoff = runoff + rain
        endif

c Update the new snow depth, the total snow depth, and the snow
c   density.
        snow_d_new = ro_water / ro_nsnow * sprec_grnd
        snow_depth = snow_depth + snow_d_new
        if (snow_depth.gt.0.0) then
          ro_snow_grid = ro_water * swe_depth / snow_depth
        endif

c If the density threshold is exceeded, adjust that and place the
c   excess moisture in the runoff array.
        if (ro_snow_grid.gt.ro_snowmax) then
          dz_water = snow_depth * (ro_snow_grid - ro_snowmax) / ro_water
          ro_snow_grid = ro_snowmax
          swe_depth = snow_depth * ro_snow_grid / ro_water
          runoff = runoff + dz_water
        endif

c Here we handle the case where there is no precipitation, but
c   there is snow falling from the canopy to the snowpack.
      else
        rain = 0.0
        if (sprec_grnd.gt.0.0) then
          swe_depth = swe_depth + sprec_grnd
          snow_d_new = ro_water / ro_snow * sprec_grnd
          snow_depth = snow_depth + snow_d_new
          ro_snow_grid = ro_water * swe_depth / snow_depth
        endif
      endif

c The following are set up to be compatible with SnowTran-3D, and
c   are in snow-depth units.  The sum_sprec corrections are done
c   in the SnowTran-3D code.

c Assume any rain sets the soft snow depth to zero.
      if (rain.eq.0.0) then
        soft_snow_d = soft_snow_d + sprec_grnd * ro_water / ro_snow
      else
        soft_snow_d = 0.0
      endif

      snow_d = swe_depth * ro_water / ro_snow
      sum_sprec = sum_sprec + sprec_grnd

c The following are in swe-depth units.
      sum_prec = sum_prec + prec
      sum_runoff = sum_runoff + runoff

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE DDENSITY(ro_snow_grid,swe_depth,Tf,Tsfc,dt,A1,A2,
     &  snow_depth,ro_water,ro_snowmax)

      implicit none

      real snow_depth,Tsg,Tf,Tsnow,Tsfc,ro_snow_grid,dt,A1,A2,
     &  swe_depth_star,ro_snowmax,ro_water,swe_depth,ro_adjust

c ro_adjust is a snow density rate adjustment factor that can be
c   used to make the snow density increase faster (ro_adjust > 1.0)
c   or slower (ro_adjust < 1.0).
      ro_adjust = 5.0

      if (snow_depth.gt.0.0) then

c Assume that the snow-ground interface temperature is -1.0 C.
        Tsg = Tf - 1.0
        Tsnow = 0.5 * (Tsg + Tsfc)
        swe_depth_star= 0.5 * swe_depth
        ro_snow_grid = ro_snow_grid + ro_adjust * dt *
     &    (A1 * swe_depth_star * ro_snow_grid *
     &    exp((- 0.08)*(Tf-Tsnow)) * exp((- A2)*ro_snow_grid))
        ro_snow_grid = min(ro_snowmax,ro_snow_grid)
        snow_depth = ro_water * swe_depth / ro_snow_grid

      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SOLVEWB(xnew,Tf,Tair,rh,xLs,Cp,sfc_pressure)

      implicit none

      real A,B,C,ea,rh,Tair,Tf,tol,old,fprime,xLs,Cp,funct,xnew,
     &  sfc_pressure

      integer maxiter,i

c Coeffs for saturation vapor pressure over water (Buck 1981).
c   Note: temperatures for Buck`s equations are in deg C, and
c   vapor pressures are in mb.  Do the adjustments so that the
c   calculations are done with temperatures in K, and vapor
c   pressures in Pa.

c Over water.
        A = 6.1121 * 100.0
        B = 17.502
        C = 240.97
c Over ice.
c       A = 6.1115 * 100.0
c       B = 22.452
c       C = 272.55

c Atmospheric vapor pressure from relative humidity data.
      ea = rh / 100.0 * A * exp((B * (Tair - Tf))/(C + (Tair - Tf)))

c Solve for the wet bulb temperature.
      tol = 1.0e-2
      maxiter = 20
      old = Tair

      do i=1,maxiter
        fprime = 1.0 + xLs/Cp * 0.622/sfc_pressure * log(10.0) *
     &    2353. * (10.0**(11.40 - 2353./old)) / old**2
        funct = old - Tair + xLs/Cp * 0.622/sfc_pressure *
     &    (10.0**(11.40-2353./old) - ea)
        xnew = old - funct/fprime
        if (abs(xnew - old).lt.tol) return
        old = xnew
      end do

c If the maximum iterations are exceeded, send a message and set
c   the wet bulb temperature to the air temperature.
      write (*,102)
  102 format('max iteration exceeded when solving for Twb')
      xnew = Tair

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE NSNOWDEN(ro_nsnow,Twb,Tf,dt)

      implicit none

      real Twgmax,Tf,Twb,ro_nsnow,scalefact,dt,wt

      Twgmax = Tf + 1.0
      if (Twb.ge.258.15 .and. Twb.le.Twgmax) then
        ro_nsnow = 50. + 1.7 * (Twb - 258.15)**1.5
      elseif (Twb.lt.258.15) then
        ro_nsnow = 50.0
      else
        ro_nsnow = 158.8
      endif

c For one day time steps, this equation gives a new snow density at
c   the end of the 24 hour period which is too low, by an approximate
c   factor of X.  Thus, for a daily time step, I scale the density by
c   X before returning it to the main program.

      scalefact = 1.0
      if (dt.eq.86400.0) then
        if (ro_nsnow.le.158.8) then
          wt = 1.0 + (50.0 - ro_nsnow) / 108.8
          ro_nsnow = wt * scalefact * ro_nsnow + ro_nsnow
          ro_nsnow = min(158.8,ro_nsnow)
        endif
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE POSTPROC(ro_snow_grid,xro_snow,snow_depth,undef)

      implicit none

      real snow_depth,xro_snow,undef,ro_snow_grid

      if (snow_depth.eq.0.0) then
        xro_snow = undef
      else
        xro_snow = ro_snow_grid
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CONSTS_SNOWPACK(Cp,xLs,ro_ice,xLf,Tf,A1,A2,ro_water,
     &  Cp_snow,ro_snowmax)

      implicit none

      real Cp,xLs,ro_ice,xLf,Tf,A1,A2,ro_water,Cp_snow,ro_snowmax

      Cp = 1004.
      xLs = 2.500e6
      ro_ice = 917.0
      xLf = 3.34e5
      Tf = 273.15
      A1 = 0.0013
      A2 = 0.021
      ro_water = 1000.0
      Cp_snow = 2106.
      ro_snowmax = 550.0

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MULTI_LAYER_SNOW(KK,ro_layer,Tf,dt,ro_water,
     &  ro_ice,T_old,snod_layer,swed_layer,Qm,ro_snowmax,rain,
     &  xLf,Cp_snow,melt_flag,runoff,tslsnowfall,ro_nsnow,
     &  sprec,Tsfc,tsls_threshold,gamma,max_layers,change_layer,
     &  dz_snow_min,snow_depth,swe_depth,undef,canopy_unload,
     &  vegtype,glacier_melt,sum_glacmelt,sum_swemelt,snow_d,
     &  Qe,sfc_sublim_flag,sum_sfcsublim,soft_snow_d,ro_snow,
     &  sum_sprec,sprec_grnd_ml,sum_prec,prec,sum_runoff,
     &  ro_snow_grid,xro_snow,swesublim,A1,A2,windspd_2m,
     &  sfc_pressure,diam_layer,flux_layer,corr_factor,
     &  icorr_factor_index)

      implicit none

      include 'snowmodel.inc'

      integer KK,max_layers,k
      integer melt_flag(nz_max)

      real snod_layer(nz_max)
      real swed_layer(nz_max)
      real ro_layer(nz_max)
      real T_old(nz_max)
      real gamma(nz_max)
      real diam_layer(nz_max)
      real flux_layer(nz_max)
      real frac_liq(nz_max)

      real Tf,dt,ro_water,ro_ice,Qm,ro_snowmax,rain,xLf,Cp_snow,
     &  runoff,tslsnowfall,ro_nsnow,sprec,Tsfc,tsls_threshold,
     &  dz_snow_min,snow_depth,swe_depth,undef,change_layer,
     &  canopy_unload,vegtype,glacier_melt,sum_glacmelt,sum_swemelt,
     &  soft_snow_d,Qe,sfc_sublim_flag,sum_sfcsublim,snow_d,
     &  ro_snow,sum_sprec,sprec_grnd_ml,sum_prec,prec,sum_runoff,
     &  ro_snow_grid,xro_snow,swesublim,A1,A2,windspd_2m,
     &  sfc_pressure,corr_factor

      integer icorr_factor_index

c THIS IS THE MULTI-LAYER SNOWPACK MODEL.

c Note there is some confusion with the dy - dz notation used here.
c   In the multi-layer code 'y' is the vertical coordinate.  This is
c   a hold-over from a time when my temperature solution code had
c   y going up-down.

c Compute the snow density change due to compaction and the impact
c   of wind on the top snow layer.
      CALL DDENSITY_ML(ro_layer,Tf,dt,ro_water,ro_snowmax,
     &  T_old,KK,snod_layer,A1,A2,windspd_2m)

c Calculate the rainfall from prec and sprec.
      if (prec.gt.0.0) then
        rain = prec - sprec
      else
        rain = 0.0
      endif

c Distribute surface melt and rain precipitation through the snowpack.
      CALL MELT_SNOW_ML(KK,swed_layer,ro_water,ro_layer,Qm,dt,
     &  snod_layer,ro_snowmax,rain,xLf,Cp_snow,Tf,T_old,melt_flag,
     &  runoff,canopy_unload,swe_depth,snow_depth,vegtype,
     &  glacier_melt,sum_glacmelt,sum_swemelt,soft_snow_d,Qe,
     &  sfc_sublim_flag,sum_sfcsublim,swesublim,ro_snow,
     &  corr_factor,icorr_factor_index)

c Account for the accumulation of snow precipitation on the snowpack.
      CALL PRECIP_ML(KK,ro_layer,snod_layer,ro_water,tslsnowfall,
     &  swed_layer,ro_nsnow,T_old,Tsfc,tsls_threshold,dt,
     &  melt_flag,soft_snow_d,ro_snow,sum_sprec,sprec_grnd_ml,
     &  sum_prec,prec,sum_runoff,runoff,snow_d,snow_depth,swe_depth,
     &  diam_layer)

c Merge layers if the number of layers exceeds some maximum number of
c   layers or if a layer gets thinner than some minimum thickness.
      CALL MERGE_LAYERS_ML(KK,ro_layer,snod_layer,swed_layer,
     &  T_old,ro_water,max_layers,change_layer,dz_snow_min,melt_flag,
     &  diam_layer)

c The grain-growth model can account for the liquid fraction in the
c   snow.  The model deals with three different wetness conditions
c   differently:
c     frac_liq < 1.0e-4 is considered dry
c     1.0e-4 <= frac_liq < 0.09 is considered wet
c     frac_liq >= 0.09 is considered very wet
c Note that there is also a different vapor diffusion threshold
c   that is also used (0.02).
c I have not implemented these yet, and for now assume that the
c   snow is dry.
      do k=1,KK
        frac_liq(k) = 0.0
      enddo

c Update the grain size using the SNTHERM equations.
      CALL GET_GRAIN_SIZE_SNTHERM(KK,dt,ro_layer,undef,
     &  diam_layer,frac_liq,T_old,snod_layer,Tf,sfc_pressure,
     &  flux_layer)

c Calculate the temperature of each snow layer.
      CALL SNOWTEMP_ML(gamma,T_old,Tsfc,KK,dt,ro_layer,Cp_snow,
     &  Tf,snod_layer,melt_flag,diam_layer)

c Postprocess the data.
      CALL POST_PROC_ML(KK,snod_layer,snow_depth,swe_depth,undef,
     &  swed_layer,gamma,ro_layer,melt_flag,T_old,Tf,ro_snow_grid,
     &  ro_water,xro_snow,ro_snow,diam_layer)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GET_GRAIN_SIZE_SNTHERM(nlayers,dt,ro_layer,undef,
     &  diam_layer,frac_liq,T_layer,dz,Tf,press,
     &  flux_layer)

c Someday I need to make the above terminology consistent with the
c   rest of the code (like that below).
c     SUBROUTINE GET_GRAIN_SIZE_SNTHERM(KK,dt,ro_layer,diam_layer,
c    &  frac_liq,T_old,snod_layer,Tf,sfc_pressure)

c This code follows Jordan 1991 (Tech Doc for SNTHERM.89; this is
c   referenced as "SNTH89" in the comments below).  The equations
c   that are solved are those in the original SNTHERM89 code.

c De0 : Effective diffusion coefficient for water vapor in
c       snow (m^2/s) at 100000 Pa and 273.15K
c press : Barometric pressure (Pa)
c dc(nlayers) : Vapor diffusion coefficient for = de*change in
c               saturation vapor pressure with temperature
c vapor_flux(nlayers) : Average diffusive flux at nodal centroid
c                       [(upper+lower)/2] (kg/m^2 s)
c T_layer(nlayers) : Temperature in center (node) of layer (K)
c g1 : Grain growth constant for dry snow (5.0e-7)
c g2 : Grain growth constant for wet snow (4.0e-12)
c diam : Nodal effective grain diameter (m)
c frac_liq : Volume fraction of liquid water
c dt : Time step (s)
c e0 : Saturation vapor pressure at 0 degrees C (mb)
c Rw : Gas constant for water vapor (461.5) (j/kg-K)
c ro_ice : Ice density (917.0) (kg/m^3)
c ro_snow(nlayers) : Nodal bulk snow density (kg/m^3)

      implicit none

      include 'snowmodel.inc'

c     integer k,KK
      integer nlayers,k,nzm

      real diam_layer(nz_max)
      real flux_layer(nz_max)
      real ro_layer(nz_max)
      real T_layer(nz_max)
      real frac_liq(nz_max)
      real dz(nz_max)
      real porosity(nz_max)
      real diff_coef(nz_max)
      real C_kT(nz_max)
      real vapor_flux(nz_max)

c These are the values at the control volume boundaries.
      real T_layer_bndry(nz_max+1)
      real dz_bndry(nz_max+1)
      real diff_coef_bndry(nz_max+1)

      real g1,g2,ro_ice,De0,Des,dt,Tf,press,grain_scale,pi,
     &  xLvi_o_Rw,c1i,xLvw_o_Rw,c1w,vaporvol,Uv_bot,Uv_top,
     &  diam_mm,undef

c     real e0,Rw,xLvw,xLvi

      data g1 /5.0e-7/
      data g2 /4.0e-12/
      data ro_ice /917.0/
      data De0 /9.2e-5/

c Calculate the vapor diffusion constants for water(w) and ice(i).
c   These are constants, so I just save the values as data values.
c     data e0 /613.6/
c     data Rw /461.5/
c     data xLvw /2.505e6/
c     data xLvi /2.838e6/
c     xLvw_o_Rw = xLvw / Rw
c     c1w = e0 * exp(xLvw_o_Rw / Tf) / Rw
c     xLvi_o_Rw = xLvi / Rw
c     c1i = e0 * exp(xLvi_o_Rw / Tf) / Rw
c     print *,xLvw_o_Rw
c     print *,c1w
c     print *,xLvi_o_Rw
c     print *,c1i

      data xLvw_o_Rw /5427.952/
      data c1w /5.6739e8/
      data xLvi_o_Rw /6149.513/
      data c1i /7.9639e9/

      pi = 2.0 * acos(0.0)

c This 'grain_scale' controls allows the user additional control
c   on the grain-growth calculations.  Values greater than 1.0
c   grows grains faster, and values less than 1.0 grows grains
c   slower.
c     data grain_scale /1.0/
c     data grain_scale /0.5/
      data grain_scale /2.5/

c Initialize the flux output variable so the no-layer outputs make
c   sense.
      do k=1,nz_max
        flux_layer(k) = undef
      enddo

c Define the snow porosity.
      do k=1,nlayers
        porosity(k) = 1.0 - (ro_layer(k) / ro_ice)

c The only place this value should have problems is if ro_layer is
c   undef.  It should be well-constrained before getting to this
c   subroutine.
        if (porosity(k) .lt. 0.0) porosity(k) = 0.0
      enddo

c Diffusion coefficient for each snow layer.
      do k=1,nlayers

c Eqn. 20 of SNTH89.
        if (frac_liq(k) .gt. 0.02) then
          C_kT(k) = c1w * exp(- xLvw_o_Rw / T_layer(k)) *
     &      (xLvw_o_Rw / T_layer(k) - 1.0) / T_layer(k)**2
        else
          C_kT(k) = c1i * exp(- xLvi_o_Rw / T_layer(k)) *
     &      (xLvi_o_Rw / T_layer(k) - 1.0) / T_layer(k)**2
        endif

c SNTH89 assumed the porosity does not affect the fluxes.  SNTH89
c   had a note that said "The diffusive vapor flux in snow is
c   customarily taken as independent of porosity, which is
c   generally a consequence of the 'hand-to-hand' process of
c   vapor diffusion."  Conceptually, it seems like if the porosity
c   goes to zero the fluxes should stop.  I do that here.  If you
c   don't like this idea, you can just comment out these 3 lines.
c Scale the flux by the available vapor.
        vaporvol = porosity(k) - frac_liq(k)
        vaporvol = max(0.0,vaporvol)
        C_kT(k) = vaporvol * C_kT(k)

c Left part of Eqn. 21 of SNTH89.
        Des = De0 * (100000.0 / press) * (T_layer(k) / Tf)**6

c Eqn. 21 of SNTH89.  The vapor diffusion coefficents.
        diff_coef(k) = Des * C_kT(k)

        if (diff_coef(k).le.0.0) then
          print *,'diff_coef(k) <= 0.0',k,diff_coef(k)
          print *,porosity(k),ro_layer(k)
        endif

      enddo

c Include the boundary information in the required arrays.  This
c   information, and the flux calcuations below, follow Glen's 3D
c   thermal sea ice growth model code (see /nice/heat/temp_3d.f,
c   or SeaIce-3D).

c Number of shifted layers.
      nzm = nlayers + 1

c Control volume size.
      dz_bndry(1) = 0.0
      do k=2,nzm
        dz_bndry(k) = dz(k-1)
      enddo
      dz_bndry(nzm+1) = 0.0

c     do k=1,nzm+1
c       print *, k,dz_bndry(k)
c     enddo

c Temperature.
      T_layer_bndry(1) = T_layer(1)
      do k=2,nzm
        T_layer_bndry(k) = T_layer(k-1)
      enddo
      T_layer_bndry(nzm+1) = T_layer(nzm-1)

c     do k=1,nzm+1
c       print *, k,T_layer_bndry(k)
c     enddo

c Diffusion coefficients.
      diff_coef_bndry(1) = diff_coef(1)
      do k=2,nzm
        diff_coef_bndry(k) = diff_coef(k-1)
      enddo
      diff_coef_bndry(nzm+1) = diff_coef(nzm-1)

c     do k=1,nzm+1
c       print *, k,diff_coef_bndry(k)
c     enddo

c Calculate the vapor flux across the control volume walls (Eqn.
c   21 of SNTH89).  This is: flux = - diff_coef * dT/dz, where 
c   diff_coef = Des * C_kT.  See page 45 of Patankar (1980) for
c   a description of what is being done with the harmonic mean
c   calculations.  Here I am solving Patankar's Eqn. 4.8, with
c   delx_e- = 0.5*dx_P, and delx_e+ = 0.5*dx_E.
      do k=2,nzm

        if (dz_bndry(k-1).eq.0.0 .and. dz_bndry(k).eq.0.0) then
          Uv_bot = 0.0
        elseif (dz_bndry(k).eq.0.0 .and. dz_bndry(k+1).eq.0.0) then
          Uv_top = 0.0
        else
          Uv_bot = - 2.0 * (T_layer_bndry(k) - T_layer_bndry(k-1)) / 
     &      (dz_bndry(k-1) / diff_coef_bndry(k-1) +
     &      dz_bndry(k) / diff_coef_bndry(k))
          Uv_top = - 2.0 * (T_layer_bndry(k+1) - T_layer_bndry(k)) / 
     &      (dz_bndry(k) / diff_coef_bndry(k) +
     &      dz_bndry(k+1) / diff_coef_bndry(k+1))
        endif

c Assume the flux at the center of the control volume is the average
c   of the fluxes at the control volume walls.  Also note that we
c   don't care about the direction of the fluxes; we are just
c   assuming that the vapor transport is growing grains, regardless
c   of the direction.  This is used in the grain growth algorithm.
        vapor_flux(k-1) = (abs(Uv_bot) + abs(Uv_top)) / 2.0

c Save a record of the layer fluxes, including the direction of the
c   flow.  Here I am saving the flux across the top of each layer.
c   Under the assumption that the flux across the bottom of the
c   bottom layer is zero, this should be enough information to
c   calculate d_flux/d_z and get the mass loss and/or gain in each
c   layer.
        flux_layer(k-1) = Uv_top

      enddo

c Because the zero temperature gradient assumed at the boundaries
c   is not realistic, set the boundary fluxes equal to those just
c   inside the boundaries. 
      vapor_flux(1) = vapor_flux(2)
      vapor_flux(nlayers) = vapor_flux(nlayers-1)

c Because below we don't allow the abs(fluxes) to be over 1.0e-6,
c   do the same thing here.
      do k=1,nlayers

        if (flux_layer(k).lt.-1.0e-6) then
          flux_layer(k) = -1.0e-6
        elseif (flux_layer(k).gt.1.0e-6) then
          flux_layer(k) = 1.0e-6
        endif

c Convert these to fluxes per dt (instead of per sec).  Without
c   this the values are something like 10^-11.
          flux_layer(k) = dt * flux_layer(k)

      enddo

c Update the snow grain diameter.
      do k=1,nlayers

        if (diam_layer(k) .le. 0.0) then
          print *, 'execution halted because diam_layer <= 0.0'
          print *, 'layer = ',k,'  diam_layer(k) =',diam_layer(k)
          stop
        endif

c Dry snow: The cut-off bewteen dry and wet snow is arbitrarily
c   set at 0.0001.
        if (frac_liq(k) .lt. 1.0e-4) then

c The max vapor flux available for growth is arbitrarily set at
c   1.0e-6.  This can be increased if you want to allow larger
c   grains to grow, if the temperature gradients are available to
c   drive greater fluxes.  Here I have also included a scaling
c   term that can be used to increase or decrease the growth rate
c   of the grains to better match any observational datasets you
c   might have.  Eqn. 33 of SNTH89.
          if (abs(vapor_flux(k)) .lt. 1.0e-6) then
            diam_layer(k) = diam_layer(k) + grain_scale * dt * g1 *
     &        abs(vapor_flux(k)) / diam_layer(k)
          else
            diam_layer(k) = diam_layer(k) + grain_scale * dt * g1 *
     &        1.0e-6 / diam_layer(k)
          endif

c Wet snow: Different equations for liquid volume fraction
c   above and below 0.09.
        else
          if (frac_liq(k) .lt. 0.09) then 
c Eqn. 34a of SNTH89.
            diam_layer(k) = diam_layer(k) + grain_scale * dt * g2 *
     &        (frac_liq(k)+0.05) / diam_layer(k)
          else
c Eqn. 34b of SNTH89.
            diam_layer(k) = diam_layer(k) + grain_scale * dt * g2 *
     &        0.14 / diam_layer(k)
          endif
        endif

c Max grain size set at 5mm, based on Arctic Alaska depth hoar
c   observations (larger sizes are possible but not common).
        if (diam_layer(k) .gt. 5.0e-3) diam_layer(k) = 5.0e-3

      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GETGAMMA(KK,ro_layer,gamma)

      implicit none

      include 'snowmodel.inc'

      integer k,KK
      real ro_layer(nz_max)
      real gamma(nz_max)

c Compute the snow thermal conductivity (gamma) from the snow density.
      do k=1,KK
        if (ro_layer(k).lt.156.0) then
          gamma(k) = 0.023 + 0.234 * (ro_layer(k)/1000.0)
        else
          gamma(k) = 0.138 - 1.01 * (ro_layer(k)/1000.0) + 3.233 *
     &      (ro_layer(k)/1000.0)**2
        endif
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        
      SUBROUTINE GET_THERMAL_CONDUCTIVITY(KK,ro_layer,diam_layer,gamma)

c This program calculates the thermal conductivity for a given
c   grain size.

c The grain size here is provided in mm, and is assumed to range
c   from 0.5 mm (wind slab) to 5.0 mm (depth hoar).

      implicit none

      include 'snowmodel.inc'

      integer k,KK
      real ro_layer(nz_max)
      real gamma(nz_max)
      real diam_layer(nz_max)

      real cond_slab,cond_hoar,wt,x1,x2,xx

c Convert the diameter limits from mm to m.
      x1 = 0.5 / 1000.0
      x2 = 5.0 / 1000.0

      do k=1,KK
        if (ro_layer(k).lt.180.0) then
          cond_slab = 3.94e-2 + 2.00e-4*ro_layer(k)
        else
          cond_slab = 1.55e-1 - 1.02e-3*ro_layer(k) +
     &      3.21e-6*ro_layer(k)**2
        endif

        cond_hoar = 3.00e-2 + 2.00e-4*ro_layer(k)

c Calculate the weighting factors.
        xx = diam_layer(k)
        wt = (xx - x1) / (x2 - x1)

c Calculate the thermal conductivity.
        gamma(k) = wt * cond_hoar + (1.0 - wt) * cond_slab

      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE POST_PROC_ML(KK,snod_layer,snow_depth,swe_depth,undef,
     &  swed_layer,gamma,ro_layer,melt_flag,T_old,Tf,ro_snow_grid,
     &  ro_water,xro_snow,ro_snow,diam_layer)

      implicit none

      include 'snowmodel.inc'

      integer k,KK

      real snod_layer(nz_max)
      real swed_layer(nz_max)
      real diam_layer(nz_max)
      real gamma(nz_max)
      real ro_layer(nz_max)
      real T_old(nz_max)
      integer melt_flag(nz_max)

      real snow_depth,swe_depth,undef,Tf,ro_snow_grid,ro_water,
     &  xro_snow,ro_snow

c Calculate the total snow and swe depth, and the bulk snow density.
      snow_depth = 0.0
      swe_depth = 0.0
      do k=1,KK
        snow_depth = snow_depth + snod_layer(k)
        swe_depth = swe_depth + swed_layer(k)
      enddo
      if (snow_depth.le.0.0) then
        ro_snow_grid = ro_snow
      else
        ro_snow_grid = swe_depth * ro_water / snow_depth
      endif

c Set any areas outside the snowpack to undef.
      do k=KK+1,nz_max
        gamma(k) = undef
        ro_layer(k) = undef
        T_old(k) = undef + Tf
        melt_flag(k) = nint(undef)
        snod_layer(k) = undef
        swed_layer(k) = undef
        diam_layer(k) = undef
      enddo

c Clean up the snow density array so there are no values when
c   there is no snow.
      if (snow_depth.eq.0.0) then
        xro_snow = undef
      else
        xro_snow = ro_snow_grid
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MERGE_LAYERS_ML(KK,ro_layer,snod_layer,swed_layer,
     &  T_old,ro_water,max_layers,change_layer,dz_snow_min,melt_flag,
     &  diam_layer)

      implicit none

      include 'snowmodel.inc'

      integer k,KK,kkk,k_small,max_layers,icount,kkkk

      real swed_layer(nz_max)
      real ro_layer(nz_max)
      real snod_layer(nz_max)
      real T_old(nz_max)
      real diam_layer(nz_max)
      integer melt_flag(nz_max)

      real snod_layer_small,ro_water,dz_snow_min,change_layer

c Merge layers if the number of layers exceeds some maximum number of
c   layers or if a layer gets thinner than some minimum thickness.
c   Do this in snow_depth space because that is the grid the snow
c   temperatures are being calculated on.

c If any layer is thinner than the minimum layer thickness, merge it
c   with the layer below.  If that layer is layer 1, merge it with
c   layer 2.  If there is only one layer left, let it be smaller than
c   the minimum thickness.  Don't do any of this if a new layer is
c   being built; only do it for layers below the top layer.
      change_layer = 0.0

c Count how many layers are less than the minimum thickness, excluding
c   the case where there is only one layer.
      icount = 0
      if (KK.gt.1) then
c       do k=1,KK
        do k=1,KK-1
          if (snod_layer(k).lt.dz_snow_min) then
            icount = icount + 1
          endif
        enddo
      endif

c Note that if two thin layers are together, the merge may take
c   out the other one.
      do k=1,icount
        change_layer = 1.0

c This gets and processes the last occurance.
c       do kkkk=1,KK
        do kkkk=1,KK-1
          if (snod_layer(kkkk).lt.dz_snow_min) then
            k_small = kkkk
          endif
        enddo

        if (k_small.eq.1) then
          snod_layer(1) = snod_layer(1) + snod_layer(2)
          swed_layer(1) = swed_layer(1) + swed_layer(2)
          ro_layer(1) = swed_layer(1) * ro_water / snod_layer(1)
          T_old(1) = T_old(2)
          diam_layer(1) = diam_layer(2)
          melt_flag(1) = melt_flag(2)
          KK = KK - 1
          do kkk=2,KK
            snod_layer(kkk) = snod_layer(kkk+1)
              swed_layer(kkk) = swed_layer(kkk+1)
            ro_layer(kkk) = swed_layer(kkk) * ro_water / snod_layer(kkk)
            T_old(kkk) = T_old(kkk+1)
            diam_layer(kkk) = diam_layer(kkk+1)
            melt_flag(kkk) = melt_flag(kkk+1)
          enddo
        else
          snod_layer(k_small-1) = snod_layer(k_small-1) + 
     &      snod_layer(k_small)
          swed_layer(k_small-1) = swed_layer(k_small-1) +
     &      swed_layer(k_small)
          ro_layer(k_small-1) = swed_layer(k_small-1) * ro_water /
     &      snod_layer(k_small-1)
          T_old(k_small-1) = T_old(k_small)
          diam_layer(k_small-1) = diam_layer(k_small)
          melt_flag(k_small-1) = melt_flag(k_small)
          KK = KK - 1
          do kkk=k_small,KK
            snod_layer(kkk) = snod_layer(kkk+1)
            swed_layer(kkk) = swed_layer(kkk+1)
            ro_layer(kkk) = swed_layer(kkk) * ro_water / snod_layer(kkk)
            T_old(kkk) = T_old(kkk+1)
            diam_layer(kkk) = diam_layer(kkk+1)
            melt_flag(kkk) = melt_flag(kkk+1)
          enddo
        endif
      enddo

c Where the number of layers exceeds some maximum number of layers,
c   find the thinnest layer and merge it with the one below.  For the
c   case where the thinnest layer is the bottom layer, merge it with
c   layer 2.
      if (KK.eq.max_layers+1) then
        change_layer = 1.0
c Find the thinnest layer.
        snod_layer_small = 1000.0
        do k=1,KK
          if (snod_layer(k).lt.snod_layer_small) then
            snod_layer_small = snod_layer(k)
            k_small = k
          endif
        enddo

c Adjust accordingly.  Note that layers below the thin layer do not
c   change, unless the thin layer is layer 1.  Also, since the layer
c   is thin, assign the new layer the thick layer temperature.
        if (k_small.eq.1) then
          snod_layer(1) = snod_layer(1) + snod_layer(2)
          swed_layer(1) = swed_layer(1) + swed_layer(2)
          ro_layer(1) = swed_layer(1) * ro_water / snod_layer(1)
          T_old(1) = T_old(2)
          diam_layer(1) = diam_layer(2)
          melt_flag(1) = melt_flag(2)
          KK = KK - 1
          do kkk=2,KK
            snod_layer(kkk) = snod_layer(kkk+1)
              swed_layer(kkk) = swed_layer(kkk+1)
            ro_layer(kkk) = swed_layer(kkk) * ro_water / snod_layer(kkk)
            T_old(kkk) = T_old(kkk+1)
            diam_layer(kkk) = diam_layer(kkk+1)
            melt_flag(kkk) = melt_flag(kkk+1)
          enddo
        else
          snod_layer(k_small-1) = snod_layer(k_small-1) +
     &      snod_layer(k_small)
          swed_layer(k_small-1) = swed_layer(k_small-1) +
     &      swed_layer(k_small)
          ro_layer(k_small-1) = swed_layer(k_small-1) * ro_water /
     &      snod_layer(k_small-1)
          T_old(k_small-1) = T_old(k_small)
          diam_layer(k_small-1) = diam_layer(k_small)
          melt_flag(k_small-1) = melt_flag(k_small)
          KK = KK - 1
          do kkk=k_small,KK
            snod_layer(kkk) = snod_layer(kkk+1)
            swed_layer(kkk) = swed_layer(kkk+1)
            ro_layer(kkk) = swed_layer(kkk) * ro_water / snod_layer(kkk)
            T_old(kkk) = T_old(kkk+1)
            diam_layer(kkk) = diam_layer(kkk+1)
            melt_flag(kkk) = melt_flag(kkk+1)
          enddo
        endif
      endif

c Now that we are done with change_layer, set it equal to k_small,
c   the position of the change.
      if (change_layer.eq.1.0) change_layer = real(k_small)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MELT_SNOW_ML(KK,swed_layer,ro_water,ro_layer,Qm,dt,
     &  snod_layer,ro_snowmax,rain,xLf,Cp_snow,Tf,T_old,melt_flag,
     &  runoff,canopy_unload,swe_depth,snow_depth,vegtype,
     &  glacier_melt,sum_glacmelt,sum_swemelt,soft_snow_d,Qe,
     &  sfc_sublim_flag,sum_sfcsublim,swesublim,ro_snow,
     &  corr_factor,icorr_factor_index)

      implicit none

      include 'snowmodel.inc'

      integer k,KK

      real swed_layer(nz_max)
      real ro_layer(nz_max)
      real snod_layer(nz_max)
      real T_old(nz_max)
      real swemelt,extra,ro_water,swe_space,add,runoff,ro_snowmax,
     &  rain,delta_T,xLf,Cp_snow,extra_delta_T,Tf,dt,Qm,canopy_unload,
     &  potmelt,swe_depth,snow_depth,vegtype,glacier_melt,
     &  sum_glacmelt,sum_swemelt,soft_snow_d,Qe,sfc_sublim_flag,
     &  xLsublim,potsublim,swesublim,sum_sfcsublim,ro_snow,
     &  potmelt_tmp,corr_factor

      integer icorr_factor_index

      integer melt_flag(nz_max)

c Initialize the runoff array.
      runoff = 0.0

c SURFACE SUBLIMATION.

c Whether static-surface (non-blowing snow) sublimation is included
c   in the model calculations is controlled by the sfc_sublim_flag.
c   I am waiting for the flux-tower data Matthew and I are collecting
c   in Alaska, to compare with the model simulations, before
c   including this part of the model in all simulations.

c If the sfc_sublim_flag is turned on, the latent heat flux (Qe)
c   calculated in ENBAL is used to add/remove snow from the snowpack.
c   xLsublim = xLf + xLv = 2.5104x10^6 J/kg + 3.334x10^5 J/kg, and
c   potsublim is in m swe.

      if (swe_depth.gt.0.0  .and.  sfc_sublim_flag.eq.1.0) then
        if (Qe.lt.0.0) then
c Compute the snow-surface sublimation (m, swe).
          xLsublim = 2.844e6
          potsublim = (- dt) * Qe / (ro_water * xLsublim)
          swesublim = min(potsublim,swe_depth)
c Save a summing array of the static surface snow sublimation.
          sum_sfcsublim = sum_sfcsublim + swesublim
        else
          swesublim = 0.0
        endif
      else
        swesublim = 0.0
      endif

c Modify the swe layer thicknesses, and reduce the number of layers
c   if needed.
      if (swesublim.gt.0.0) then
c Check to see whether this sublimation requires a layer reduction.
        CALL REDUCE_LAYERS(swesublim,swed_layer,KK)

c Build the new snow layer thicknesses, and recalculate the total
c   snow and swe depths.  Assume this sublimated snow does not
c   change the snow density and does not change the soft snow depth.
c   It only reduces the snow depth and the associated swe depth.
        snow_depth = 0.0
        swe_depth = 0.0
        do k=1,KK
          snod_layer(k) = swed_layer(k) * ro_water / ro_layer(k)
          snow_depth = snow_depth + snod_layer(k)
          swe_depth = swe_depth + swed_layer(k)
        enddo
      endif

c MELTING.

      if (Qm.gt.0.0) then

c Convert the melt energy to water equivalent melt depth (m).
        potmelt = dt * Qm / (ro_water * xLf)

c Account for any snowmelt data assimilation.
        if (icorr_factor_index.lt.0) then
          potmelt_tmp = potmelt * corr_factor
          swemelt = min(potmelt_tmp,swe_depth)
c Handle the case of no snowmelt data assimilation.
        else
          swemelt = min(potmelt,swe_depth)
        endif

c Compute any glacier or permanent snow-field melt (m water equiv.).
        if (vegtype.eq.20.0) then
          glacier_melt = potmelt - swemelt
        else
          glacier_melt = 0.0
        endif

c Save a summing array of the glacier melt.
        sum_glacmelt = sum_glacmelt + glacier_melt

c Save the runoff contribution.
        runoff = runoff + glacier_melt

c Save a summing array of the snow melt.
        sum_swemelt = sum_swemelt + swemelt

c In the presence of melt, zero out the soft snow layer.
        soft_snow_d = 0.0

      else

c These prevent values from the previous time step from being
c   carried through to the next time step.
        swemelt = 0.0
        glacier_melt = 0.0

      endif

c Handle the case where rain and canopy_unload fall on snow-free
c   ground (this is not included in the code below, nor in the
c   PRECIP_ML subroutine, so I include it here).
      if (swe_depth.eq.0.0) then
        runoff = runoff + rain + canopy_unload
      endif

c Deal with melting snow.

      if (swemelt.gt.0.0) then
c Check to see whether this melt leads to a reduction in layers.
        CALL REDUCE_LAYERS(swemelt,swed_layer,KK)

c Build the new snow layer thicknesses, and initiate the melt_flag.
        do k=1,KK
          snod_layer(k) = swed_layer(k) * ro_water / ro_layer(k)
          melt_flag(k) = 0
        enddo
      endif

c Add the melt, rain, and canopy unloading (assumed to be wet as rain)
c   to the remaining snow layer thicknesses, up to the maximum snow
c   density, and let the rest of the melt drain out the snowpack bottom
c   as runoff.
      extra = swemelt + rain + canopy_unload
      if (extra.gt.0.0) then
        do k=KK,1,-1
          if (extra.gt.0.0) then
            swe_space = snod_layer(k) * (ro_snowmax - ro_layer(k)) /
     &        ro_water
            add = min(swe_space,extra)
            swed_layer(k) = swed_layer(k) + add
            extra = extra - add
          endif
          if (snod_layer(k).le.0.0) then
            ro_layer(k) = ro_snow
          else
            ro_layer(k) = swed_layer(k) * ro_water / snod_layer(k)
          endif
        enddo
        runoff = extra
      endif

c Also take into account the refreezing of this liquid in a cold
c   snowpack.  Assume that the liquid will fully warm each layer before
c   moving on to the next layer.
      extra = swemelt + rain + canopy_unload
      if (extra.gt.0.0) then
        do k=KK,1,-1
          if (extra.gt.0.0) then

c Compute the change in temperature that would result if this liquid
c   was used to freeze and raise the snow temperature.
            if (snod_layer(k).le.0.0) then
              delta_T = 0.0
            else
              delta_T = (extra * xLf) / (Cp_snow * snod_layer(k))
            endif

c Use this potential temperature change to adjust the snow
c   temperature in the presence of the liquid.
            T_old(k) = T_old(k) + delta_T
            extra_delta_T = max(0.0,T_old(k)-Tf)
            T_old(k) = min(T_old(k),Tf)

c Keep track of which layers have been pushed to Tf.  This will be
c   used in the temperature solution subroutine to fix the snow
c   temperature at Tf (if melt_flag = 1).
            if (T_old(k).eq.Tf) then
              melt_flag(k) = 1
            else
              melt_flag(k) = 0
            endif

c Define the amount of liquid this remaining temperature change
c   represents, so it can be used in the layer below (that may be
c   a different size).
            extra = Cp_snow * snod_layer(k) * extra_delta_T / xLf

          endif
        enddo
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE REDUCE_LAYERS(swemelt,swed_layer,KK)

      implicit none

      include 'snowmodel.inc'

      integer k,KK
      real swed_layer(nz_max)
      real eps,swemelt_tmp,swemelt,excess

      eps = 1e-6
      swemelt_tmp = swemelt

c The use of eps here does not allow the vertical grid increment to
c   be less that eps.

      do k=KK,1,-1
        excess = swed_layer(k) - swemelt_tmp

c       if (excess.gt.0.0) then
        if (excess.gt.eps) then
          swed_layer(k) = excess
          KK = k
          return
c       elseif (excess.eq.0.0) then
        elseif (excess.ge.0.0 .and. excess.le.eps) then
          KK = k - 1
          return
        else
          swemelt_tmp = - excess
        endif
      enddo

c If there is no snow left.
      KK = 0

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE PRECIP_ML(KK,ro_layer,snod_layer,ro_water,tslsnowfall,
     &  swed_layer,ro_nsnow,T_old,Tsfc,tsls_threshold,dt,
     &  melt_flag,soft_snow_d,ro_snow,sum_sprec,sprec_grnd_ml,
     &  sum_prec,prec,sum_runoff,runoff,snow_d,snow_depth,swe_depth,
     &  diam_layer)

      implicit none

      include 'snowmodel.inc'

      integer k,KK

      real snod_layer(nz_max)
      real ro_layer(nz_max)
      real swed_layer(nz_max)
      real T_old(nz_max)
      real diam_layer(nz_max)
      real ro_nsnow,ro_water,z_nsnow,tsls_threshold,
     &  z_snowtopl,sweq_topl,Tsfc,tslsnowfall,dt,soft_snow_d,ro_snow,
     &  sum_sprec,sprec_grnd_ml,sum_prec,prec,sum_runoff,runoff,snow_d,
     &  snow_depth,swe_depth

      integer melt_flag(nz_max)

c If the melt from the previous subroutine reduced the snowpack
c   to no snow, reset the time since last snowfall to the threshold,
c   otherwise you will not build a new layer on the bare ground.
      if (KK.eq.0) tslsnowfall = tsls_threshold

c Create and/or modify the snow c.v.'s to account for new snowfall.
      if (sprec_grnd_ml.gt.0.0) then
        if (tslsnowfall.ge.tsls_threshold) then
c Create a new layer if snowfall has stopped for a period of time
c   greater or equal to the defined threshold.
          KK = KK + 1
          z_nsnow = ro_water / ro_nsnow * sprec_grnd_ml
          snod_layer(KK) = z_nsnow
          ro_layer(KK) = ro_nsnow
          swed_layer(KK) =  ro_layer(KK) * snod_layer(KK) / ro_water
c Define this new snow layer to have the surface temperature.
          T_old(KK) = Tsfc
c Define this new layer to have the initial grain size (0.5 mm).
          diam_layer(KK) = 0.5 / 1000.0

          melt_flag(KK) = 0
        else
c Add to the existing top layer.
          z_nsnow = ro_water / ro_nsnow * sprec_grnd_ml
          z_snowtopl = snod_layer(KK) + z_nsnow
          sweq_topl = sprec_grnd_ml + snod_layer(KK) * ro_layer(KK) /
     &      ro_water
          snod_layer(KK) = snod_layer(KK) + z_nsnow
          ro_layer(KK) = ro_water * sweq_topl / z_snowtopl
          swed_layer(KK) = ro_layer(KK) * snod_layer(KK) / ro_water
        endif

c Update the total swe and snow depths.
        snow_depth = 0.0
        swe_depth = 0.0
        do k=1,KK
          snow_depth = snow_depth + snod_layer(k)
          swe_depth = swe_depth + swed_layer(k)
        enddo
      endif

c Define the time since last snowfall, in hours.  Handle the case
c   where there is no snow on the ground.
      if (sprec_grnd_ml.gt.0.0) then
        tslsnowfall = 0.0
      else
        tslsnowfall = tslsnowfall + dt / 3600.0
      endif
      if (KK.eq.0) tslsnowfall = tsls_threshold

c The following are set up to be compatible with SnowTran-3D, and
c   are in snow-depth units.  The sum_sprec corrections are done
c   in the SnowTran-3D code.
      soft_snow_d = soft_snow_d + sprec_grnd_ml * ro_water / ro_snow
      snow_d = swe_depth * ro_water / ro_snow
c     sum_sprec = sum_sprec + sprec_grnd_ml * ro_water / ro_snow
      sum_sprec = sum_sprec + sprec_grnd_ml

c The following are in swe-depth units.
      sum_prec = sum_prec + prec
      sum_runoff = sum_runoff + runoff

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE DDENSITY_ML(ro_layer,Tf,dt,ro_water,ro_snowmax,
     &  T_old,KK,snod_layer,A1,A2,windspd_2m)

      implicit none

      include 'snowmodel.inc'

      integer k,KK,kkk

      real snod_layer(nz_max)
      real ro_layer(nz_max)
      real T_old(nz_max)
      real sweqstar(nz_max)
      real sweql(nz_max)

      real A1,A2,ro_water,ro_snowmax,dt,Tf,ro_adjust,C,U,alfa,
     &  windspd_2m

c Define the density rate coefficients.
c     C = 1.00
      C = 0.50
c     C = 0.10

c Define alfa. 
      alfa = 0.2

c ro_adjust is a snow density rate adjustment factor that can be
c   used to make the snow density increase faster (ro_adjust > 1.0)
c   or slower (ro_adjust < 1.0).
      ro_adjust = 5.0

      if (KK.gt.0) then

        do k=1,KK
          sweql(k) = ro_layer(k) / ro_water * snod_layer(k)
        enddo

        do kkk=1,KK
          sweqstar(kkk) = sweql(kkk) / 2.0
          do k=kkk+1,KK
            sweqstar(kkk) = sweqstar(kkk) + sweql(k)
          enddo
        enddo

c Pre-wind-adjustment code.
c       do k=1,KK
c         ro_layer(k) = ro_layer(k) + ro_adjust * dt *
c    &      (A1 * sweqstar(k) * ro_layer(k) *
c    &      exp(-0.08*(Tf-T_old(k))) * exp(-A2*ro_layer(k)))
c         ro_layer(k) = min(ro_snowmax,ro_layer(k))
c         snod_layer(k) = sweql(k) * ro_water / ro_layer(k)
c       enddo

c Update the density of all the layers except the top layer.
        do k=1,KK-1
          ro_layer(k) = ro_layer(k) + ro_adjust * dt *
     &      (A1 * sweqstar(k) * ro_layer(k) *
     &      exp(-0.08*(Tf-T_old(k))) * exp(-A2*ro_layer(k)))
          ro_layer(k) = min(ro_snowmax,ro_layer(k))
          snod_layer(k) = sweql(k) * ro_water / ro_layer(k)
        enddo

c Update the density of the top layer while including the influence
c   of wind in this layer only.  This is Equation (18) for U in
c   Liston et al. (2007).  C here has just been an assigned an
c   arbitrary value; it controls the impact of U on the density
c   evolution.
        k=KK

        if (windspd_2m.ge.5.0) then
          U = C *
     &      (5.0 + 15.0 * (1.0 - exp(-(alfa*(windspd_2m - 5.0)))))
        else
          U = 1.0
        endif
c If you want to turn this off, you can uncomment the following
c   line.
c       U = 1.0

c The equation below is the same as that for the below-surface
c   layers, except for the addition of the U term.
        ro_layer(k) = ro_layer(k) + ro_adjust * dt *
     &    (U * A1 * sweqstar(k) * ro_layer(k) *
     &    exp(-0.08*(Tf-T_old(k))) * exp(-A2*ro_layer(k)))
        ro_layer(k) = min(ro_snowmax,ro_layer(k))
        snod_layer(k) = sweql(k) * ro_water / ro_layer(k)

      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SNOWTEMP_ML(gamma,T_old,Tsfc,KK,dt,ro_layer,Cp_snow,
     &  Tf,snod_layer,melt_flag,diam_layer)

      implicit none

      include 'snowmodel.inc'

      real gamma(nz_max)
      real diam_layer(nz_max)
      real ro_layer(nz_max)
      real snod_layer(nz_max)
      real g_b_ns(nz_max+1)
      real f_n(nz_max+1)
      real aN(nz_max)
      real aP0(nz_max)
      real aS(nz_max)
      real dely_p(nz_max+1)
      real dy_p(nz_max)
      real y_crds(nz_max+2)
      real y_wall(nz_max+1)
      real A_sub(nz_max)
      real A_super(nz_max)
      real A_main(nz_max)
      real b_vector(nz_max)
      real T_old(nz_max)
      real Sc(nz_max)
      real Sp(nz_max)

      integer melt_flag(nz_max)

      integer k,KK
      real Tsfc,T_N,T_S,bc_N,bc_S,Cp_snow,Tf,dt,Tsg

c Define the snow thermal conductivity (gamma) for each layer.
c     CALL GETGAMMA(KK,ro_layer,gamma)

c This is the thermal conductivity that depends on grain size.
      CALL GET_THERMAL_CONDUCTIVITY(KK,ro_layer,diam_layer,gamma)

      if (KK.gt.1) then

c Update the control volume information.
        CALL GETCV(KK,dy_p,snod_layer)
        CALL CV_INFO(dely_p,f_n,y_crds,y_wall,dy_p,KK)

c Compute the general equation coefficients.
        CALL GAMMA1(g_b_ns,gamma,f_n,KK)
        CALL GE_COEF(aN,aS,aP0,dy_p,dely_p,g_b_ns,dt,KK,
     &    ro_layer,Cp_snow)

c---------------------------------------------------------------------
c---------------------------------------------------------------------
c Account for the boundary conditions.
c   South boundary condition:
c     For T_S = known, define 
c       bc_S = aS(1) * T_S;         where T_S = known
c     For dT_S/dn = 0, define
c       bc_S = 0.0
c       aS(1) = 0.0
c   North boundary condition:
c     For T_N = known, define 
c       bc_N = aN(KK) * T_N;        where T_N = known
c     For dT_N/dn = 0, define
c       bc_N = 0.0
c       aN(KK) = 0.0
c---------------------------------------------------------------------
c---------------------------------------------------------------------

c Define the upper and lower boundary conditions.
c Upper.
        T_N = Tsfc
        bc_N = aN(KK) * T_N
c Lower.
c   Zero gradient:
c       bc_S = 0.0
c       aS(1) = 0.0
c   Fixed bottom temperature (-1.0 C):
        T_S = Tf - 1.0
        bc_S = aS(1) * T_S

c Provide the source terms.

c Force the source terms to produce Tf at the positions where melting
c   occurred during this time step.
        do k=1,KK
          if (melt_flag(k).eq.1) then
            Sc(k) = 10e30 * Tf
            Sp(k) = -10e30
          else
            Sc(k) = 0.0
            Sp(k) = 0.0
          endif
        enddo

c Configure the information for the matrix solver.
        CALL PREPSOLVE(A_sub,A_super,A_main,b_vector,T_old,
     &    dy_p,bc_S,bc_N,Sc,Sp,aN,aS,aP0,KK)

c Solve the system of equations.
        CALL TRISOLVE(T_old,A_sub,A_main,A_super,b_vector,KK)

      elseif (KK.eq.1) then
c Assume that the snow-ground interface temperature is -1.0 C.
        Tsg = Tf - 1.0
        T_old(1) = 0.5 * (Tsg + Tsfc)
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GETCV(KK,dy_p,snod_layer)

      implicit none

      include 'snowmodel.inc'

      real dy_p(nz_max)
      real snod_layer(nz_max)

      integer k,KK

c Provide values of Control Volume size in the y direction.
      do k=1,KK
        dy_p(k) = snod_layer(k)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE PREPSOLVE(A_sub,A_super,A_main,b_vector,T_old,
     &  dy_p,bc_S,bc_N,Sc,Sp,aN,aS,aP0,KK)

      implicit none

      include 'snowmodel.inc'

      real aP(nz_max)
      real aN(nz_max)
      real aS(nz_max)
      real Sp(nz_max)
      real Sc(nz_max)
      real aP0(nz_max)
      real dy_p(nz_max)
      real T_old(nz_max)
      real b_vector(nz_max)
      real A_sub(nz_max)
      real A_super(nz_max)
      real A_main(nz_max)

      integer k,KK
      real bc_S,bc_N

c Compute matrix diagonal and b coeffs.
      do k=1,KK
        aP(k) = aN(k) + aS(k) + aP0(k) - Sp(k) * dy_p(k)
        b_vector(k) = Sc(k) * dy_p(k) + aP0(k) * T_old(k)
      enddo

c Modify b to account for dirichlet boundary conditions.
      b_vector(1) = b_vector(1) + bc_S
      b_vector(KK) = b_vector(KK) + bc_N

c Prepare to call the tridiagonal solver.
      do k=1,KK-1
        A_sub(k) = - aS(k+1)
        A_super(k) = - aN(k)
      enddo

      do k=1,KK
        A_main(k) = aP(k)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CV_INFO(dely_p,f_n,y_crds,y_wall,dy_p,KK)

      implicit none

      include 'snowmodel.inc'

      real dy_pbc(nz_max+2)
      real dely_p(nz_max+1)
      real f_n(nz_max+1)
      real dy_p(nz_max)
      real y_crds(nz_max+2)
      real y_wall(nz_max+1)

      integer k,KK
      real temp

c PRESSURE CONTROL VOLUME SIZE AND POSITION INFORMATION

c Include exterior boundary pressure grid points.
      dy_pbc(1) = 0.0
      do k=2,KK+1
        dy_pbc(k) = dy_p(k-1)
      enddo
      dy_pbc(KK+2) = 0.0

c Compute the distance between pressure grid points.
      do k=1,KK+1
        dely_p(k) = .5 * (dy_pbc(k) + dy_pbc(k+1))
      enddo

c Compute the distance between the pressure grid points and the control
c   volume wall.  (The following is true because the grid points do
c   pressure are defined to be in the center of the control volume.)
c   And then compute f_e and f_n.  These two steps are combined below.
      do k=1,KK+1
        f_n(k) = .5 * dy_pbc(k+1) / dely_p(k)
      enddo

c Compute the x and y coordinates of the pressure c.v. grid points,
c   including boundaries.
      temp = 0.0
      do k=1,KK+2
        y_crds(k) = temp + .5 * dy_pbc(k)
        temp = temp + dy_pbc(k)
      enddo

c Compute the x and y coordinates of the pressure c.v. walls.
      y_wall(1) = 0.0
      do k=2,KK+1
        y_wall(k) = y_wall(k-1) + dy_p(k-1)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GAMMA1(g_b_ns,gamma,f_n,KK)

      implicit none

      include 'snowmodel.inc'

      real g_b_ns(nz_max+1)
      real gamma(nz_max)
      real g_ns(nz_max+2)
      real f_n(nz_max+1)

      integer k,KK

c This provides gamma information on c.v. walls.

c Include gamma just outside of n, s boundaries.
      g_ns(1) = gamma(1)
      do k=2,KK+1
        g_ns(k) = gamma(k-1)
      enddo
      g_ns(KK+2) = gamma(KK)

c Compute gamma (diffusion coefficient) at the n, s control
c   volume boundaries using equation 4.9, p. 45.
      do k=1,KK+1
        g_b_ns(k) = 1.0/((1.0 - f_n(k))/g_ns(k) + f_n(k)/g_ns(k+1))
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GE_COEF(aN,aS,aP0,dy_p,dely_p,g_b_ns,dt,KK,
     &  ro_layer,Cp_snow)

      implicit none

      include 'snowmodel.inc'

      real aN(nz_max)
      real aS(nz_max)
      real aP0(nz_max)
      real dely_p(nz_max+1)
      real g_b_ns(nz_max+1)
      real dy_p(nz_max)
      real ro_layer(nz_max)

      integer k,KK
      real Cp_snow,dt

c CALCULATE THE COEFFICIENTS aP, for the general phi equation.
      do k=2,KK+1
        aN(k-1) = g_b_ns(k)   / dely_p(k)
        aS(k-1) = g_b_ns(k-1) / dely_p(k-1)
      enddo

      do k=1,KK
        aP0(k) = ro_layer(k) * Cp_snow * dy_p(k) / dt
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE TRISOLVE(x,asub,amain,asuper,b,KK)

      implicit none

      include 'snowmodel.inc'

      real asub(nz_max)
      real asuper(nz_max)
      real amain(nz_max)
      real b(nz_max)
      real x(nz_max)
      real z(nz_max)
      real lmain(nz_max)
      real lsub(nz_max)
      real usuper(nz_max)

      integer k,KK

      lmain(1) = amain(1)
      usuper(1) = asuper(1)/lmain(1)

      do k=2,KK-1
        lsub(k-1) = asub(k-1)
        lmain(k) = amain(k) - lsub(k-1) * usuper(k-1)
        usuper(k) = asuper(k) / lmain(k)
      enddo

      lsub(KK-1) = asub(KK-1)
      lmain(KK) = amain(KK) - lsub(KK-1) * usuper(KK-1)
      z(1) = b(1) / lmain(1)

      do k=2,KK
        z(k) = 1.0 / lmain(k) * (b(k) - lsub(k-1) * z(k-1))
      enddo

      x(KK) = z(KK)

      do k=KK-1,1,-1
        x(k) = z(k) - usuper(k) * x(k+1)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE ZERO_SNOW(nx,ny,snow_depth,ro_snow_grid,ro_snow,
     &  swe_depth,swe_depth_old,canopy_int_old,KK,sum_swemelt,
     &  tslsnowfall,snod_layer,swed_layer,ro_layer,T_old,
     &  sum_sprec,multilayer_snowpack,tsls_threshold,
     &  sum_trans)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,k

      integer multilayer_snowpack
      integer KK(nx_max,ny_max)

      real tsls_threshold,ro_snow
      real tslsnowfall(nx_max,ny_max)
      real snod_layer(nx_max,ny_max,nz_max)
      real swed_layer(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real sum_sprec(nx_max,ny_max)
      real sum_swemelt(nx_max,ny_max)
      real sum_trans(nx_max,ny_max)

      print *,'ZEROING OUT THE SNOW ARRAYS'
      print *,'ZEROING OUT THE SNOW ARRAYS'
      print *,'ZEROING OUT THE SNOW ARRAYS'

      do j=1,ny
        do i=1,nx
          canopy_int_old(i,j) = 0.0
          swe_depth_old(i,j) = 0.0
          snow_depth(i,j) = 0.0
          ro_snow_grid(i,j) = ro_snow
          swe_depth(i,j) = 0.0
          sum_sprec(i,j) = 0.0
          sum_swemelt(i,j) = 0.0
          sum_trans(i,j) = 0.0
        enddo
      enddo

      if (multilayer_snowpack.eq.1) then
        do j=1,ny
          do i=1,nx
            tslsnowfall(i,j) = tsls_threshold
            do k=1,KK(i,j)
              snod_layer(i,j,k) = 0.0
              swed_layer(i,j,k) = 0.0
              ro_layer(i,j,k) = ro_snow
              T_old(i,j,k) = 273.15
            enddo
            KK(i,j) = 0
          enddo
        enddo
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE ZERO_SNOW_SEAICE_4(nx,ny,snow_depth,ro_snow_grid,
     &  ro_snow,swe_depth,swe_depth_old,canopy_int_old,KK,
     &  sum_swemelt,tslsnowfall,snod_layer,swed_layer,
     &  ro_layer,T_old,sum_sprec,multilayer_snowpack,
     &  tsls_threshold,iyear,diam_layer,output_path_wo_assim)

c NOTE: This program is VERY specific to the original Lagrangian
c   simulations.  If you are doing something else, you will have to
c   make numerous changes to this code and the subroutines that are
c   called from it.

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,k,iyear,icount

      parameter (icount=70000)

      integer multilayer_snowpack
      integer KK(nx_max,ny_max)

      real tsls_threshold,ro_snow
      real tslsnowfall(nx_max,ny_max)
      real snod_layer(nx_max,ny_max,nz_max)
      real swed_layer(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real sum_sprec(nx_max,ny_max)
      real sum_swemelt(nx_max,ny_max)
      real diam_layer(nx_max,ny_max,nz_max)

      real snod_init(icount)
      real swed_init(icount)
      real sden_init(icount)

      character*80 output_path_wo_assim

      print *,'TRANSFERRING THE SNOW DATA TO THE NEXT YEAR'
      print *,'TRANSFERRING THE SNOW DATA TO THE NEXT YEAR'
      print *,'TRANSFERRING THE SNOW DATA TO THE NEXT YEAR'

c Calculate the snow data initial conditions that will be used
c   in the next year.
      CALL SUPERIMPOSED_ICE(iyear,output_path_wo_assim,
     &  snod_init,swed_init,sden_init,ro_snow)

      do j=1,ny
        do i=1,nx

          canopy_int_old(i,j) = 0.0
          swe_depth_old(i,j) = 0.0
          snow_depth(i,j) = 0.0
          ro_snow_grid(i,j) = ro_snow
          swe_depth(i,j) = 0.0
          sum_sprec(i,j) = 0.0
          sum_swemelt(i,j) = 0.0

          if (snod_init(i).ge.0.0) then
            swe_depth_old(i,j) = swed_init(i)
            snow_depth(i,j) = snod_init(i)
            ro_snow_grid(i,j) = sden_init(i)
            swe_depth(i,j) = swed_init(i)
          endif

        enddo
      enddo

      if (multilayer_snowpack.eq.1) then
        do j=1,ny
          do i=1,nx
            tslsnowfall(i,j) = tsls_threshold
            do k=1,KK(i,j)
              snod_layer(i,j,k) = 0.0
              swed_layer(i,j,k) = 0.0
              ro_layer(i,j,k) = ro_snow
              T_old(i,j,k) = 273.15
              diam_layer(i,j,k) = 0.5 / 1000.0
            enddo

            if (snod_init(i).ge.0.0) then
              KK(i,j) = 1
              snod_layer(i,j,1) = snod_init(i)
              swed_layer(i,j,1) = swed_init(i)
              ro_layer(i,j,1) = sden_init(i)
              T_old(i,j,1) = 273.15
              diam_layer(i,j,1) = 0.5 / 1000.0
            else
              KK(i,j) = 0
            endif

          enddo
        enddo
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE ZERO_SEAICE_SNOW(nx,ny,snow_depth,ro_snow_grid,
     &  ro_snow,swe_depth,swe_depth_old,canopy_int_old,KK,
     &  tslsnowfall,snod_layer,swed_layer,ro_layer,T_old,
     &  multilayer_snowpack,tsls_threshold,seaice_conc,
     &  sum_sprec,sum_trans)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,k

      integer multilayer_snowpack
      integer KK(nx_max,ny_max)

      real tsls_threshold,ro_snow
      real tslsnowfall(nx_max,ny_max)
      real snod_layer(nx_max,ny_max,nz_max)
      real swed_layer(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real seaice_conc(nx_max,ny_max)
      real sum_sprec(nx_max,ny_max)
      real sum_trans(nx_max,ny_max)

      do j=1,ny
        do i=1,nx
          if (seaice_conc(i,j).eq.0.0) then
            canopy_int_old(i,j) = 0.0
            swe_depth_old(i,j) = 0.0
            snow_depth(i,j) = 0.0
            ro_snow_grid(i,j) = ro_snow
            swe_depth(i,j) = 0.0
            sum_sprec(i,j) = 0.0
            sum_trans(i,j) = 0.0
          endif
        enddo
      enddo

      if (multilayer_snowpack.eq.1) then
        do j=1,ny
          do i=1,nx
            if (seaice_conc(i,j).eq.0.0) then
              tslsnowfall(i,j) = tsls_threshold
              do k=1,KK(i,j)
                snod_layer(i,j,k) = 0.0
                swed_layer(i,j,k) = 0.0
                ro_layer(i,j,k) = ro_snow
                T_old(i,j,k) = 273.15
              enddo
              KK(i,j) = 0
            endif
          enddo
        enddo
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE NSNOW_DENSITY_FROM_BLOWING_SNOW(windspd_2m,sprec,dt,
     &  ro_nsnow_wind)

      implicit none

      real alfa,windspd_2m,ro_nsnow_wind,sprec,dt_24hours,dt

c Required constants.
      alfa = 0.2
      dt_24hours = 86400.0

c This new snow density increase due to wind, is only for the single-
c   layer snowpack.
      if (sprec.gt.0.0) then

c To define the offset I assumed under cold conditions (ro_nsnow =
c   50.0 kg/m3):
c   1) 24-hour ave wind speed >= 20 m/s gives ro_nsnow = 350 kg/m3.
c   2) 24-hour ave wind speed = 5 m/s gives ro_nsnow = 150 kg/m3,
c      (actually what really matters here is the density difference
c      of 200 kg/m3 for the wind speed difference from 5 to 20 m/s).
c   3) 24-hour ave wind speed < 5 m/s gives ro_nsnow = ro_nsnow.
c   4) It is appropriate to use an exponential decay function
c      between the 5 and 20 m/s values.
        if (windspd_2m.lt.5.0) then
          ro_nsnow_wind = 0.0
        else
          ro_nsnow_wind = 25.0 +
     &      250.0 * (1.0 - exp(-(alfa*(windspd_2m - 5.0))))

c Now scale this 24-hour value by dt.  I checked, and this is the
c   perfect way to do this.  It makes the 3-hour and 24-hour dt
c   cases identical!
c In the end I decided to not do this scaling because the resulting
c   snow depth is scaled at this time step by the precipitation
c   inputs, and because I think the wind speed is the main
c   controlling factor, not the duration of the wind at this speed.
c         ro_nsnow_wind = dt / dt_24hours * ro_nsnow_wind
        endif

      else

        ro_nsnow_wind = 0.0

      endif

c If you want to turn this off, you can uncomment the following
c   line.
c     ro_nsnow_wind = 0.0

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE WINDSPEED_2M(windspd,ht_windobs,windspd_2m)

      implicit none

      real ht_windobs,snow_z0,windspd,windspd_2m

c Required constants.
      snow_z0 = 0.001

c Calculate the 2-m wind speed.
      windspd_2m = windspd *
     &  log(2.0/snow_z0)/log(ht_windobs/snow_z0)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SUPERIMPOSED_ICE(iyear,output_path_wo_assim,
     &  snod_col_init2,swed_col_init2,sden_col_init1,ro_snow)

c Here I am going to sweep through June and July and determine
c   whether the snowpack ever became isothermal during this
c   period.

      implicit none

      integer k,icount,iend_year,iyear,nnx,nny,i,j,ii,jj

c Number of tracked parcels.
      parameter (icount=70000)

      parameter (nnx=361,nny=361)

      character*80 output_path_wo_assim

c Parcels.
      real roff_col(icount)
      real snod_col(icount)
      real swed_col(icount)
      real conc_col(icount)
      real simp_col(icount)
      real snod_col_init1(icount)
      real snod_col_init2(icount)
      real swed_col_init1(icount)
      real swed_col_init2(icount)
      real sden_col_init1(icount)

      real zi(icount)
      real zj(icount)
      real conc(icount)

c EASE grid.
      real snod1(nnx,nny)
      real snod2(nnx,nny)
      real swed1(nnx,nny)

      integer iyr_start,imo_start,idy_start,imo_end,idy_end,iiyear,
     &  ioptn,julian_end,julian_start,irec,imo_beg,idy_beg,irec1,
     &  irec2,irec3,julian_beg,i_len_wo,trailing_blanks,istart_year

      real undef,ro_snow

c Parcel tracks and concentration from NSIDC.
      character path3*(*) 
      parameter (path3=
     &  '../../parcel_tracks/3_mk_daily/')

c SnowModel run outputs on original parcels.
c     character path1*(*) 
c     parameter (path1 = '/data4/lagrangian/sm_38yrs/merra/outputs/')
      i_len_wo = 80 - trailing_blanks(output_path_wo_assim)

      undef = -9999.0

      iend_year = 2018
      istart_year = 1981

      iiyear = iyear - istart_year + 1

c Open the parcel track data.
      if (iyear.eq.istart_year) then
        open (7021,file=path3//'ij_parcels_1980-2018.gdat',
     &    form='unformatted',access='direct',recl=4*2*icount)

        open (7022,file=path3//'conc_parcels_1980-2018.gdat',
     &    form='unformatted',access='direct',recl=4*1*icount)
      endif

c Input files.
c     open (7031,file=output_path_wo_assim(1:i_len_wo)//'roff.gdat',
c    &  form='unformatted',access='direct',recl=4*icount)

c     open (7032,file=output_path_wo_assim(1:i_len_wo)//'snod.gdat',
c    &  form='unformatted',access='direct',recl=4*icount)

c     open (7033,file=output_path_wo_assim(1:i_len_wo)//'swed.gdat',
c    &  form='unformatted',access='direct',recl=4*icount)

c These have to be closed and reopened so the data are completely
c   available for the following read statements.
      close (234)
      close (236)
      close (238)

      open (234,file=output_path_wo_assim(1:i_len_wo)//'roff.gdat',
     &  form='unformatted',access='direct',recl=4*icount)

      open (236,file=output_path_wo_assim(1:i_len_wo)//'snod.gdat',
     &  form='unformatted',access='direct',recl=4*icount)

      open (238,file=output_path_wo_assim(1:i_len_wo)//'swed.gdat',
     &  form='unformatted',access='direct',recl=4*icount)

c Output files.
      if (iyear.eq.istart_year) then
        open (7051,file='seaice/superimposed_ice_flag_parcel.gdat',
     &    form='unformatted',access='direct',recl=4*3*icount,
     &    status='replace')

        open (7061,file='seaice/superimposed_ice_flag_ease.gdat',
     &    form='unformatted',access='direct',recl=4*2*nnx*nny,
     &    status='replace')

        open (7071,file='seaice/snod_init.gdat',
     &    form='unformatted',access='direct',recl=4*3*icount,
     &    status='replace')
      endif

c Find the irecs between 1 June and 31 July of each year.

c This is the begining of the simulation.
      iyr_start = 1980
      imo_start = 8
      idy_start = 1

c Julian start.
      ioptn = 3
      call calndr (ioptn,idy_start,imo_start,iyr_start,julian_start)

c This is the begining of the summer period.
      imo_beg = 6
      idy_beg = 1

c This is the end of the simulation year.
      imo_end = 7
      idy_end = 31

c Extracting the last day of the simulation year.

c Initialize the array.
      do k=1,icount
        simp_col(k) = undef
      enddo

c Find the irecs for 1 June and 31 July.
      call calndr (ioptn,idy_beg,imo_beg,iyear,julian_beg)
      irec1 = julian_beg - julian_start + 1
      call calndr (ioptn,idy_end,imo_end,iyear,julian_end)
      irec2 = julian_end - julian_start + 1

      print *,iiyear,iyear,irec1,irec2
      print *,iiyear,iyear,irec1,irec2
      print *,iiyear,iyear,irec1,irec2

      do irec=irec1,irec2

c Read in the parcel data.
c       read (7031,rec=irec) (roff_col(k),k=1,icount)
        read (234,rec=irec) (roff_col(k),k=1,icount)
        read (7022,rec=irec) (conc_col(k),k=1,icount)

c Perform some cleanup duties.  This should be done in the
c   SnowModel code before roff is written out.  But right now
c   it is not.
        do k=1,icount
          if (conc_col(k).eq.0.0) roff_col(k) = undef
        enddo

c These runoff data = 0.0 for parcels with snow (or no snow) and
c   zero runoff at this time step, and = undef if there is no
c   parcel (i.e., if conc = 0.0).

c If there is any runoff out the base of the snowpack during
c   June or July, then the snowpack is isothermal and we assume
c   that any remaining snow on this parcel on 31 July is so wet
c   that if and when it eventually re-freezes it will create
c   superimposed ice.
        do k=1,icount
          if (roff_col(k).gt.0.0) then
            simp_col(k) = 1.0
          endif
        enddo

      enddo

c Apply the 'ZERO THE SNOW DEPTH' requirement to the 31 July
c   snow depth.
c     read (7032,rec=irec2) (snod_col(k),k=1,icount)
c     read (7033,rec=irec2) (swed_col(k),k=1,icount)
      read (236,rec=irec2) (snod_col(k),k=1,icount)
      read (238,rec=irec2) (swed_col(k),k=1,icount)

      do k=1,icount
        snod_col_init1(k) = snod_col(k)
        swed_col_init1(k) = swed_col(k)
        if (simp_col(k).eq.1.0) then
          snod_col_init1(k) = 0.0
          swed_col_init1(k) = 0.0
        endif
      enddo

      write (7051,rec=iiyear)
     &  (snod_col_init1(k),k=1,icount),
     &  (snod_col(k),k=1,icount),
     &  (simp_col(k),k=1,icount)

c Grid these data to the EASE grid.
      call parcel_to_ease(nnx,nny,icount,irec2,snod_col_init1,snod1,
     &  undef)
      call parcel_to_ease(nnx,nny,icount,irec2,snod_col,snod2,
     &  undef)
      call parcel_to_ease(nnx,nny,icount,irec2,swed_col_init1,swed1,
     &  undef)

c Save the EASE grid data.
      write (7061,rec=iiyear)
     &  ((snod1(i,j),i=1,nnx),j=1,nny),
     &  ((snod2(i,j),i=1,nnx),j=1,nny)

c Prepare the initial condition snow depth array for 1 Aug.
c   snod1 is my initial condition on the EASE grid.  Use this to
c   define an initial condition on the 1 Aug parcels.  Do this
c   by finding the EASE grid (i,j) position that corresponds to
c   each 1 Aug parcel location.  And then assigning this initial
c   snow depth (and the other corresponding variables) to that
c   parcel.

c NOTE: this is going to crash on the last year!  I need to fix
c   that!
c The +1 here is what is converting the end of year data to a
c   begining of the year IC on the new parcels.
c This is a stupid way to fix that problem.
      if (iyear.ne.iend_year) then
        irec3 = irec2 + 1
      else
        irec3 = irec2
      endif

      read (7021,rec=irec3) (zi(k),k=1,icount),(zj(k),k=1,icount)
      read (7022,rec=irec3) (conc(k),k=1,icount)

      do k=1,icount
        snod_col_init2(k) = 0.0
        swed_col_init2(k) = 0.0
        if (conc(k).ne.0.0) then
          ii = nint(zi(k))
          jj = nint(zj(k))
          if (snod1(ii,jj).ne.undef) then
            snod_col_init2(k) = snod1(ii,jj)
            swed_col_init2(k) = swed1(ii,jj)
          else
            snod_col_init2(k) = 0.0
            swed_col_init2(k) = 0.0
          endif
        endif

        if (snod_col_init2(k).eq.undef) then
          print *,'snod_col_init2(k)=undef',k,conc(k)
        endif
        if (swed_col_init2(k).eq.undef) then
          print *,'swed_col_init2(k)=undef',k,conc(k)
        endif

      enddo

c Calculate the snow density IC by using the swed and snod values.
c   This cleans up the problem of having undef sden when the
c   depths are 0.0.
      do k=1,icount
        if (snod_col_init2(k).ne.0.0 .and.
     &    snod_col_init2(k).ne.undef) then
          sden_col_init1(k) = 1000.0 * swed_col_init2(k) /
     &      snod_col_init2(k)
        else
          sden_col_init1(k) = ro_snow
        endif
      enddo

c Save the snow depth initial condition data.
      write (7071,rec=iiyear) (snod_col_init2(k),k=1,icount),
     &                        (swed_col_init2(k),k=1,icount),
     &                        (sden_col_init1(k),k=1,icount)

c     close (7021)
c     close (7022)
c     close (7031)
c     close (7032)
c     close (7033)
c     close (7051)
c     close (7061)
c     close (7071)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine parcel_to_ease(nnx,nny,icount,iter,snow_col,snow,
     &  undef)

      implicit none

      integer ii,jj,nnx,nny,i,j,k,iter,icount

      real zi(icount),zj(icount),conc(icount)

      real snow_col(icount)
      real snow(nnx,nny)

      real cnt(nnx,nny)
      real conc_area_sum(nnx,nny)

      real undef,zii,zjj
      real center_x1,center_y1,center_x2,center_y2,area

c Read in the parcel track data.
      read (7021,rec=iter) (zi(k),k=1,icount),(zj(k),k=1,icount)
      read (7022,rec=iter) (conc(k),k=1,icount)

c Make any 0.0 concentrations undefined, and convert them
c   from 0-100 to 0-1.
      do k=1,icount
        conc(k) = conc(k) / 100.0
        if (conc(k).eq.0.0) conc(k) = undef
      enddo

c Initialize the working arrays.
      do j=1,nny
        do i=1,nnx

c Snow depth.
          snow(i,j) = 0.0

c Counting array.
          cnt(i,j) = 0.0

c Concentration-area product summing array.
          conc_area_sum(i,j) = 0.0

        enddo
      enddo

      do k=1,icount
        if (conc(k).ne.undef) then

c Decimal coordinate of the center of the parcel.
          center_x2 = zi(k)
          center_y2 = zj(k)

c Coordinate of the EASE grid cell center that this parcel
c   center sits in, integer and real versions.
          ii = nint(zi(k))
          jj = nint(zj(k))

c This eliminates any problems at the boundaries, if you have
c   a situation where there is non-zero conc on the boundary
c   grid cells.
          if (ii.eq.1) ii = 2
          if (ii.eq.nnx) ii = nnx-1
          if (jj.eq.1) jj = 2
          if (jj.eq.nny) jj = nny-1

          zii = real(ii)
          zjj = real(jj)

c Loop through the 3x3 grid cells surrounding the EASE grid cell.
          do j=jj-1,jj+1
            do i=ii-1,ii+1

c Define the center coords of the surrounding EASE grid cells.
              center_x1 = real(i)
              center_y1 = real(j)

c Find the overlap in fractional area of each EASE grid cell.  So,
c   the "area" coming out of this is: 0-1 = the fraction of the
c   25-km by 25-km EASE grid cell that this parcel covers.
              call overlap_area (center_x1,center_y1,center_x2,
     &          center_y2,area)

c Use the concentration to weight the snow depth under the
c   assumption that the concentration is proportional to the
c   contributing area.
              area = area * conc(k)

c Count how many parcels contribute to each EASE grid cell.
              if (area.gt.0.0) then
                cnt(i,j) = cnt(i,j) + 1.0
              endif

c Keep track of the concentrations that sum to over 1.0.  They
c   will be used to scale the result at the end of the averaging
c   calculations.
              conc_area_sum(i,j) = conc_area_sum(i,j) + area

c Sum the snow depth contributions from each parcel in each EASE
c   grid cell, by scaling the depth values by the area and conc.
              snow(i,j) = snow(i,j) + area * snow_col(k)

            enddo
          enddo

        endif
      enddo

c Take care of the problem where the area-concentrations summed
c   to over 1.0.
      do j=1,nny
        do i=1,nnx

c Clip the values below 1.0 so when you do the divide, only
c   concentration sums above 1.0 scale the result (values below
c   1.0 are already grid-averaged values).
          conc_area_sum(i,j) = max(1.0,conc_area_sum(i,j))

c Scale the snow depth averages for cases where the concentration
c   summed to greater than 1.0.
          snow(i,j) = snow(i,j) / conc_area_sum(i,j)

        enddo
      enddo

c Clean up any grid cells that never had any ice in them.
      do j=1,nny
        do i=1,nnx
          if (cnt(i,j).eq.0.0) then
            snow(i,j) = undef
          endif
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine overlap_area (center_x1,center_y1,center_x2,
     & center_y2,area)

c Here center_x1,y1 is the EASE grid cell, and center_x2,y2 is
c   the parcel cell.

      implicit none

      real center_x1,center_y1
      real center_x2,center_y2
      real x1_min,x1_max,y1_min,y1_max
      real x2_min,x2_max,y2_min,y2_max
      real x_overlap,y_overlap,area

c These are the boundary coords of the EASE grid cell.
      x1_min = center_x1 - 0.5
      x1_max = center_x1 + 0.5
      y1_min = center_y1 - 0.5
      y1_max = center_y1 + 0.5

c These are the boundary coords of the parcel.
      x2_min = center_x2 - 0.5
      x2_max = center_x2 + 0.5
      y2_min = center_y2 - 0.5
      y2_max = center_y2 + 0.5

c Define the overlap in x and y.
      x_overlap = max(0.0,min(x1_max,x2_max) - max(x1_min,x2_min))
      y_overlap = max(0.0,min(y1_max,y2_max) - max(y1_min,y2_min))

c Calculate the area overlap.
      area = x_overlap * y_overlap

c     print 98, x_overlap,y_overlap,area
c  98 format (3f10.3)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c snowtran_code.f

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccc     Snow-Transport Modeling System - 3D (SnowTran-3D)    cccccc
ccccc                    Copyright (C) 1998                    cccccc
ccccc          by Glen E. Liston, InterWorks Consulting        cccccc
ccccc                    All Rights Reserved                   cccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c This FORTRAN code receives inputs of wind speed, wind direction,
c   air temperature, relative humidity, vegetation type, topography,
c   and precipitation, and it outputs snow depth, saltation flux,
c   suspended flux, sublimation of blowing snow, and the snow depth
c   changes resulting from these processes.
c
c All units are in m, kg, s, K.
c
c This model is described in the paper:
c   A Snow-Transport Model for Complex Terrain, by Glen E. Liston
c   and Matthew Sturm, Journal of Glaciology, 1998, Vol. 44,
c   No. 148, pages 498-516.
c
c The author of this code is:
c   Dr. Glen E. Liston
c   InterWorks Consulting
c   15621 SnowMan Road
c   Loveland, Colorado 80538
c
c To run in 2-D mode, set nx = 3 and look at the data at i = 2.
c   This is required because of the boundary conditions imposed
c   along i = 1 and 3.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine SNOWTRAN_CODE(bc_flag,bs_flag,C_z,
     &  conc_salt,deltax,deltay,dh_salt,dh_salt_u,dh_salt_v,
     &  dh_susp,dh_susp_u,dh_susp_v,dt,dz_susp,fall_vel,fetch,
     &  gravity,h_const,h_star,ht_rhobs,ht_windobs,index_ue,
     &  index_uw,index_vn,index_vs,iter,nx,ny,pi,Qsalt,Qsalt_max,
     &  Qsalt_maxu,Qsalt_maxv,Qsalt_u,Qsalt_v,Qsubl,Qsusp,
     &  Qsusp_u,Qsusp_v,rh_grid,ro_air,ro_snow,ro_water,snow_d,
     &  snow_d_init,snow_z0,soft_snow_d,sprec,sum_glacmelt,
     &  subgrid_flag,wbal_salt,wbal_susp,wbal_qsubl,sum_sprec,
     &  tabler_ee,tabler_ne,tabler_nn,tabler_nw,tabler_se,
     &  tabler_ss,tabler_sw,tabler_ww,tair_grid,topo,topo_land,
     &  topoflag,twolayer_flag,Up_const,Ur_const,Utau,
     &  Utau_t,uwind_grid,veg_z0,vegsnowd_xy,vegtype,vonKarman,
     &  vwind_grid,wind_min,winddir_flag,winddir_grid,
     &  windspd_flag,windspd_grid,xmu,z_0,ztop_susp,max_iter,
     &  run_enbal,run_snowpack,wbal_subgrid,sum_qsubl,sum_trans,
     &  swe_depth,snow_depth,ro_snow_grid,sum_prec,sum_runoff,
     &  sum_Qcs,canopy_int,w_balance,sum_sfcsublim,tabler_dir,
     &  slope_adjust,Utau_t_const,Utau_t_flag,ro_soft_snow_old,
     &  ro_soft_snow,ro_nsnow,prec,Qcs,runoff,d_canopy_int,
     &  glacier_melt,swe_depth_old,swesublim,canopy_unload,
     &  canopy_int_old,iter_start,multilayer_snowpack,swed_layer,
     &  KK,snod_layer,ro_layer,curve_lg_scale_flag,curve_wt_lg,
     &  seaice_run,seaice_conc,tslsnowfall,T_old,tsls_threshold,
     &  curve_len_scale,Tabler_1_flag,Tabler_2_flag,undef,
     &  tabler_sfc_path_name,output_path_wo_assim,
     &  output_path_wi_assim,icorr_factor_loop,windspd_2m_grid,
     &  Qsubl_depth)

      implicit none

      include 'snowmodel.inc'

      integer iter,nx,ny,i,j,iter_start,max_iter

      real ro_snow,ro_water,ro_air,gravity,vonKarman,snow_z0
      real deltax,deltay,dt,undef
      real fetch,xmu,C_z,h_const,wind_min,windspd_flag
      real Up_const,dz_susp,ztop_susp,fall_vel,Ur_const
      real Utau_t_const,pi,bc_flag,topoflag,Utau_t_flag
      real ht_windobs,ht_rhobs,bs_flag,twolayer_flag
      real subgrid_flag,winddir_flag,curve_len_scale
      real run_enbal,run_snowpack,tabler_dir,slope_adjust

      real topo_land(nx_max,ny_max)
      real tabler_nn(nx_max,ny_max)
      real tabler_ss(nx_max,ny_max)
      real tabler_ee(nx_max,ny_max)
      real tabler_ww(nx_max,ny_max)
      real tabler_ne(nx_max,ny_max)
      real tabler_se(nx_max,ny_max)
      real tabler_sw(nx_max,ny_max)
      real tabler_nw(nx_max,ny_max)
      real topo(nx_max,ny_max)
      real vegtype(nx_max,ny_max)

      real tabler_nn_orig(nx_max,ny_max)
      real tabler_ss_orig(nx_max,ny_max)
      real tabler_ee_orig(nx_max,ny_max)
      real tabler_ww_orig(nx_max,ny_max)
      real tabler_ne_orig(nx_max,ny_max)
      real tabler_se_orig(nx_max,ny_max)
      real tabler_sw_orig(nx_max,ny_max)
      real tabler_nw_orig(nx_max,ny_max)
      real snow_d_tabler(nx_max,ny_max)
      real topo_tmp(nx_max,ny_max)

      real uwind_grid(nx_max,ny_max),vwind_grid(nx_max,ny_max)
      real windspd_grid(nx_max,ny_max),winddir_grid(nx_max,ny_max)
      real tair_grid(nx_max,ny_max),sprec(nx_max,ny_max)
      real rh_grid(nx_max,ny_max),windspd_2m_grid(nx_max,ny_max)

      integer index_ue(ny_max,2*nx_max+1),index_uw(ny_max,2*nx_max+1)
      integer index_vn(nx_max,2*ny_max+1),index_vs(nx_max,2*ny_max+1)

      real snow_d(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real ro_soft_snow(nx_max,ny_max)
      real ro_soft_snow_old(nx_max,ny_max)
      real ro_nsnow(nx_max,ny_max)
      real snow_d_init(nx_max,ny_max)
      real Utau(nx_max,ny_max)
      real Utau_t(nx_max,ny_max)
      real z_0(nx_max,ny_max)
      real h_star(nx_max,ny_max)
      real conc_salt(nx_max,ny_max)

      real Qsalt_max(nx_max,ny_max)
      real Qsalt_maxu(nx_max,ny_max),Qsalt_maxv(nx_max,ny_max)
      real Qsalt(nx_max,ny_max)
      real Qsalt_u(nx_max,ny_max),Qsalt_v(nx_max,ny_max)
      real dh_salt(nx_max,ny_max)
      real dh_salt_u(nx_max,ny_max),dh_salt_v(nx_max,ny_max)

      real Qsusp(nx_max,ny_max)
      real Qsusp_u(nx_max,ny_max),Qsusp_v(nx_max,ny_max)
      real dh_susp(nx_max,ny_max)
      real dh_susp_u(nx_max,ny_max),dh_susp_v(nx_max,ny_max)

      real dh_subgrid(nx_max,ny_max)
      real Qsubl(nx_max,ny_max)
      real Qsubl_depth(nx_max,ny_max)

      real sum_sprec(nx_max,ny_max)
      real wbal_qsubl(nx_max,ny_max)
      real wbal_salt(nx_max,ny_max)
      real wbal_susp(nx_max,ny_max)
      real wbal_subgrid(nx_max,ny_max)
      real sum_qsubl(nx_max,ny_max)
      real sum_trans(nx_max,ny_max)
      real soft_snow_d(nx_max,ny_max)

      real prec(nx_max,ny_max)
      real Qcs(nx_max,ny_max)
      real runoff(nx_max,ny_max)
      real d_canopy_int(nx_max,ny_max)
      real glacier_melt(nx_max,ny_max)
      real swe_depth_old(nx_max,ny_max)
      real swesublim(nx_max,ny_max)
      real canopy_unload(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)

      real vegsnowd_xy(nx_max,ny_max)
      real veg_z0(nx_max,ny_max)

      real sum_glacmelt(nx_max,ny_max),w_balance(nx_max,ny_max),
     &  sum_prec(nx_max,ny_max),sum_runoff(nx_max,ny_max),
     &  sum_Qcs(nx_max,ny_max),canopy_int(nx_max,ny_max),
     &  sum_sfcsublim(nx_max,ny_max)

      integer multilayer_snowpack,k
      integer KK(nx_max,ny_max)
      real swe_change_tmp,swe_change,tsls_threshold
      real swed_layer_z(nz_max)
      real swed_layer(nx_max,ny_max,nz_max)
      real snod_layer(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real tslsnowfall(nx_max,ny_max)

      real curve_lg_scale_flag
      real curve_wt_lg(nx_max,ny_max)

      real seaice_run
      real seaice_conc(nx_max,ny_max)

      real Tabler_1_flag,Tabler_2_flag
      character*80 output_path_wo_assim,output_path_wi_assim,
     &  tabler_sfc_path_name
      integer i_len_tabler,trailing_blanks,icorr_factor_loop,
     &  i_len_wo,i_len_wi

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Perform some intialization steps that are unique to SnowTran-3D.
      if (iter.eq.iter_start) then

c This is now done in preprocess_code.f
c       print *,
c    & 'cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
c       print *,
c    & 'c     Snow-Transport Modeling System - 3D (SnowTran-3D)    c'
c       print *,
c    & 'c                    Copyright (C) 1998                    c'
c       print *,
c    & 'c          by Glen E. Liston, InterWorks Consulting        c'
c       print *,
c    & 'c                    All Rights Reserved                   c'
c       print *,
c    & 'cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'

c SnowTran-3D can only be run with SnowPack and EnBal turned on.
        if (run_enbal.ne.1.0 .and. run_snowpack.ne.1.0) then
          print *,'You cannot run SnowTran-3D without'
          print *,'SnowPack and EnBal anymore.'
          stop
        endif

        if (subgrid_flag.eq.1.0) then

c Check to make sure topoflag = 1.0.
          if (subgrid_flag.eq.1.0 .and. topoflag.eq.0.0) then
            print *,'If subgrid_flag=1.0, then topoflag must = 1.0'
            print *,'  Correct this in snowmodel.par to continue.'
            stop
          endif

c The Tabler surfaces were originally developed assuming the grid
c   increment would never be less than 1.0 m.  You probably should
c   not run it with deltax and deltay less than 1.0 without some
c   further testing.
          if (deltax.lt.1.0 .or. deltay.lt.1.0) then
            print *,'The Tabler subgrid algorithm has not been'
            print *,'tested for deltax and/or deltay less than'
            print *,'1.0 m.  You should probably do some testing'
            print *,'before running the model at less than 1.0-m'
            print *,'resolution.  Acually I am pretty sure it will'
            print *,'run, but it will not generate the correct'
            print *,'snow-depth profiles.'
            stop
          endif

c If this is the first time through, generate the Tabler snow
c   accumulation surfaces for the land topography.
          call tabler_3d(nx,ny,topo_land,deltax,deltay,
     &      tabler_ww_orig,tabler_ee_orig,tabler_ss_orig,
     &      tabler_nn_orig,tabler_ne_orig,tabler_se_orig,
     &      tabler_sw_orig,tabler_nw_orig,slope_adjust)

c Include the vegetation snow-holding depths in the Tabler
c   surfaces.
          do i=1,nx
            do j=1,ny
              tabler_nn_orig(i,j) =
     &          max(tabler_nn_orig(i,j),vegsnowd_xy(i,j))
              tabler_ne_orig(i,j) =
     &          max(tabler_ne_orig(i,j),vegsnowd_xy(i,j))
              tabler_ee_orig(i,j) =
     &          max(tabler_ee_orig(i,j),vegsnowd_xy(i,j))
              tabler_se_orig(i,j) =
     &          max(tabler_se_orig(i,j),vegsnowd_xy(i,j))
              tabler_ss_orig(i,j) =
     &          max(tabler_ss_orig(i,j),vegsnowd_xy(i,j))
              tabler_sw_orig(i,j) =
     &          max(tabler_sw_orig(i,j),vegsnowd_xy(i,j))
              tabler_ww_orig(i,j) =
     &          max(tabler_ww_orig(i,j),vegsnowd_xy(i,j))
              tabler_nw_orig(i,j) =
     &          max(tabler_nw_orig(i,j),vegsnowd_xy(i,j))
            enddo
          enddo

c Save the Tabler equilibrium surfaces for each of the 8 main wind
c   directions.  Also save the land DEM distribution.
          if (Tabler_1_flag.eq.1.0) then
            i_len_tabler = 80 - trailing_blanks(tabler_sfc_path_name)
            open(51,
     &    file=tabler_sfc_path_name(1:i_len_tabler)//'tabler_sfcs.gdat',
     &        form='unformatted',access='direct',recl=4*nx*ny)
            write(51,rec=1) ((tabler_nn_orig(i,j),i=1,nx),j=1,ny)
            write(51,rec=2) ((tabler_ne_orig(i,j),i=1,nx),j=1,ny)
            write(51,rec=3) ((tabler_ee_orig(i,j),i=1,nx),j=1,ny)
            write(51,rec=4) ((tabler_se_orig(i,j),i=1,nx),j=1,ny)
            write(51,rec=5) ((tabler_ss_orig(i,j),i=1,nx),j=1,ny)
            write(51,rec=6) ((tabler_sw_orig(i,j),i=1,nx),j=1,ny)
            write(51,rec=7) ((tabler_ww_orig(i,j),i=1,nx),j=1,ny)
            write(51,rec=8) ((tabler_nw_orig(i,j),i=1,nx),j=1,ny)
            write(51,rec=9) ((topo_land(i,j),i=1,nx),j=1,ny)
            close (51)
          endif

c This available if you want to save the Tabler surface that was
c   used at each time step.
          if (Tabler_2_flag.eq.1.0) then
            if (icorr_factor_loop.eq.1) then
              i_len_wo = 80 - trailing_blanks(output_path_wo_assim)
              open(52,
     &  file=output_path_wo_assim(1:i_len_wo)//'tabler_sfcs_iter.gdat',
     &        form='unformatted',access='direct',recl=4*nx*ny)
            endif
            if (icorr_factor_loop.eq.2) then
              i_len_wi = 80 - trailing_blanks(output_path_wi_assim)
              open(52,
     &  file=output_path_wi_assim(1:i_len_wi)//'tabler_sfcs_iter.gdat',
     &        form='unformatted',access='direct',recl=4*nx*ny)
            endif
          endif
        endif

      endif

c Print out some basic run information to the screen.
c     print 102, windspd_flag,winddir_flag
c 102 format(25x,'    wind spd = ',f5.2,'   wind dir = ',f4.0)

c In SnowTran-3D, the summed snow precipitation must be in units
c   of snow-depth.  The rest of the routines assume that it is in
c   swe units.
      do j=1,ny
        do i=1,nx
          sum_sprec(i,j) = sum_sprec(i,j) * ro_water / ro_snow
        enddo
      enddo

c Update the threshold friction velocity.
      if (Utau_t_flag.eq.0.0) then
        if (curve_lg_scale_flag.eq.1.0) then
          do j=1,ny
            do i=1,nx
              Utau_t(i,j) = curve_wt_lg(i,j) * Utau_t_const
            enddo
          enddo
        else
          do j=1,ny
            do i=1,nx
              Utau_t(i,j) = Utau_t_const
            enddo
          enddo
        endif
      elseif (Utau_t_flag.eq.1.0) then
        do j=1,ny
          do i=1,nx
            call surface_snow_1(tair_grid(i,j),windspd_2m_grid(i,j),
     &        sprec(i,j),ro_soft_snow(i,j),Utau_t(i,j),
     &        ro_soft_snow_old(i,j),dt,ro_nsnow(i,j))
          enddo
        enddo
        if (curve_lg_scale_flag.eq.1.0) then
          do j=1,ny
            do i=1,nx
              Utau_t(i,j) = curve_wt_lg(i,j) * Utau_t(i,j)
            enddo
          enddo
        endif
      endif

c Set the blowing snow flag to zero until it is clear that we will
c   have blowing snow.
      bs_flag = 0.0

c If the wind speed is lower that some threshold, then don't
c   need to to any of the snow transport computations.
      if (windspd_flag.ge.4.0) then

c Get the wind direction indexing arrays for this particular
c   wind event (time step).
        call getdirection(nx,ny,uwind_grid,vwind_grid,index_ue,
     &    index_uw,index_vn,index_vs)

c Solve for Utau and z_0 if snow is saltating, else solve assuming
c   z_0 is known from snow depth and/or veg type, and solve for
c   Utau.
        call solveUtau(Utau,ht_windobs,windspd_grid,C_z,vonKarman,
     &    gravity,z_0,h_star,h_const,vegsnowd_xy,snow_d,
     &    snow_z0,veg_z0,bs_flag,nx,ny,Utau_t,soft_snow_d)

c If the blowing snow flag indicates wind transported snow
c   somewhere within the domain (bs_flag = 1.0), run the saltation
c   and suspension models.
        if (bs_flag.eq.1.0) then

c Solve for the saltation flux.
c         print *,'         Saltation'
          call saltation(Qsalt,deltax,fetch,Utau,Utau_t,nx,ny,
     &      ro_air,gravity,vegsnowd_xy,snow_d,
     &      Qsalt_max,Qsalt_maxu,Qsalt_maxv,deltay,Qsalt_u,Qsalt_v,
     &      index_ue,index_uw,index_vn,index_vs,uwind_grid,
     &      vwind_grid,xmu,soft_snow_d,bc_flag)

c Solve for the suspension flux.
c         print *,'         Suspension'
          call suspension(Utau,vonKarman,nx,ny,conc_salt,
     &      Qsalt,Qsusp,z_0,h_star,dz_susp,ztop_susp,pi,
     &      fall_vel,Ur_const,Up_const,Utau_t,Qsubl,ht_rhobs,
     &      tair_grid,rh_grid,Qsusp_u,Qsusp_v,uwind_grid,
     &      vwind_grid)

        elseif (bs_flag.eq.0.0) then

          call noblowsnow(nx,ny,Qsalt_max,Qsalt_maxu,
     &      Qsalt_maxv,Qsalt,Qsalt_u,Qsalt_v,dh_salt,dh_salt_u,
     &      dh_salt_v,conc_salt,Qsusp,Qsusp_u,Qsusp_v,dh_susp,
     &      dh_susp_u,dh_susp_v,Qsubl,dh_subgrid)

        endif

      else

c This 'noblowsnow' call zeros out data from a previous time step
c   that had blowing snow.
        call noblowsnow(nx,ny,Qsalt_max,Qsalt_maxu,
     &    Qsalt_maxv,Qsalt,Qsalt_u,Qsalt_v,dh_salt,dh_salt_u,
     &    dh_salt_v,conc_salt,Qsusp,Qsusp_u,Qsusp_v,dh_susp,
     &    dh_susp_u,dh_susp_v,Qsubl,dh_subgrid)

      endif

c If this is a normal SnowTran-3D run, adjust the accumulations
c   and erosions in reponse to varying transport fluxes across
c   the simulation domain.  If it is a sea ice run with 25-km
c   grid cells, just adjust the snow depth in response to the
c   blowing snow sublimation fluxes only.

      if (seaice_run.eq.1.0 .and. deltax.eq.25000.0) then

c Here Qsubl goes in as a flux, and comes out in snow depth units.
c   And Qsubl_depth comes out in swe depth units.
        call bs_sublimation_only(nx,ny,dt,bs_flag,ro_water,
     &    snow_depth,swe_depth,ro_snow_grid,soft_snow_d,Qsubl,
     &    vegsnowd_xy,Qsubl_depth)

      elseif (seaice_run.eq.4.0 .and. deltax.eq.25000.0) then

c Here Qsubl goes in as a flux, and comes out in snow depth units.
c   And Qsubl_depth comes out in swe depth units.
        call bs_sublimation_only(nx,ny,dt,bs_flag,ro_water,
     &    snow_depth,swe_depth,ro_snow_grid,soft_snow_d,Qsubl,
     &    vegsnowd_xy,Qsubl_depth)

      else

c Compute the new snow depth due to accumulation from precipitation,
c   saltation, and suspension, and the mass loss due to
c   sublimation.
        call accum(snow_d,nx,ny,ro_snow,dt,ro_water,
     &    deltax,deltay,vegsnowd_xy,Tabler_2_flag,
     &    index_ue,index_uw,index_vn,index_vs,undef,
     &    Qsalt_u,Qsalt_v,Qsusp_u,Qsusp_v,Qsubl,dh_salt,
     &    dh_salt_u,dh_salt_v,dh_susp,dh_susp_u,dh_susp_v,
     &    wbal_qsubl,wbal_salt,wbal_susp,bs_flag,
     &    soft_snow_d,topo,topo_land,topoflag,subgrid_flag,
     &    tabler_nn,tabler_ss,tabler_ee,tabler_ww,
     &    tabler_ne,tabler_se,tabler_sw,tabler_nw,
     &    uwind_grid,vwind_grid,wbal_subgrid,sum_qsubl,
     &    sum_trans,swe_depth,snow_depth,ro_snow_grid,
     &    dh_subgrid,tabler_dir,iter,slope_adjust,
     &    curve_len_scale)

      endif

c Use the changes in swe due to saltation, suspension, and
c   blowing snow sublimation to adjust the multilayer snowpack
c   layers.
      if (multilayer_snowpack.eq.1) then

        if (seaice_run.ne.0.0) then
          do j=1,ny
            do i=1,nx
c This is the sublimation in terms of swe depth.
              wbal_qsubl(i,j) = Qsubl(i,j) * ro_snow_grid(i,j) /
     &          ro_water
c Because these simulations are done over grid cells that are
c   around 25-km by 25-km, subgrid blowing snow processes are not
c   simulated.
              wbal_salt(i,j) = 0.0
              wbal_susp(i,j) = 0.0
              wbal_subgrid(i,j) = 0.0
            enddo
          enddo
        endif

        do j=1,ny
          do i=1,nx
            swe_change = wbal_qsubl(i,j) + wbal_salt(i,j) +
     &        wbal_susp(i,j) + wbal_subgrid(i,j)

c Net mass loss for this grid cell at this time step.
            if (swe_change.lt.0.0) then
              swe_change_tmp = -swe_change

c Extract the vertical column for this i,j point, and send it
c   to the subroutine. *** Note that I should use f95, then I would
c   not have to do this (I could pass in subsections of the arrays).
              do k=1,nz_max
                swed_layer_z(k) = swed_layer(i,j,k)
              enddo

c Check to see whether a layer reduction is required.
              CALL REDUCE_LAYERS(swe_change_tmp,swed_layer_z,KK(i,j))

c Re-build the 3-D array.  See note above about using f95 to avoid this.
              do k=1,nz_max
                swed_layer(i,j,k) = swed_layer_z(k)
              enddo

c Update the snow layer thicknesses, and recalculate the total
c   snow and swe depths.  Assume this swe change does not change
c   the snow density and does not change the soft snow depth.  It
c   only reduces the snow depth and the associated swe depth.
              snow_depth(i,j) = 0.0
              swe_depth(i,j) = 0.0
              do k=1,KK(i,j)
                snod_layer(i,j,k) = swed_layer(i,j,k) * ro_water /
     &            ro_layer(i,j,k)
c               ro_layer(i,j,k) = ro_layer(i,j,k)
                snow_depth(i,j) = snow_depth(i,j) + snod_layer(i,j,k)
                swe_depth(i,j) = swe_depth(i,j) + swed_layer(i,j,k)
              enddo

c Net mass gain for this grid cell at this time step.
            elseif (swe_change.gt.0.0) then

c Add to the existing top layer.
              swed_layer(i,j,KK(i,j)) = swed_layer(i,j,KK(i,j)) +
     &          swe_change
c             ro_layer(i,j,k) = ro_layer(i,j,k)
              snod_layer(i,j,KK(i,j)) = swed_layer(i,j,KK(i,j)) *
     &          ro_water / ro_layer(i,j,KK(i,j))

c Update the snow layer thicknesses, and recalculate the total
c   snow and swe depths.  Assume this swe change does not change
c   the snow density and does not change the soft snow depth.  It
c   only reduces the snow depth and the associated swe depth.
              snow_depth(i,j) = 0.0
              swe_depth(i,j) = 0.0
              do k=1,KK(i,j)
                snow_depth(i,j) = snow_depth(i,j) + snod_layer(i,j,k)
                swe_depth(i,j) = swe_depth(i,j) + swed_layer(i,j,k)
              enddo

            else

              snow_depth(i,j) = 0.0
              swe_depth(i,j) = 0.0
              do k=1,KK(i,j)
                snow_depth(i,j) = snow_depth(i,j) + snod_layer(i,j,k)
                swe_depth(i,j) = swe_depth(i,j) + swed_layer(i,j,k)
              enddo

            endif

          enddo
        enddo
      endif

c Perform a water balance check (see notes in this subroutine).
      if (seaice_run.eq.0.0) then

c Don't do this calculation for subgrid_flag = 1.0.  This can be
c   turned back on and the required adjustments made to the code,
c   if needed.
c       if (subgrid_flag.eq.0.0) then
c         call waterbal_snowtran(w_balance,prec,Qcs,
c    &      runoff,d_canopy_int,swe_depth,glacier_melt,iter,
c    &      wbal_qsubl,wbal_salt,wbal_susp,wbal_subgrid,nx,ny,
c    &      swe_depth_old,swesublim,canopy_unload,canopy_int,
c    &      canopy_int_old)
c       endif
      endif

c If this is a sea ice run, zero out the ocean grid cells that
c   have no sea ice in them.
      if (seaice_run.ne.0.0) then
        CALL ZERO_SEAICE_SNOW(nx,ny,snow_depth,ro_snow_grid,
     &    ro_snow,swe_depth,swe_depth_old,canopy_int_old,KK,
     &    tslsnowfall,snod_layer,swed_layer,ro_layer,T_old,
     &    multilayer_snowpack,tsls_threshold,seaice_conc,
     &    sum_sprec,sum_trans)
      endif

c Save the mass balance variables from this time step.
      do j=1,ny
        do i=1,nx
          swe_depth_old(i,j) = swe_depth(i,j)
          canopy_int_old(i,j) = canopy_int(i,j)
        enddo
      enddo

c In SnowTran-3D, the summed snow precipitation were in units
c   of snow-depth.  The rest of the routines assume that it is in
c   swe units.
      do j=1,ny
        do i=1,nx
          sum_sprec(i,j) = sum_sprec(i,j) * ro_snow / ro_water
        enddo
      enddo

c Close the Tabler surface output file if this is the end of this
c   assimulation loop.
      if (iter.eq.max_iter) close(52)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine waterbal_snowtran(w_balance,prec,Qcs,
     &  runoff,d_canopy_int,swe_depth,glacier_melt,iter,
     &  wbal_qsubl,wbal_salt,wbal_susp,wbal_subgrid,nx,ny,
     &  swe_depth_old,swesublim,canopy_unload,canopy_int,
     &  canopy_int_old)

      implicit none

      include 'snowmodel.inc'

      integer iter,nx,ny,i,j

      real w_balance(nx_max,ny_max),prec(nx_max,ny_max),
     &  Qcs(nx_max,ny_max),runoff(nx_max,ny_max),
     &  d_canopy_int(nx_max,ny_max),swe_depth(nx_max,ny_max),
     &  glacier_melt(nx_max,ny_max),wbal_qsubl(nx_max,ny_max),
     &  wbal_salt(nx_max,ny_max),swe_depth_old(nx_max,ny_max),
     &  swesublim(nx_max,ny_max),wbal_susp(nx_max,ny_max),
     &  wbal_subgrid(nx_max,ny_max),canopy_unload(nx_max,ny_max),
     &  canopy_int_old(nx_max,ny_max),canopy_int(nx_max,ny_max)

c Note that the following balances should hold.  These aren't quite
c   right, but it is a place to start.
c   Canopy Balance (forest):
c     canopy = sprec - unload + Qcs ==> unload = sprec - canopy + Qcs
c
c   Snowpack Balance (forest):
c     swe_d = unload + rain - runoff ==>
c       canopy + swe_d = sprec + rain + Qcs - runoff
c     prec = sprec + rain
c     sum_rain  = sum_sprec - sum_prec
c
c   Snowpack Balance (non-forest):
c     swe_d = sprec + rain - runoff + subl + salt + susp + subgrid +
c       glaciermelt
c
c   Everywhere:
c     w_balance = sum_prec + sum_Qcs - sum_runoff + sum_subl +
c       sum_trans - canopy_int - swe_depth + sum_glacmelt
c
c   The related variables that would need to be brought in are:
c      d_canopy_int,sum_d_canopy_int,sum_unload

c The subroutine WATERBAL_SNOWTRAN is used if the model simulation
c   includes SnowTran-3D.
      do j=1,ny
        do i=1,nx
          w_balance(i,j) = swe_depth_old(i,j) - swe_depth(i,j) +
     &      prec(i,j) - runoff(i,j) + glacier_melt(i,j) +
     &      wbal_qsubl(i,j) + wbal_salt(i,j) + wbal_susp(i,j) +
     &      wbal_subgrid(i,j) - swesublim(i,j) + canopy_int_old(i,j) -
     &      canopy_int(i,j) + Qcs(i,j)

          if (abs(w_balance(i,j)).gt.1.0e-5)
     &      print*,'water imbalance at iter =',iter,' ',w_balance(i,j)

        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine noblowsnow(nx,ny,Qsalt_max,Qsalt_maxu,
     &  Qsalt_maxv,Qsalt,Qsalt_u,Qsalt_v,dh_salt,dh_salt_u,
     &  dh_salt_v,conc_salt,Qsusp,Qsusp_u,Qsusp_v,dh_susp,
     &  dh_susp_u,dh_susp_v,Qsubl,dh_subgrid)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j
      real Qsalt_max(nx_max,ny_max)
      real Qsalt_maxu(nx_max,ny_max),Qsalt_maxv(nx_max,ny_max)
      real Qsalt(nx_max,ny_max)
      real Qsalt_u(nx_max,ny_max),Qsalt_v(nx_max,ny_max)
      real dh_salt(nx_max,ny_max)
      real dh_salt_u(nx_max,ny_max),dh_salt_v(nx_max,ny_max)

      real conc_salt(nx_max,ny_max)

      real Qsusp(nx_max,ny_max)
      real Qsusp_u(nx_max,ny_max),Qsusp_v(nx_max,ny_max)
      real dh_susp(nx_max,ny_max)
      real dh_susp_u(nx_max,ny_max),dh_susp_v(nx_max,ny_max)
      real dh_subgrid(nx_max,ny_max)

      real Qsubl(nx_max,ny_max)

      do i=1,nx
        do j=1,ny
          Qsalt_max(i,j) = 0.0
          Qsalt_maxu(i,j) = 0.0
          Qsalt_maxv(i,j) = 0.0
          Qsalt(i,j) = 0.0
          Qsalt_u(i,j) = 0.0
          Qsalt_v(i,j) = 0.0
          dh_salt(i,j) = 0.0
          dh_salt_u(i,j) = 0.0
          dh_salt_v(i,j) = 0.0
          conc_salt(i,j) = 0.0
          Qsusp(i,j) = 0.0
          Qsusp_u(i,j) = 0.0
          Qsusp_v(i,j) = 0.0
          dh_susp(i,j) = 0.0
          dh_susp_u(i,j) = 0.0
          dh_susp_v(i,j) = 0.0
          Qsubl(i,j) = 0.0
          dh_subgrid(i,j) = 0.0
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine bs_sublimation_only(nx,ny,dt,bs_flag,ro_water,
     &  snow_depth,swe_depth,ro_snow_grid,soft_snow_d,Qsubl,
     &  vegsnowd_xy,Qsubl_depth)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny

      real dt,bs_flag,ro_water,snowdmin,hard_snow_d

      real snow_depth(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)

      real soft_snow_d(nx_max,ny_max)
      real Qsubl(nx_max,ny_max)
      real Qsubl_depth(nx_max,ny_max)

      real vegsnowd_xy(nx_max,ny_max)

c Adjust the snow and swe depths to account for blowing-snow
c   sublimation.

      if (bs_flag.eq.1.0) then

c SUBLIMATION
c Make adjustments for the case where there is no snow available
c   on the ground (or captured within the vegetation) to be
c   eroded.  Since Qsubl is blowing snow sublimation, don't let
c   this sublimation reach down into the hard snow layer or into
c   the vegsnowd, because that snow is not available to be blown
c   around.
        do i=1,nx
          do j=1,ny
            hard_snow_d = snow_depth(i,j) - soft_snow_d(i,j)
            snowdmin = max(vegsnowd_xy(i,j),hard_snow_d)

c Convert Qsubl from sublimation flux to sublimated snow depth.
c   Qsubl is negative here.
            Qsubl(i,j) = Qsubl(i,j) * dt / ro_snow_grid(i,j)

            if (snow_depth(i,j).gt.snowdmin) then
              if (snow_depth(i,j)+Qsubl(i,j).le.snowdmin) then
                Qsubl(i,j) = snowdmin - snow_depth(i,j)
              endif
            else
              Qsubl(i,j) = 0.0
            endif
          enddo
        enddo

c Account for decreases in snow depth due to sublimation.
        do i=1,nx
          do j=1,ny
            snow_depth(i,j) = snow_depth(i,j) + Qsubl(i,j)
            soft_snow_d(i,j) = soft_snow_d(i,j) + Qsubl(i,j)
          enddo
        enddo

      else

        do i=1,nx
          do j=1,ny
            Qsubl(i,j) = 0.0
          enddo
        enddo

      endif

c Convert any snow-depth adjustments that occurred above to
c   swe using the spatially-distributed snow density from the
c   snowpack model.  Also convert the Qsubl_depth values from
c   snow depth to swe depth units.
      do i=1,nx
        do j=1,ny
          swe_depth(i,j) = snow_depth(i,j) * ro_snow_grid(i,j) /
     &      ro_water
          Qsubl_depth(i,j) = Qsubl(i,j) * ro_snow_grid(i,j) /
     &      ro_water
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine accum(snow_d,nx,ny,ro_snow,dt,ro_water,
     &  deltax,deltay,vegsnowd_xy,Tabler_2_flag,
     &  index_ue,index_uw,index_vn,index_vs,undef,
     &  Qsalt_u,Qsalt_v,Qsusp_u,Qsusp_v,Qsubl,dh_salt,
     &  dh_salt_u,dh_salt_v,dh_susp,dh_susp_u,dh_susp_v,
     &  wbal_qsubl,wbal_salt,wbal_susp,bs_flag,
     &  soft_snow_d,topo,topo_land,topoflag,subgrid_flag,
     &  tabler_nn,tabler_ss,tabler_ee,tabler_ww,
     &  tabler_ne,tabler_se,tabler_sw,tabler_nw,
     &  uwind_grid,vwind_grid,wbal_subgrid,sum_qsubl,
     &  sum_trans,swe_depth,snow_depth,ro_snow_grid,
     &  dh_subgrid,tabler_dir,iter,slope_adjust,
     &  curve_len_scale)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,iter,k,loops_snowd_smoother,II,JJ,nnx,nny,
     &  irotate_flag

      real ro_snow,dt,deltax,deltay,bs_flag,topoflag,ro_water
      real snowdmin,hard_snow_d,subgrid_flag,tabler_dir
      real slope_adjust,xmult,curve_len_scale,Tabler_2_flag

      real snow_d(nx_max,ny_max)
      real snow_d_tmp(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real snow_d_tabler(nx_max,ny_max)
      real snow_d_dep(nx_max,ny_max)
      real soft_snow_d_dep(nx_max,ny_max)

      real tabler_nn(nx_max,ny_max)
      real tabler_ss(nx_max,ny_max)
      real tabler_ee(nx_max,ny_max)
      real tabler_ww(nx_max,ny_max)
      real tabler_ne(nx_max,ny_max)
      real tabler_se(nx_max,ny_max)
      real tabler_sw(nx_max,ny_max)
      real tabler_nw(nx_max,ny_max)
      real uwind_grid(nx_max,ny_max)
      real vwind_grid(nx_max,ny_max)

      real soft_snow_d(nx_max,ny_max)
      real Qsubl(nx_max,ny_max)
      real topo(nx_max,ny_max)
      real topo_land(nx_max,ny_max)
      real topo_tmp(nx_max,ny_max)

      real dh_salt(nx_max,ny_max)
      real dh_salt_u(nx_max,ny_max)
      real dh_salt_v(nx_max,ny_max)

      real dh_susp(nx_max,ny_max)
      real dh_susp_u(nx_max,ny_max)
      real dh_susp_v(nx_max,ny_max)

      real dh_subgrid(nx_max,ny_max)
      real dh_dep(nx_max,ny_max)

      real Qsalt_u(nx_max,ny_max)
      real Qsalt_v(nx_max,ny_max)

      real Qsusp_u(nx_max,ny_max)
      real Qsusp_v(nx_max,ny_max)

      real wbal_qsubl(nx_max,ny_max)
      real wbal_salt(nx_max,ny_max)
      real wbal_susp(nx_max,ny_max)
      real wbal_subgrid(nx_max,ny_max)
      real sum_qsubl(nx_max,ny_max)
      real sum_trans(nx_max,ny_max)

      real vegsnowd_xy(nx_max,ny_max)

      integer index_ue(ny_max,2*nx_max+1)
      integer index_uw(ny_max,2*nx_max+1)
      integer index_vn(nx_max,2*ny_max+1)
      integer index_vs(nx_max,2*ny_max+1)

      real extra,space,fill,undef
      real pi,rad2deg,bsflux_dir,bs_flux_u,bs_flux_v,subgrid_dir

c Define the required constants.
      pi = 2.0 * acos(0.0)
      rad2deg = 180.0 / pi

c COMPUTE THE NEW SNOW DEPTH.

c PRECIPITATION
c Account for the addition due to snow precipitation.
c This is now updated at the beginning of the program (day).

c Sum the precipitation in terms of snow depth.
      if (bs_flag.eq.1.0) then

c SALTATION

        call getnewdepth(nx,ny,deltax,deltay,Qsalt_u,
     &    Qsalt_v,dh_salt_u,dh_salt_v,index_ue,index_uw,
     &    index_vn,index_vs,ro_snow,dt,vegsnowd_xy,snow_d,
     &    soft_snow_d,snow_d_dep,soft_snow_d_dep,subgrid_flag)

c This saves the deposition part of the snow-depth change.
        if (subgrid_flag.eq.1.0) then
          do i=1,nx
            do j=1,ny
              dh_dep(i,j) = snow_d_dep(i,j)
            enddo
          enddo
        endif

        do i=1,nx
          do j=1,ny
            dh_salt(i,j) = dh_salt_u(i,j) + dh_salt_v(i,j)
          enddo
        enddo

c SUSPENSION

        call getnewdepth(nx,ny,deltax,deltay,Qsusp_u,
     &    Qsusp_v,dh_susp_u,dh_susp_v,index_ue,index_uw,
     &    index_vn,index_vs,ro_snow,dt,vegsnowd_xy,snow_d,
     &    soft_snow_d,snow_d_dep,soft_snow_d_dep,subgrid_flag)

c This adds the suspension part the deposition to the
c   saltation part of the snow-depth change.
        if (subgrid_flag.eq.1.0) then
          do i=1,nx
            do j=1,ny
              dh_dep(i,j) = dh_dep(i,j) + snow_d_dep(i,j)
            enddo
          enddo
        endif

        do i=1,nx
          do j=1,ny
            dh_susp(i,j) = dh_susp_u(i,j) + dh_susp_v(i,j)
          enddo
        enddo

c Save a copy of the snow distribution to be used to calculate the
c   snow distribution changes resulting from the subgrid
c   redistribution.
        do i=1,nx
          do j=1,ny
            snow_d_tmp(i,j) = snow_d(i,j)
          enddo
        enddo

c Run the subgrid parameterization to account for unrealistic
c   snow accumulation spikes.
        if (subgrid_flag.eq.1.0) then

c Do the Tabler corrections while assuming the constant snow density
c   defined in snowmodel.par (ro_snow).  This is done to avoid all
c   of the messy tracking of different density snows that is being
c   blown around by the wind.  This simplification could be relaxed
c   at some point, but it is not an easy accounting, and I don't
c   think it is justified at this point, given everything else that
c   we don't know.  snow_d is coming in from SnowPack where the
c   ro_snow adjustment has already been made.  When all of the Tabler
c   calculations are finished, then we will convert back from the
c   SnowTran constant density convention to the SnowPack ro_snow_grid
c   spatially distributed snow density.  Note that here, that coming
c   from SnowPack, if the snow depth equals zero, the snow density
c   is undefined.  This can cause problems for the case where Tabler
c   has moved snow into a previously snow-free grid cell; that grid
c   cell will also need a snow density assigned to it.  Right now I
c   am assigning that value to be ro_snow from snowmodel.par.
          do i=1,nx
            do j=1,ny
              snow_d_tabler(i,j) = snow_d(i,j)
            enddo
          enddo

c The following subgrid_1 subroutine was saved because it provides
c   an example of how to relax the assumption of a single wind
c   direction over the entire domain at a given time step.  This
c   is the old subgrid routine that is no longer used or supported.
c         if (subgrid_flag.eq.1.0) then
c           call subgrid_1(nx,ny,snow_d_tabler,
c    &        index_ue,index_uw,index_vn,index_vs,
c    &        tabler_nn,tabler_ss,tabler_ee,tabler_ww,
c    &        tabler_ne,tabler_se,tabler_sw,tabler_nw,uwind_grid,
c    &        vwind_grid,tabler_dir)
c         endif

c Calculate the integrated snow-transport direction.

c Initialize the summing arrays.
          bs_flux_u = 0.0
          bs_flux_v = 0.0

c Sum the fluxes in the u-v directions.  Add the u-v sign to the
c   fluxes so there is a direction associated with them.
          do i=1,nx
            do j=1,ny
              bs_flux_u = bs_flux_u +
     &          sign(Qsalt_u(i,j),uwind_grid(i,j)) +
     &          sign(Qsusp_u(i,j),uwind_grid(i,j))
              bs_flux_v = bs_flux_v +
     &          sign(Qsalt_v(i,j),vwind_grid(i,j)) +
     &          sign(Qsusp_v(i,j),vwind_grid(i,j))
            enddo
          enddo

c Calculate the resulting direction.  Some compilers do not
c   allow both u and v to be 0.0 in the atan2 computation.
          if (abs(bs_flux_u).lt.1e-10) bs_flux_u = 1e-10
          bsflux_dir = rad2deg * atan2(bs_flux_u,bs_flux_v)
          if (bsflux_dir.ge.180.0) then
            bsflux_dir = bsflux_dir - 180.0 
          else
            bsflux_dir = bsflux_dir + 180.0 
          endif

c         print *,bsflux_dir,bs_flux_u,bs_flux_v
c         print *,bsflux_dir,bs_flux_u,bs_flux_v
c         print *,bsflux_dir,bs_flux_u,bs_flux_v
c         print *,bsflux_dir,bs_flux_u,bs_flux_v

c Decide whether tabler_dir or bsflux_dir is going to be used to do
c   the Tabler-surface snow redistributions.
          if (tabler_dir.lt.0.0) then
            subgrid_dir = bsflux_dir
c           print *, 'using bsflux_dir'
          else
            subgrid_dir = tabler_dir
c           print *, 'using tabler_dir'
          endif

c Smooth the snow distribution to eliminate any sharp wind speed
c   variations computed at the next time step.  Define the number
c   of times this is done to be a function of the curvature length
c   scale and the grid increment.  Unwanted peaks in the snow
c   distributions can be further eliminated by adjusting the
c   multiplication factor, xmult.  Also see "loops_windwt_smoother"
c   in micromet_code.f.
c         xmult = 0.15
          xmult = 0.25
c         xmult = 0.5
          loops_snowd_smoother = nint(xmult * curve_len_scale /
     &      (0.5 * (deltax + deltay)))

c         print *
c         print *, 'loops_snowd_smoother ',loops_snowd_smoother
c         print *

c Don't do this smoothing if the domain is arbitrarily small.
          if (nx.gt.100 .and. ny.gt.100) then
            do k=1,loops_snowd_smoother
              call smoother9(nx,ny,snow_d_tabler)
            enddo
          endif

c Create Tabler surfaces from the snow on the ground at this
c   time step, with just erosion taken into account (that's
c   where we are at this point).  Add that current snow depth
c   to topo_land.  Then use this to create the Tabler surface that
c   will be used for this time step.  Also define this depth to be
c   dependent on the SnowPack spatially distributed snow density,
c   not the constant density used in SnowTran.
          do i=1,nx
            do j=1,ny
              topo_tmp(i,j) = snow_d_tabler(i,j) + topo_land(i,j)
            enddo
          enddo

c The following loop does four things:

c (1) Extract the Tabler surface for the direction of interest at
c     this time step.

c (2) The Tabler surfaces that were just generated have had topo_tmp
c     subtracted off of them, giving just the drift profiles with
c     things like zero drift depth on ridges and windwards slopes.
c     So, add the snow depth, prior to any wind redistribution, to
c     these Tabler surfaces.  This will be the maximum snow depth
c     allowed as part of the wind redistribution.

c (3) Set the snow-free areas equal to the snow-holding depth.

c (4) Save the calculated Tabler surface at this time step.  You can
c     comment this out if you don't want to write them out.

c nn.
c Consider N winds.
          if (subgrid_dir.gt.337.5 .and. subgrid_dir.le.360.0 .or.
     &      subgrid_dir.ge.0.0 .and. subgrid_dir.le.22.5) then
c           print *,'in nn'

c Extract the Tabler surface.
            irotate_flag = 1
            call tabler_n(nx,ny,topo_tmp,tabler_nn,deltay,
     &        irotate_flag,slope_adjust)

c Add the snow depth back on, and clip to the snow-holding depth.
            do i=1,nx
              do j=1,ny
                tabler_nn(i,j) = snow_d_tabler(i,j) + tabler_nn(i,j)
                tabler_nn(i,j) = max(tabler_nn(i,j),vegsnowd_xy(i,j))
              enddo
            enddo

c Save the Tabler surface.
            if (Tabler_2_flag.eq.1.0)
     &        write(52,rec=iter) ((tabler_nn(i,j),i=1,nx),j=1,ny)

c ne.
c Consider NE winds.
          elseif (subgrid_dir.gt.22.5 .and. subgrid_dir.le.67.5) then
c           print *,'in ne'

c Extract the Tabler surface.
            irotate_flag = 2
            call tabler_e(nx,ny,topo_tmp,tabler_ne,1.41*deltax,
     &        irotate_flag,slope_adjust)

c Add the snow depth back on, and clip to the snow-holding depth.
            do i=1,nx
              do j=1,ny
                tabler_ne(i,j) = snow_d_tabler(i,j) + tabler_ne(i,j)
                tabler_ne(i,j) = max(tabler_ne(i,j),vegsnowd_xy(i,j))
              enddo
            enddo

c Save the Tabler surface.
            if (Tabler_2_flag.eq.1.0)
     &        write(52,rec=iter) ((tabler_ne(i,j),i=1,nx),j=1,ny)

c ee.
c Consider E winds.
          elseif (subgrid_dir.gt.67.5 .and. subgrid_dir.le.112.5) then
c           print *,'in ee'

c Extract the Tabler surface.
            irotate_flag = 1
            call tabler_e(nx,ny,topo_tmp,tabler_ee,deltax,
     &        irotate_flag,slope_adjust)

c Add the snow depth back on, and clip to the snow-holding depth.
            do i=1,nx
              do j=1,ny
                tabler_ee(i,j) = snow_d_tabler(i,j) + tabler_ee(i,j)
                tabler_ee(i,j) = max(tabler_ee(i,j),vegsnowd_xy(i,j))
              enddo
            enddo

c Save the Tabler surface.
            if (Tabler_2_flag.eq.1.0)
     &        write(52,rec=iter) ((tabler_ee(i,j),i=1,nx),j=1,ny)

c se.
c Consider SE winds.
          elseif(subgrid_dir.gt.112.5 .and. subgrid_dir.le.157.5)then
c           print *,'in se'

c Extract the Tabler surface.
            irotate_flag = 2
            call tabler_s(nx,ny,topo_tmp,tabler_se,1.41*deltay,
     &        irotate_flag,slope_adjust)

c Add the snow depth back on, and clip to the snow-holding depth.
            do i=1,nx
              do j=1,ny
                tabler_se(i,j) = snow_d_tabler(i,j) + tabler_se(i,j)
                tabler_se(i,j) = max(tabler_se(i,j),vegsnowd_xy(i,j))
              enddo
            enddo

c Save the Tabler surface.
            if (Tabler_2_flag.eq.1.0)
     &        write(52,rec=iter) ((tabler_se(i,j),i=1,nx),j=1,ny)

c ss.
c Consider S winds.
          elseif(subgrid_dir.gt.157.5 .and. subgrid_dir.le.202.5)then
c           print *,'in ss'

c Extract the Tabler surface.
            irotate_flag = 1
            call tabler_s(nx,ny,topo_tmp,tabler_ss,deltay,
     &        irotate_flag,slope_adjust)

c Add the snow depth back on, and clip to the snow-holding depth.
            do i=1,nx
              do j=1,ny
                tabler_ss(i,j) = snow_d_tabler(i,j) + tabler_ss(i,j)
                tabler_ss(i,j) = max(tabler_ss(i,j),vegsnowd_xy(i,j))
              enddo
            enddo

c Save the Tabler surface.
            if (Tabler_2_flag.eq.1.0)
     &        write(52,rec=iter) ((tabler_ss(i,j),i=1,nx),j=1,ny)

c sw.
c Consider SW winds.
          elseif(subgrid_dir.gt.202.5 .and. subgrid_dir.le.247.5)then
c           print *,'in sw'

c Extract the Tabler surface.
            irotate_flag = 2
            call tabler_w(nx,ny,topo_tmp,tabler_sw,1.41*deltax,
     &        irotate_flag,slope_adjust)

c Add the snow depth back on, and clip to the snow-holding depth.
            do i=1,nx
              do j=1,ny
                tabler_sw(i,j) = snow_d_tabler(i,j) + tabler_sw(i,j)
                tabler_sw(i,j) = max(tabler_sw(i,j),vegsnowd_xy(i,j))
              enddo
            enddo

c Save the Tabler surface.
            if (Tabler_2_flag.eq.1.0)
     &        write(52,rec=iter) ((tabler_sw(i,j),i=1,nx),j=1,ny)

c ww.
c Consider W winds.
          elseif(subgrid_dir.gt.247.5 .and. subgrid_dir.le.292.5)then
c           print *,'in ww'

c Extract the Tabler surface.
            irotate_flag = 1
            call tabler_w(nx,ny,topo_tmp,tabler_ww,deltax,
     &        irotate_flag,slope_adjust)

c Add the snow depth back on, and clip to the snow-holding depth.
            do i=1,nx
              do j=1,ny
                tabler_ww(i,j) = snow_d_tabler(i,j) + tabler_ww(i,j)
                tabler_ww(i,j) = max(tabler_ww(i,j),vegsnowd_xy(i,j))
              enddo
            enddo

c Save the Tabler surface.
            if (Tabler_2_flag.eq.1.0)
     &        write(52,rec=iter) ((tabler_ww(i,j),i=1,nx),j=1,ny)

c nw.
c Consider NW winds.
          elseif(subgrid_dir.gt.292.5 .and. subgrid_dir.le.337.5)then
c           print *,'in nw'

c Extract the Tabler surface.
            irotate_flag = 2
            call tabler_n(nx,ny,topo_tmp,tabler_nw,1.41*deltay,
     &        irotate_flag,slope_adjust)

c Add the snow depth back on, and clip to the snow-holding depth.
            do i=1,nx
              do j=1,ny
                tabler_nw(i,j) = snow_d_tabler(i,j) + tabler_nw(i,j)
                tabler_nw(i,j) = max(tabler_nw(i,j),vegsnowd_xy(i,j))
              enddo
            enddo

c Save the Tabler surface.
            if (Tabler_2_flag.eq.1.0)
     &        write(52,rec=iter) ((tabler_nw(i,j),i=1,nx),j=1,ny)

          else
            print *,'subgrid_dir not found'
            stop

          endif

c Now add the +dh snow back on, and do the sweep that does not
c   allow any snow to exist above the Tabler surface.
          do i=1,nx
            do j=1,ny
              snow_d_tabler(i,j) = snow_d_tabler(i,j) + dh_dep(i,j)
            enddo
          enddo

c Sweep across the domain in the direction defined by tabler_dir
c   or bsflux_dir (see above; now defined by subgrid_dir).  This is
c   done in the same direction across the entire domain.  If using
c   tabler_dir, then it is the same direction for all time steps.
c   If using bsflux_dir, then it varies for each time step depending
c   on the dominant transport direction.  If you ever want to relax
c   this "same direction over the entire domain" assumption, see
c   the subgrid_1 subroutine (search subgrid_1 in this document; it
c   is all commented out).

c nn.
c Consider N winds.
          if (subgrid_dir.gt.337.5 .and. subgrid_dir.le.360.0 .or.
     &      subgrid_dir.ge.0.0 .and. subgrid_dir.le.22.5) then
c           print *,'in nn'

c Do the sweep in the direction of interest.
            do i=1,nx
              extra = 0.0
              do j=ny,1,-1
                if (snow_d_tabler(i,j).ge.tabler_nn(i,j)) then
                  extra = extra + snow_d_tabler(i,j) - tabler_nn(i,j)
                  snow_d_tabler(i,j) = tabler_nn(i,j)
                else
                  space = tabler_nn(i,j) - snow_d_tabler(i,j)
                  fill = min(extra,space)
                  snow_d_tabler(i,j) = snow_d_tabler(i,j) + fill
                  extra = extra - fill
                  if (extra.lt.0.0) print *,'extra < 0.0',extra
                endif
              enddo
            enddo

c ne.
c Consider NE winds.
          elseif (subgrid_dir.gt.22.5 .and. subgrid_dir.le.67.5) then
c           print *,'in ne'

c Do the sweep in the direction of interest.
            nny = nx + ny - 1
            do j=1,nny
              extra = 0.0
              do i=nx,1,-1
                JJ = j + i - nx
                if (JJ.ge.1 .and. JJ.le.ny) then
                  if (snow_d_tabler(i,JJ).ge.tabler_ne(i,JJ)) then
                    extra = extra+snow_d_tabler(i,JJ)-tabler_ne(i,JJ)
                    snow_d_tabler(i,JJ) = tabler_ne(i,JJ)
                  else
                    space = tabler_ne(i,JJ) - snow_d_tabler(i,JJ)
                    fill = min(extra,space)
                    snow_d_tabler(i,JJ) = snow_d_tabler(i,JJ) + fill
                    extra = extra - fill
                    if (extra.lt.0.0) print *,'extra < 0.0',extra
                  endif
                endif
              enddo
            enddo

c ee.
c Consider E winds.
          elseif (subgrid_dir.gt.67.5 .and. subgrid_dir.le.112.5) then
c           print *,'in ee'

c Do the sweep in the direction of interest.
            do j=1,ny
              extra = 0.0
              do i=nx,1,-1
                if (snow_d_tabler(i,j).ge.tabler_ee(i,j)) then
                  extra = extra + snow_d_tabler(i,j) - tabler_ee(i,j)
                  snow_d_tabler(i,j) = tabler_ee(i,j)
                else
                  space = tabler_ee(i,j) - snow_d_tabler(i,j)
                  fill = min(extra,space)
                  snow_d_tabler(i,j) = snow_d_tabler(i,j) + fill
                  extra = extra - fill
                  if (extra.lt.0.0) print *,'extra < 0.0',extra
                endif
              enddo
            enddo

c se.
c Consider SE winds.
          elseif(subgrid_dir.gt.112.5 .and. subgrid_dir.le.157.5)then
c           print *,'in se'

c Do the sweep in the direction of interest.
            nnx = nx + ny - 1
            do i=1,nnx
              extra = 0.0
              do j=1,ny
                II = i - j + 1
                if (II.ge.1 .and. II.le.nx) then
                  if (snow_d_tabler(II,j).ge.tabler_se(II,j)) then
                    extra = extra+snow_d_tabler(II,j)-tabler_se(II,j)
                    snow_d_tabler(II,j) = tabler_se(II,j)
                  else
                    space = tabler_se(II,j) - snow_d_tabler(II,j)
                    fill = min(extra,space)
                    snow_d_tabler(II,j) = snow_d_tabler(II,j) + fill
                    extra = extra - fill
                    if (extra.lt.0.0) print *,'extra < 0.0',extra
                  endif
                endif
              enddo
            enddo

c ss.
c Consider S winds.
          elseif(subgrid_dir.gt.157.5 .and. subgrid_dir.le.202.5)then
c           print *,'in ss'

c Do the sweep in the direction of interest.
            do i=1,nx
              extra = 0.0
              do j=1,ny
                if (snow_d_tabler(i,j).ge.tabler_ss(i,j)) then
                  extra = extra + snow_d_tabler(i,j) - tabler_ss(i,j)
                  snow_d_tabler(i,j) = tabler_ss(i,j)
                else
                  space = tabler_ss(i,j) - snow_d_tabler(i,j)
                  fill = min(extra,space)
                  snow_d_tabler(i,j) = snow_d_tabler(i,j) + fill
                  extra = extra - fill
                  if (extra.lt.0.0) print *,'extra < 0.0',extra
                endif
              enddo
            enddo

c sw.
c Consider SW winds.
          elseif(subgrid_dir.gt.202.5 .and. subgrid_dir.le.247.5)then
c           print *,'in sw'

c Do the sweep in the direction of interest.
            nny = nx + ny - 1
            do j=1,nny
              extra = 0.0
              do i=1,nx
                JJ = j + i - nx
                if (JJ.ge.1 .and. JJ.le.ny) then
                  if (snow_d_tabler(i,JJ).ge.tabler_sw(i,JJ)) then
                    extra = extra+snow_d_tabler(i,JJ)-tabler_sw(i,JJ)
                    snow_d_tabler(i,JJ) = tabler_sw(i,JJ)
                  else
                    space = tabler_sw(i,JJ) - snow_d_tabler(i,JJ)
                    fill = min(extra,space)
                    snow_d_tabler(i,JJ) = snow_d_tabler(i,JJ) + fill
                    extra = extra - fill
                    if (extra.lt.0.0) print *,'extra < 0.0',extra
                  endif
                endif
              enddo
            enddo

c ww.
c Consider W winds.
          elseif(subgrid_dir.gt.247.5 .and. subgrid_dir.le.292.5)then
c           print *,'in ww'

c Do the sweep in the direction of interest.
            do j=1,ny
              extra = 0.0
              do i=1,nx
                if (snow_d_tabler(i,j).ge.tabler_ww(i,j)) then
                  extra = extra + snow_d_tabler(i,j) - tabler_ww(i,j)
                  snow_d_tabler(i,j) = tabler_ww(i,j)
                else
                  space = tabler_ww(i,j) - snow_d_tabler(i,j)
                  fill = min(extra,space)
                  snow_d_tabler(i,j) = snow_d_tabler(i,j) + fill
                  extra = extra - fill
                  if (extra.lt.0.0) print *,'extra < 0.0',extra
                endif
              enddo
            enddo

c nw.
c Consider NW winds.
          elseif(subgrid_dir.gt.292.5 .and. subgrid_dir.le.337.5)then
c           print *,'in nw'

c Do the sweep in the direction of interest.
            nnx = nx + ny - 1
            do i=1,nnx
              extra = 0.0
              do j=ny,1,-1
                II = i - j + 1
                if (II.ge.1 .and. II.le.nx) then
                  if (snow_d_tabler(II,j).ge.tabler_nw(II,j)) then
                    extra = extra+snow_d_tabler(II,j)-tabler_nw(II,j)
                    snow_d_tabler(II,j) = tabler_nw(II,j)
                  else
                    space = tabler_nw(II,j) - snow_d_tabler(II,j)
                    fill = min(extra,space)
                    snow_d_tabler(II,j) = snow_d_tabler(II,j) + fill
                    extra = extra - fill
                    if (extra.lt.0.0) print *,'extra < 0.0',extra
                  endif
                endif
              enddo
            enddo

          else
            print *,'subgrid_dir not found'
            stop

          endif

c Update the snow depths using the Tabler adjustments.
          do i=1,nx
            do j=1,ny
              snow_d(i,j) = snow_d_tabler(i,j)
            enddo
          enddo

        endif
c End subgrid_flag = 1.0.

c Calculate the snow depth resulting from the subgrid
c   redistribution.
        do i=1,nx
          do j=1,ny
            dh_subgrid(i,j) = snow_d(i,j) - snow_d_tmp(i,j)
          enddo
        enddo

c SUBLIMATION
c Make adjustments for the case where there is no snow available
c   on the ground (or captured within the vegetation) to be
c   eroded.  Since Qsubl is blowing snow sublimation, don't let
c   this sublimation reach down into the hard snow layer or into
c   the vegsnowd, because that snow is not availble to be blown
c   around.  I don't think it matters much whether this is done
c   before or after the Tabler subgrid redistribution.
        do i=1,nx
          do j=1,ny
            hard_snow_d = snow_d(i,j) - soft_snow_d(i,j)
            snowdmin = max(vegsnowd_xy(i,j),hard_snow_d)

c Convert Qsubl flux to sublimated snow depth.
            Qsubl(i,j) = Qsubl(i,j) * dt / ro_snow

            if (snow_d(i,j).gt.snowdmin) then
              if (snow_d(i,j)+Qsubl(i,j).le.snowdmin) then
                Qsubl(i,j) = snowdmin - snow_d(i,j)
              endif
            else
              Qsubl(i,j) = 0.0
            endif
          enddo
        enddo

c Account for decreases in snow depth due to sublimation.  Note
c   that the subgrid_flag = 1.0 routines have not modified the
c   soft snow depth in any way.  Changes must be made if this is
c   not an appropriate assumption.
        do i=1,nx
          do j=1,ny
            snow_d(i,j) = snow_d(i,j) + Qsubl(i,j)
            soft_snow_d(i,j) = soft_snow_d(i,j) + Qsubl(i,j)
          enddo
        enddo

c Find any undefined ro_snow_grid(i,j) values, that correspond to
c   non-zero snow_d(i,j) values, and fill them in with ro_snow.
c   This no longer seems to be required.  Any zero snow_d(i,j)
c   values should have ro_snow in the ro_snow_grid(i,j) positions
c   from the initial conditions that were provided.
c       do i=1,nx
c         do j=1,ny
c           the checking code would go here.
c         enddo
c       enddo

c Update the surface topography resulting from the snow setting
c   on the land.
        if (topoflag.eq.1.0) then
          do i=1,nx
            do j=1,ny
c This gives the constant density version.
c             topo(i,j) = topo_land(i,j) + snow_d(i,j)
c This gives the spatially distributed density version.
              topo(i,j) = topo_land(i,j) + snow_d(i,j) *
     &          ro_snow_grid(i,j) / ro_snow
            enddo
          enddo
        elseif (topoflag.eq.0.0) then
          do i=1,nx
            do j=1,ny
              topo(i,j) = topo_land(i,j)
            enddo
          enddo
        endif

      else
c Because a Tabler surface was not generated or used (because there
c   was no blowing snow), save this time step as undef values.
        if (Tabler_2_flag.eq.1.0)
     &    write(52,rec=iter) ((undef,i=1,nx),j=1,ny)
        
c This ends the "if blowing snow flag = 1" case.
      endif

c MOISTURE BALANCE
c Save enough information to do a moisture balance.
      do i=1,nx
        do j=1,ny

c Save the sublimation in terms of swe depth.
          wbal_qsubl(i,j) = Qsubl(i,j) * ro_snow / ro_water

c Save the saltation in terms of swe depth.
          wbal_salt(i,j) = dh_salt(i,j) * ro_snow / ro_water

c Save the suspension in terms of swe depth.
          wbal_susp(i,j) = dh_susp(i,j) * ro_snow / ro_water

c Save the subgrid redistribution in terms of swe depth.
          wbal_subgrid(i,j) = dh_subgrid(i,j) * ro_snow / ro_water

c Fill summing arrays of the sublimation and transport quantities.
          sum_qsubl(i,j) = sum_qsubl(i,j) + wbal_qsubl(i,j)
          sum_trans(i,j) = sum_trans(i,j) + wbal_salt(i,j) +
     &      wbal_susp(i,j) + wbal_subgrid(i,j)

        enddo
      enddo

      do i=1,nx
        do j=1,ny

c Convert any snow-depth adjustments that occurred in SnowTran-3D
c   to swe (using the SnowTran-3D constant snow density) so
c   that it can be used in SNOWPACK that accounts for the
c   time-evolution of snow density.
          swe_depth(i,j) = snow_d(i,j) * ro_snow / ro_water

c Calculate the snow depth using the spatially-distributed snow density
c   from the snowpack model.
          snow_depth(i,j) = swe_depth(i,j) *
     &      ro_water / ro_snow_grid(i,j)

        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine smoother9(nx,ny,snow)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny
      real snow(nx_max,ny_max)
      real snow_tmp(nx_max,ny_max)

c Performs a 9-point smoothing operation.

c The result at each grid point is a weighted average of the grid
c   point and the surrounding 8 points.  The center point receives
c   a weight of 1.0, the points at each side and above and below
c   receive a weight of 0.5, and corner points receive a weight of
c   0.3.  All points are multiplied by their weights and summed,
c   then divided by the total weight.

c Do the interior.
      do i=2,nx-1
        do j=2,ny-1
          snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) +
     &      snow(i,j+1) + snow(i-1,j) + snow(i+1,j)) + 0.3 *
     &      (snow(i-1,j-1) + snow(i+1,j+1) + snow(i-1,j+1) +
     &      snow(i+1,j-1))) / 4.2
        enddo
      enddo

c Do the sides.
      j = 1
      do i=2,nx-1
        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i-1,j) +
     &    snow(i+1,j)) + 0.3 * (snow(i+1,j+1) + snow(i-1,j+1))) / 3.1
      enddo

      j = ny
      do i=2,nx-1
        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i-1,j) +
     &    snow(i+1,j)) + 0.3 * (snow(i+1,j-1) + snow(i-1,j-1))) / 3.1
      enddo

      i = 1
      do j=2,ny-1
        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i,j+1) +
     &    snow(i+1,j)) + 0.3 * (snow(i+1,j-1) + snow(i+1,j+1))) / 3.1
      enddo

      i = nx
      do j=2,ny-1
        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i,j+1) +
     &    snow(i-1,j)) + 0.3 * (snow(i-1,j-1) + snow(i-1,j+1))) / 3.1
      enddo

c Do the corners.
      i = 1
      j = 1
      snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i+1,j)) +
     &  0.3 * snow(i+1,j+1)) / 2.3

      i = nx
      j = 1
      snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i-1,j)) +
     &  0.3 * snow(i-1,j+1)) / 2.3

      i = 1
      j = ny
      snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i+1,j)) +
     &  0.3 * snow(i+1,j-1)) / 2.3

      i = nx
      j = ny
      snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i-1,j)) +
     &  0.3 * snow(i-1,j-1)) / 2.3

c Return the smoothed array.
      do i=1,nx
        do j=1,ny
          snow(i,j) = snow_tmp(i,j)
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine suspension(Utau,vonKarman,nx,ny,conc_salt,
     &  Qsalt,Qsusp,z_0,h_star,dz,ztop,pi,
     &  fall_vel,Ur_const,Up_const,Utau_t,Qsubl,ht_rhobs,
     &  tair_grid,rh_grid,Qsusp_u,Qsusp_v,uwind_grid,
     &  vwind_grid)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,nzsteps,iz

      real vonKarman,dz,ztop,fall_vel,Ur_const,Up_const
      real ht_rhobs,V_susp,V_salt,pi
      real U_p,Utau_fallvel,U_r,phistar_Cr,product,conc,z

      real Utau(nx_max,ny_max)
      real Utau_t(nx_max,ny_max)
      real uwind_grid(nx_max,ny_max)
      real vwind_grid(nx_max,ny_max)
      real z_0(nx_max,ny_max)
      real h_star(nx_max,ny_max)
      real conc_salt(nx_max,ny_max)
      real Qsalt(nx_max,ny_max)
      real Qsusp(nx_max,ny_max)
      real Qsusp_u(nx_max,ny_max)
      real Qsusp_v(nx_max,ny_max)
      real Qsubl(nx_max,ny_max)

      real tair_grid(nx_max,ny_max)
      real rh_grid(nx_max,ny_max)

c Compute the mass concentration of suspended snow according to
c   Kind (1992).

      do i=1,nx
      do j=1,ny
        if (Qsalt(i,j).gt.0.0) then
          Utau_fallvel = Utau(i,j) / fall_vel
          if (h_star(i,j).eq.z_0(i,j)) h_star(i,j) = 2.0 * z_0(i,j)
          U_r = Utau(i,j)/vonKarman * log(h_star(i,j)/z_0(i,j))
          phistar_Cr = Utau(i,j)/U_r * Ur_const
          product = phistar_Cr * Utau_fallvel
          U_p = Up_const * Utau_t(i,j)

c Compute the concentration in the saltation layer (kg/m**3).
          conc_salt(i,j) = Qsalt(i,j) / (h_star(i,j) * U_p)

          nzsteps = int((ztop - h_star(i,j)) / dz)

          Qsusp(i,j) = 0.0
          Qsubl(i,j) = 0.0

          do iz=1,nzsteps
            z = h_star(i,j) + 0.5 * dz + real(iz - 1) * dz

c Compute the concentration of the suspended snow at height z.
            conc = conc_salt(i,j) * ((product + 1.0) *
     &        (z/h_star(i,j))**((-fall_vel)/(vonKarman*Utau(i,j))) -
     &        product)
            conc = max(conc,0.0)

c Only do The integration if the concentration is non-zero.
            if (conc.gt.0.0) then

c Compute the sublimation due to suspension.
              call getsublim(z,rh_grid(i,j),tair_grid(i,j),Utau(i,j),
     &          z_0(i,j),V_susp,V_salt,Utau_t(i,j),ht_rhobs,1.0,pi)

c Perform the quadrature (summation), without the constants.
              if (z.eq.z_0(i,j)) z = 1.2 * z_0(i,j)
              Qsusp(i,j) = Qsusp(i,j) + conc * log(z/z_0(i,j)) * dz
              Qsubl(i,j) = Qsubl(i,j) + conc * V_susp * dz

            endif

          enddo

c Finish the quadratures.
c Include the constants for Qsusp.
        Qsusp(i,j) = Utau(i,j) / vonKarman * Qsusp(i,j)

c Include the sublimation contribution due to saltation.
        z = h_star(i,j) / 2.0
        call getsublim(z,rh_grid(i,j),tair_grid(i,j),Utau(i,j),
     &    z_0(i,j),V_susp,V_salt,Utau_t(i,j),ht_rhobs,0.0,pi)

        Qsubl(i,j) = Qsubl(i,j) +
     &    V_salt * conc_salt(i,j) * h_star(i,j)

        else
          conc_salt(i,j) = 0.0
          Qsusp(i,j) = 0.0
          Qsubl(i,j) = 0.0
        endif

      enddo
      enddo

c Separate the east-west and the north-south suspended transport
c   components; the vector sum should equal Qsusp.
      do i=1,nx
        do j=1,ny
          Qsusp_u(i,j) = Qsusp(i,j) * abs(uwind_grid(i,j)) /
     &      sqrt(uwind_grid(i,j)**2 + vwind_grid(i,j)**2)
          Qsusp_v(i,j) = Qsusp(i,j) * abs(vwind_grid(i,j)) /
     &      sqrt(uwind_grid(i,j)**2 + vwind_grid(i,j)**2)
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine saltation(Qsalt,deltax,fetch,Utau,Utau_t,nx,ny,
     &  ro_air,gravity,vegsnowd_xy,snow_d,
     &  Qsalt_max,Qsalt_maxu,Qsalt_maxv,deltay,Qsalt_u,Qsalt_v,
     &  index_ue,index_uw,index_vn,index_vs,uwind_grid,
     &  vwind_grid,xmu,soft_snow_d,bc_flag)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny
      integer k,istart,iend,jstart,jend

      real deltax,deltay,fetch,ro_air,gravity,dUtau,xmu,
     &  blowby,bc_flag

      real Qsalt_max(nx_max,ny_max)
      real Qsalt_maxu(nx_max,ny_max)
      real Qsalt_maxv(nx_max,ny_max)
      real Qsalt(nx_max,ny_max)
      real Qsalt_u(nx_max,ny_max)
      real Qsalt_v(nx_max,ny_max)
      real Utau(nx_max,ny_max)
      real Utau_t(nx_max,ny_max)
      real uwind_grid(nx_max,ny_max)
      real vwind_grid(nx_max,ny_max)
      real snow_d(nx_max,ny_max)
      real soft_snow_d(nx_max,ny_max)
      real vegsnowd_xy(nx_max,ny_max)

      integer index_ue(ny_max,2*nx_max+1)
      integer index_uw(ny_max,2*nx_max+1)
      integer index_vn(nx_max,2*ny_max+1)
      integer index_vs(nx_max,2*ny_max+1)

      real scale_EW,scale_NS

c The blowby parameter is implemented to account for the erosion
c   of the tops of deep snow accumulations.  It corrects a
c   deficiency in the du*/dx* < 0 formulation.  It is a number that
c   should range from 0 to 1.0, and represents the fraction of the
c   upwind saltation flux that is transfered farther downwind into
c   the next grid cell.  So, the bigger the number, the less
c   peaked the drift accumulation profile is.  blowby = 0.0 is the
c   original model.  I am now using the Tabler surfaces to do the
c   same kind of thing, so here I hard-code the parameter as in the
c   original model.
      blowby = 0.0

c Compute the maximum possible saltation flux, assuming that
c   an abundance of snow is available at the surface.
      do i=1,nx
        do j=1,ny

c For a given wind speed, find Qsalt_max.
          Qsalt_max(i,j) = 0.68 * ro_air / gravity *
     &      Utau_t(i,j) / Utau(i,j) * (Utau(i,j)**2 - Utau_t(i,j)**2)
          Qsalt_max(i,j) = max(Qsalt_max(i,j),0.0)

c Now weight the max saltation flux for the u and v wind
c   components, where the vector sum should equal Qsalt_max.
          Qsalt_maxu(i,j) = Qsalt_max(i,j) * abs(uwind_grid(i,j)) /
     &      sqrt(uwind_grid(i,j)**2 + vwind_grid(i,j)**2)
          Qsalt_maxv(i,j) = Qsalt_max(i,j) * abs(vwind_grid(i,j)) /
     &      sqrt(uwind_grid(i,j)**2 + vwind_grid(i,j)**2)
        enddo
      enddo

c Define an upwind boundary condition.  If bc_flag = 1.0 then it is
c   assumed that the inflow saltation flux has reached steady state.
c   If bc_flag = 0.0 then the saltation flux is assumed to be zero.
c   The boundary condition is implemented by initializing the arrays
c   to Qsalt_max, and since upwind boundaries are not called in
c   the Qsalt computation, they stay in effect for the future 
c   accumulation/erosion computation.
      if (bc_flag.eq.0.0) then
        do i=1,nx
          do j=1,ny
c Zero incoming flux at the boundaries.
            Qsalt_u(i,j) = 0.0
            Qsalt_v(i,j) = 0.0
          enddo
        enddo
      elseif (bc_flag.eq.1.0) then
        do i=1,nx
          do j=1,ny
c Steady-state (maximum) incoming flux at the boundaries.
            Qsalt_u(i,j) = Qsalt_maxu(i,j)
            Qsalt_v(i,j) = Qsalt_maxv(i,j)
          enddo
        enddo
      endif

c Define the scaling coefficients for Eqn. 9 in L&S 1998. Don't
c   let them be greater than 1.0 or you will make more snow than
c   there was before.
      scale_EW =  xmu * deltax / fetch
      scale_EW = min(1.0,scale_EW)
      scale_NS =  xmu * deltay / fetch
      scale_NS = min(1.0,scale_NS)

c Consider WESTERLY winds.
      do j=1,ny
        do k=1,index_uw(j,1)
          istart = index_uw(j,k*2)+1
          iend = index_uw(j,k*2+1)
          do i=istart,iend
            dUtau = Utau(i,j) - Utau(i-1,j)
            if (dUtau.ge.0.0) then
              Qsalt_u(i,j) = Qsalt_u(i-1,j) + scale_EW *
     &          (Qsalt_maxu(i,j) - Qsalt_u(i-1,j))
            else
c             Qsalt_u(i,j) = min(Qsalt_u(i-1,j),Qsalt_maxu(i,j))

              if (Qsalt_u(i-1,j).lt.Qsalt_maxu(i,j)) then
                Qsalt_u(i,j) = Qsalt_u(i-1,j)
              else
                Qsalt_u(i,j) =
     &            max(blowby*Qsalt_u(i-1,j),Qsalt_maxu(i,j))
              endif

            endif
          enddo
        enddo
      enddo

c Consider EASTERLY winds.
      do j=1,ny
        do k=1,index_ue(j,1)
          iend = index_ue(j,k*2)
          istart = index_ue(j,k*2+1)-1
          do i=istart,iend,-1
            dUtau = Utau(i,j) - Utau(i+1,j)
            if (dUtau.ge.0.0) then
              Qsalt_u(i,j) = Qsalt_u(i+1,j) + scale_EW *
     &          (Qsalt_maxu(i,j) - Qsalt_u(i+1,j))
            else
c             Qsalt_u(i,j) = min(Qsalt_u(i+1,j),Qsalt_maxu(i,j))

              if (Qsalt_u(i+1,j).lt.Qsalt_maxu(i,j)) then
                Qsalt_u(i,j) = Qsalt_u(i+1,j)
              else
                Qsalt_u(i,j) =
     &            max(blowby*Qsalt_u(i+1,j),Qsalt_maxu(i,j))
              endif

            endif
          enddo
        enddo
      enddo

c Consider SOUTHERLY winds.
      do i=1,nx
        do k=1,index_vs(i,1)
          jstart = index_vs(i,k*2)+1
          jend = index_vs(i,k*2+1)
          do j=jstart,jend
            dUtau = Utau(i,j) - Utau(i,j-1)
            if (dUtau.ge.0.0) then
              Qsalt_v(i,j) = Qsalt_v(i,j-1) + scale_NS *
     &          (Qsalt_maxv(i,j) - Qsalt_v(i,j-1))
            else
c             Qsalt_v(i,j) = min(Qsalt_v(i,j-1),Qsalt_maxv(i,j))

              if (Qsalt_v(i,j-1).lt.Qsalt_maxv(i,j)) then
                Qsalt_v(i,j) = Qsalt_v(i,j-1)
              else
                Qsalt_v(i,j) =
     &            max(blowby*Qsalt_v(i,j-1),Qsalt_maxv(i,j))
              endif

            endif
          enddo
        enddo
      enddo

c Consider NORTHERLY winds.
      do i=1,nx
        do k=1,index_vn(i,1)
          jend = index_vn(i,k*2)
          jstart = index_vn(i,k*2+1)-1
          do j=jstart,jend,-1
            dUtau = Utau(i,j) - Utau(i,j+1)
            if (dUtau.ge.0.0) then
              Qsalt_v(i,j) = Qsalt_v(i,j+1) + scale_NS *
     &          (Qsalt_maxv(i,j) - Qsalt_v(i,j+1))
            else
c             Qsalt_v(i,j) = min(Qsalt_v(i,j+1),Qsalt_maxv(i,j))

              if (Qsalt_v(i,j+1).lt.Qsalt_maxv(i,j)) then
                Qsalt_v(i,j) = Qsalt_v(i,j+1)
              else
                Qsalt_v(i,j) =
     &            max(blowby*Qsalt_v(i,j+1),Qsalt_maxv(i,j))
              endif

            endif
          enddo
        enddo
      enddo

c Combine the u and v components to yield the total saltation flux
c   at each grid cell.
      do i=1,nx
        do j=1,ny
          Qsalt(i,j) = Qsalt_u(i,j) + Qsalt_v(i,j)
        enddo
      enddo

c Adjust Qsalt to account for the availablity of snow for transport;
c   taking into consideration whether there is snow on the ground,
c   the holding depth of the vegetation, etc..
      do i=1,nx
        do j=1,ny
          if (snow_d(i,j).le.vegsnowd_xy(i,j)) Qsalt(i,j) = 0.0
          if (soft_snow_d(i,j).le.0.0) Qsalt(i,j) = 0.0
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine solveUtau(Utau,ht_windobs,windspd_grid,C_z,vonKarman,
     &  gravity,z_0,h_star,h_const,vegsnowd_xy,snow_d,
     &  snow_z0,veg_z0,bs_flag,nx,ny,Utau_t,soft_snow_d)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny

      real bs_flag,guess,sfrac,vonKarman,ht_windobs,C_z,gravity
      real h_const,snow_z0,Utautmp,windtmp,wind_max
      real threshold,threshold_flag,z_0_tmp

      real Utau(nx_max,ny_max)
      real Utau_t(nx_max,ny_max)
      real windspd_grid(nx_max,ny_max)
      real z_0(nx_max,ny_max)
      real h_star(nx_max,ny_max)
      real snow_d(nx_max,ny_max)
      real soft_snow_d(nx_max,ny_max)
      real veg_z0(nx_max,ny_max)
      real vegsnowd_xy(nx_max,ny_max)

c Initially set the blowing snow flag to no blowing snow
c   (bs_flag = 0.0).  Then, if snow is found to blow in any
c   domain grid cell, set the flag to on (bs_flag = 1.0).
      bs_flag = 0.0

c Build the Utau array.
      guess = 0.1
      do i=1,nx
      do j=1,ny

c Determine whether snow is saltating (this influences how Utau
c   and z_0 are computed).
        if (snow_d(i,j).le.vegsnowd_xy(i,j)) then

c Saltation will not occur.
          sfrac = snow_d(i,j) / max(vegsnowd_xy(i,j),veg_z0(i,j))
          z_0(i,j) = sfrac * snow_z0 + (1.0 - sfrac) * veg_z0(i,j)
          z_0_tmp = min(0.25*ht_windobs,z_0(i,j))
          Utau(i,j) = windspd_grid(i,j) *
     &      vonKarman / log(ht_windobs/z_0_tmp)
          h_star(i,j) = z_0(i,j) * h_const / C_z
        elseif (soft_snow_d(i,j).le.0.0) then
c Saltation will not occur.
          z_0(i,j) = snow_z0
          Utau(i,j) = windspd_grid(i,j) *
     &      vonKarman / log(ht_windobs/z_0(i,j))
          h_star(i,j) = z_0(i,j)
        else
c Saltation may occur.  Test for that possibility by assuming that
c   saltation is present, solving for Utau and z_0, and comparing
c   whether Utau exceeds Utau_t.  If it does not, set z_0 to that
c   of snow and recompute Utau.

c To help insure that the iteration converges, set the minimum
c   wind speed to be 1.0 m/s, and the maximum wind speed to be
c   30 m/s at 10-m height.
          windtmp = max(1.0,windspd_grid(i,j))
          wind_max = 30.0 * log(ht_windobs/snow_z0)/log(10.0/snow_z0)
          windtmp = min(windtmp,wind_max) 

c For u* over 0.6, use the relation z0 = 0.00734 u* - 0.0022,
c   instead of Equation (5) in Liston and Sturm (1998).  Note that
c   for windspeeds greater than about 35 m/s this will have to be
c   modified for the solution algorithm to converge (because the
c   roughness length will start to be higher than the obs height!).
          threshold = 0.6/vonKarman * log(ht_windobs/0.0022)
          if (windtmp.le.threshold) then
            threshold_flag = 1.0
          else
            threshold_flag = 2.0
          endif

          call solve1(Utautmp,guess,ht_windobs,windtmp,C_z,vonKarman,
     &      gravity,threshold_flag)

          if (Utautmp.gt.Utau_t(i,j)) then

c We have saltation.
            Utau(i,j) = Utautmp
            z_0(i,j) = C_z * Utau(i,j)**2 / (2.0 * gravity)
            h_star(i,j) = h_const * Utau(i,j)**2 / (2.0 * gravity)
            bs_flag = 1.0
          else

c We do not have saltation, but the vegetation is covered by snow.
c   Because we have determined that we do not have saltation, make
c   sure Utau does not exceed Utau_t.
            z_0(i,j) = snow_z0
            Utau(i,j) = windspd_grid(i,j) *
     &        vonKarman / log(ht_windobs/z_0(i,j))
            Utau(i,j) = min(Utau(i,j),Utau_t(i,j))
            h_star(i,j) = z_0(i,j) * h_const / C_z

          endif
        endif

      enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine solve1(xnew,guess,z,windtmp,C_z,vonKarman,
     &  gravity,threshold_flag)

      implicit none

      integer i,maxiter

      real xnew,guess,z,windtmp,C_z,vonKarman,tol,old,gravity
      real fprime,funct,threshold_flag

      tol = 1.0e-3
      maxiter = 20
      old = guess

      if (threshold_flag.eq.1.0) then

        do i=1,maxiter
          fprime = - 1.0 + 2.0 / old * windtmp * vonKarman *
     &      (log(z) - log(C_z/(2.0*gravity)) - 2.0*log(old))**(-2)
          funct = - old + windtmp * vonKarman *
     &      (log(z) - log(C_z/(2.0*gravity)) - 2.0*log(old))**(-1)
          xnew = old - funct/fprime
          if (abs(xnew - old).lt.tol) return
          old = xnew
        end do

      elseif (threshold_flag.eq.2.0) then

        old = 0.6
        do i=1,maxiter
          fprime = - 1.0 + windtmp * vonKarman *
     &      0.00734 / (0.00734 * old - 0.0022) *
     &      (log(z) - log(0.00734 * old - 0.0022))**(-2)
          funct = - old + windtmp * vonKarman *
     &      (log(z) - log(0.00734 * old - 0.0022))**(-1)
          xnew = old - funct/fprime
          if (abs(xnew - old).lt.tol) return
          old = xnew
        end do

      endif

      print *,'max iteration exceeded when solving for Utau, Utau=',old

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getsublim(z,rh,tair,Utau,
     &  z_0,V_susp,V_salt,Utau_t,ht_rhobs,flag,pi)

      implicit none

      real pi,ro_ice,xM,R,R_dryair,vonKarman,visc_air,h_s
      real xlamdaT,D,ro_sat,rh_offset,sigma
      real alfa,rbar_r,xmbar,rbar,u_z,x_r,wbar,flag
      real z,rh,tair,Utau,z_0,V_susp,V_salt,Utau_t,ht_rhobs
      real V_r,xN_r,xNu,xSh,tmp1,tmp2,top,bottom,V_rsalt

      ro_ice = 917.0
      xM = 18.01
      R = 8313.
      R_dryair = 287.
      vonKarman = 0.4
      visc_air = 13.e-6
      h_s = 2.838e6

c     xlamdaT = 0.00063 * tair + 0.0673
      xlamdaT = 0.024
      D = 2.06e-5 * (tair/273.0)**(1.75)
c     ro_sat = 0.622 * 10.0**(11.40 - 2353./tair) / (R_dryair * tair)
      ro_sat = 0.622 / (R_dryair * tair) *
     &  610.78 * exp(21.875 * (tair - 273.15) / (tair - 7.66))

c Assume that the rh varies according to a modification to 
c   Pomeroy's humidity variation with height equation.
      rh_offset = 1.0 - 0.027 * log(ht_rhobs)
      sigma = (0.01 * rh - 1.0) * (rh_offset + 0.027 * log(z))
      sigma = min(0.0,sigma)
      sigma = max(-1.0,sigma)

      alfa = 4.08 + 12.6 * z
      rbar_r = 4.6e-5 * z**(-0.258)
      xmbar = 4.0/3.0 * pi * ro_ice * rbar_r**3 *
     &  (1.0 + 3.0/alfa + 2.0/alfa**2)
      rbar = ((3.0 * xmbar) / (4.0 * pi * ro_ice))**(0.33)
      u_z = Utau/vonKarman * log(z/z_0)
      x_r = 0.005 * u_z**(1.36)
      wbar = 1.1e7 * rbar**(1.8)

      if (flag.eq.1.0) then

c Compute the sublimation loss rate coefficient for the suspension
c   layer.
        V_r = wbar + 3.0 * x_r * cos(pi/4.0)
        xN_r = 2.0 * rbar * V_r / visc_air
        xNu = 1.79 + 0.606 * xN_r**(0.5)
        xSh = xNu
        tmp1 = (h_s * xM)/(R * tair) - 1.0
        tmp2 = xlamdaT * tair * xNu
        top = 2.0 * pi * rbar * sigma
        bottom = h_s/tmp2 * tmp1 + 1.0/(D * ro_sat * xSh)
        V_susp = (top/bottom)/xmbar
        V_salt = 0.0

      elseif (flag.eq.0.0) then

c Compute the sublimation loss rate coefficient for the saltation
c   layer.
        V_rsalt = 0.68 * Utau + 2.3 * Utau_t
        xN_r = 2.0 * rbar * V_rsalt / visc_air
        xNu = 1.79 + 0.606 * xN_r**(0.5)
        xSh = xNu
        tmp1 = (h_s * xM)/(R * tair) - 1.0
        tmp2 = xlamdaT * tair * xNu
        top = 2.0 * pi * rbar * sigma
        bottom = h_s/tmp2 * tmp1 + 1.0/(D * ro_sat * xSh)
        V_salt = (top/bottom)/xmbar
        V_susp = 0.0

      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getdirection(nx,ny,uwind_grid,vwind_grid,index_ue,
     &  index_uw,index_vn,index_vs)

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny,npairs

      real sign1,sign2

      integer index_ue(ny_max,2*nx_max+1)
      integer index_uw(ny_max,2*nx_max+1)
      integer index_vn(nx_max,2*ny_max+1)
      integer index_vs(nx_max,2*ny_max+1)

      real uwind_grid(nx_max,ny_max)
      real vwind_grid(nx_max,ny_max)

c Index whether the winds are blowing east or west.  The first
c   column of the index array is the number of pairs of begining
c   and ending array index of blocks of wind running in the same
c   direction.

c Sweep looking for WESTERLY winds, looking for positive numbers.
      do j=1,ny

        if (uwind_grid(1,j).le.0.0) then
          sign1 = -1.0
        else
          sign1 = 1.0
        endif

          if (sign1.gt.0.0) then
            npairs = 1
            index_uw(j,2) =  1
          else
            npairs = 0
          endif
        do i=2,nx

          if (uwind_grid(i-1,j).le.0.0) then
            sign1 = -1.0
          else
            sign1 = 1.0
          endif
          if (uwind_grid(i,j).le.0.0) then
            sign2 = -1.0
          else
            sign2 = 1.0
          endif

          if (sign2.ne.sign1) then
c We have a sign change.
            if (sign2.gt.0.0) then
c We have gone from negative to positive, indicating the start
c   of a new positive group.
              npairs = npairs + 1
              index_uw(j,npairs*2) = i
            else
c We have gone from positive to negative, indicating the end of
c   the group.
              index_uw(j,npairs*2+1) = i - 1
            endif
          endif
        enddo

        if (uwind_grid(nx,j).le.0.0) then
          sign1 = -1.0
        else
          sign1 = 1.0
        endif

        if (sign1.gt.0.0) then
          index_uw(j,npairs*2+1) = nx
        endif
        index_uw(j,1) = npairs
      enddo

c     do j=1,ny
c       print 30, (index_uw(j,k),k=1,index_uw(j,1)*2+1)
c     enddo
c     print *
c     print *

c Sweep looking for EASTERLY winds, looking for negative numbers.
      do j=1,ny

        if (uwind_grid(1,j).le.0.0) then
          sign1 = -1.0
        else
          sign1 = 1.0
        endif

          if (sign1.lt.0.0) then
            npairs = 1
            index_ue(j,2) = 1
          else
            npairs = 0
          endif
        do i=2,nx

          if (uwind_grid(i-1,j).le.0.0) then
            sign1 = -1.0
          else
            sign1 = 1.0
          endif
          if (uwind_grid(i,j).le.0.0) then
            sign2 = -1.0
          else
            sign2 = 1.0
          endif

          if (sign2.ne.sign1) then
c We have a sign change.
            if (sign2.lt.0.0) then
c We have gone from positive to negative, indicating the start
c   of a new negative group.
              npairs = npairs + 1
              index_ue(j,npairs*2) = i
            else
c We have gone from negative to positive, indicating the end of
c   the group.
              index_ue(j,npairs*2+1) = i - 1
            endif
          endif
        enddo

        if (uwind_grid(nx,j).le.0.0) then
          sign1 = -1.0
        else
          sign1 = 1.0
        endif

        if (sign1.lt.0.0) then
          index_ue(j,npairs*2+1) = nx
        endif
        index_ue(j,1) = npairs
      enddo

c     do j=1,ny
c       print 30, (index_ue(j,k),k=1,index_ue(j,1)*2+1)
c     enddo
c     print *
c     print *

c Sweep looking for SOUTHERLY winds, looking for positive numbers.
      do i=1,nx

        if (vwind_grid(i,1).le.0.0) then
          sign1 = -1.0
        else
          sign1 = 1.0
        endif

          if (sign1.gt.0.0) then
            npairs = 1
            index_vs(i,2) = 1
          else
            npairs = 0
          endif
        do j=2,ny

          if (vwind_grid(i,j-1).le.0.0) then
            sign1 = -1.0
          else
            sign1 = 1.0
          endif
          if (vwind_grid(i,j).le.0.0) then
            sign2 = -1.0
          else
            sign2 = 1.0
          endif

          if (sign2.ne.sign1) then
c We have a sign change.
            if (sign2.gt.0.0) then
c We have gone from negative to positive, indicating the start
c   of a new positive group.
              npairs = npairs + 1
              index_vs(i,npairs*2) = j
            else
c We have gone from positive to negative, indicating the end of
c   the group.
              index_vs(i,npairs*2+1) = j - 1
            endif
          endif
        enddo

        if (vwind_grid(i,ny).le.0.0) then
          sign1 = -1.0
        else
          sign1 = 1.0
        endif

        if (sign1.gt.0.0) then
          index_vs(i,npairs*2+1) = ny
        endif
        index_vs(i,1) = npairs
      enddo

c     do i=1,nx
c       print 30, (index_vs(i,k),k=1,index_vs(i,1)*2+1)
c     enddo
c     print *
c     print *

c Sweep looking for NORTHERLY winds, looking for negative numbers.
      do i=1,nx

        if (vwind_grid(i,1).le.0.0) then
          sign1 = -1.0
        else
          sign1 = 1.0
        endif

          if (sign1.lt.0.0) then
            npairs = 1
            index_vn(i,2) = 1
          else
            npairs = 0
          endif
        do j=2,ny

          if (vwind_grid(i,j-1).le.0.0) then
            sign1 = -1.0
          else
            sign1 = 1.0
          endif
          if (vwind_grid(i,j).le.0.0) then
            sign2 = -1.0
          else
            sign2 = 1.0
          endif

          if (sign2.ne.sign1) then
c We have a sign change.
            if (sign2.lt.0.0) then
c We have gone from positive to negative, indicating the start
c   of a new negative group.
              npairs = npairs + 1
              index_vn(i,npairs*2) = j
            else
c We have gone from negative to positive, indicating the end of
c   the group.
              index_vn(i,npairs*2+1) = j - 1
            endif
          endif
        enddo

        if (vwind_grid(i,ny).le.0.0) then
          sign1 = -1.0
        else
          sign1 = 1.0
        endif

        if (sign1.lt.0.0) then
          index_vn(i,npairs*2+1) = ny
        endif
        index_vn(i,1) = npairs
      enddo

c     do i=1,nx
c       print 30, (index_vn(i,k),k=1,index_vn(i,1)*2+1)
c     enddo
c     print *
c     print *
c 30  format(20i4)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getnewdepth(nx,ny,deltax,deltay,Qsalt_u,
     &  Qsalt_v,dh_salt_u,dh_salt_v,index_ue,index_uw,
     &  index_vn,index_vs,ro_snow,dt,vegsnowd_xy,snow_d,
     &  soft_snow_d,snow_d_dep,soft_snow_d_dep,subgrid_flag)

c If subgrid_flag = 1.0, this routine separates the snow
c   accumulation and erosion contributions to the snowpack
c   evolution so they can be dealt with separately in the
c   subgrid_flag = 1.0 approach and routines.

      implicit none

      include 'snowmodel.inc'

      integer i,j,nx,ny
      integer k,istart,iend,jstart,jend

      real deltax,deltay,ro_snow,dt,dQsalt,snowdmin
      real hard_snow_d,weight_u,weight_v,eps

      integer index_ue(ny_max,2*nx_max+1)
      integer index_uw(ny_max,2*nx_max+1)
      integer index_vn(nx_max,2*ny_max+1)
      integer index_vs(nx_max,2*ny_max+1)

      real Qsalt_u(nx_max,ny_max)
      real Qsalt_v(nx_max,ny_max)

      real snow_d(nx_max,ny_max)
      real soft_snow_d(nx_max,ny_max)
      real dh_salt_u(nx_max,ny_max)
      real dh_salt_v(nx_max,ny_max)
      real vegsnowd_xy(nx_max,ny_max)
      real snow_d_dep(nx_max,ny_max)
      real soft_snow_d_dep(nx_max,ny_max)

      real subgrid_flag

c Define an upwind boundary condition for saltation (here I have
c   assumed that the transport is in equilibrium).
      do i=1,nx
        do j=1,ny
          dh_salt_u(i,j) = 0.0
          dh_salt_v(i,j) = 0.0
        enddo
      enddo

c Consider WESTERLY winds.
      do j=1,ny
        do k=1,index_uw(j,1)
          istart = index_uw(j,k*2)+1
          iend = index_uw(j,k*2+1)
          do i=istart,iend
            dQsalt = Qsalt_u(i,j) - Qsalt_u(i-1,j)
            dh_salt_u(i,j) = (- dt) / ro_snow * dQsalt / deltax

c Make adjustments for the case where there is no snow available
c   on the ground (or captured within the vegetation) to be
c   eroded.
            hard_snow_d = snow_d(i,j) - soft_snow_d(i,j)
            snowdmin = max(vegsnowd_xy(i,j),hard_snow_d)
            if (snow_d(i,j).gt.snowdmin) then
              if (snow_d(i,j)+dh_salt_u(i,j).le.snowdmin) then
                dh_salt_u(i,j) = snowdmin - snow_d(i,j)
                Qsalt_u(i,j) = Qsalt_u(i-1,j) - dh_salt_u(i,j) *
     &            ro_snow * deltax / dt
              endif
            else
              Qsalt_u(i,j) = 0.0
              dh_salt_u(i,j) = 0.0
            endif
          enddo
        enddo
      enddo

c Consider EASTERLY winds.
      do j=1,ny
        do k=1,index_ue(j,1)
          iend = index_ue(j,k*2)
          istart = index_ue(j,k*2+1)-1
          do i=istart,iend,-1
            dQsalt = Qsalt_u(i,j) - Qsalt_u(i+1,j)
            dh_salt_u(i,j) = (- dt) / ro_snow * dQsalt / deltax

c Make adjustments for the case where there is no snow available
c   on the ground (or captured within the vegetation) to be
c   eroded.
            hard_snow_d = snow_d(i,j) - soft_snow_d(i,j)
            snowdmin = max(vegsnowd_xy(i,j),hard_snow_d)
            if (snow_d(i,j).gt.snowdmin) then
              if (snow_d(i,j)+dh_salt_u(i,j).le.snowdmin) then
                dh_salt_u(i,j) = snowdmin - snow_d(i,j)
                Qsalt_u(i,j) = Qsalt_u(i+1,j) - dh_salt_u(i,j) *
     &            ro_snow * deltax / dt
              endif
            else
              Qsalt_u(i,j) = 0.0
              dh_salt_u(i,j) = 0.0
            endif
          enddo
        enddo
      enddo

c Consider SOUTHERLY winds.
      do i=1,nx
        do k=1,index_vs(i,1)
          jstart = index_vs(i,k*2)+1
          jend = index_vs(i,k*2+1)
          do j=jstart,jend
            dQsalt = Qsalt_v(i,j) - Qsalt_v(i,j-1)
            dh_salt_v(i,j) = (- dt) / ro_snow * dQsalt / deltay

c Make adjustments for the case where there is no snow available
c   on the ground (or captured within the vegetation) to be
c   eroded.
            hard_snow_d = snow_d(i,j) - soft_snow_d(i,j)
            snowdmin = max(vegsnowd_xy(i,j),hard_snow_d)
            if (snow_d(i,j).gt.snowdmin) then
              if (snow_d(i,j)+dh_salt_v(i,j).le.snowdmin) then
                dh_salt_v(i,j) = snowdmin - snow_d(i,j)
                Qsalt_v(i,j) = Qsalt_v(i,j-1) - dh_salt_v(i,j) *
     &            ro_snow * deltay / dt
              endif
            else
              Qsalt_v(i,j) = 0.0
              dh_salt_v(i,j) = 0.0
            endif
          enddo
        enddo
      enddo

c Consider NORTHERLY winds.
      do i=1,nx
        do k=1,index_vn(i,1)
          jend = index_vn(i,k*2)
          jstart = index_vn(i,k*2+1)-1
          do j=jstart,jend,-1
            dQsalt = Qsalt_v(i,j) - Qsalt_v(i,j+1)
            dh_salt_v(i,j) = (- dt) / ro_snow * dQsalt / deltay

c Make adjustments for the case where there is no snow available
c   on the ground (or captured within the vegetation) to be
c   eroded.
            hard_snow_d = snow_d(i,j) - soft_snow_d(i,j)
            snowdmin = max(vegsnowd_xy(i,j),hard_snow_d)
            if (snow_d(i,j).gt.snowdmin) then
              if (snow_d(i,j)+dh_salt_v(i,j).le.snowdmin) then
                dh_salt_v(i,j) = snowdmin - snow_d(i,j)
                Qsalt_v(i,j) = Qsalt_v(i,j+1) - dh_salt_v(i,j) *
     &            ro_snow * deltay / dt
              endif
            else
              Qsalt_v(i,j) = 0.0
              dh_salt_v(i,j) = 0.0
            endif
          enddo
        enddo
      enddo

c Update the snow depth changes due to saltation transport from the
c   the east and west, and north and south.  Also correct dh_salt_u
c   and dh_salt_v to account for the minimum snow depth.
      eps = 1e-6
      do i=1,nx
        do j=1,ny
          weight_u = abs(dh_salt_u(i,j)) /
     &      (abs(dh_salt_u(i,j)) + abs(dh_salt_v(i,j)) + eps)

          weight_v = abs(dh_salt_v(i,j)) /
     &      (abs(dh_salt_u(i,j)) + abs(dh_salt_v(i,j)) + eps)

          dh_salt_u(i,j) = weight_u * dh_salt_u(i,j)
          dh_salt_v(i,j) = weight_v * dh_salt_v(i,j)

          if (subgrid_flag.eq.0.0) then
            snow_d(i,j) = snow_d(i,j) + dh_salt_u(i,j) + dh_salt_v(i,j)

            soft_snow_d(i,j) = soft_snow_d(i,j) + dh_salt_u(i,j) +
     &        dh_salt_v(i,j)

          elseif (subgrid_flag.eq.1.0) then

c Just do the erosion.
            snow_d(i,j) = snow_d(i,j) + min(0.0,dh_salt_u(i,j)) +
     &        min(0.0,dh_salt_v(i,j))

            soft_snow_d(i,j) = soft_snow_d(i,j) +
     &        min(0.0,dh_salt_u(i,j))+ min(0.0,dh_salt_v(i,j))

c And save an array of what was deposited during this time step.
            snow_d_dep(i,j) = max(0.0,dh_salt_u(i,j)) +
     &        max(0.0,dh_salt_v(i,j))

            soft_snow_d_dep(i,j) = 
     &        max(0.0,dh_salt_u(i,j))+ max(0.0,dh_salt_v(i,j))
          endif

        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     subroutine subgrid_1(nx,ny,snow_d,
c    &  index_ue,index_uw,index_vn,index_vs,
c    &  tabler_nn,tabler_ss,tabler_ee,tabler_ww,
c    &  tabler_ne,tabler_se,tabler_sw,tabler_nw,uwind_grid,
c    &  vwind_grid,tabler_dir)

c This subroutine forces SnowTran-3D's snow accumluation profiles
c   to be bounded by the equilibrium topographic drift catchment
c   profiles observed and modeled by Tabler (1975).

c Tabler, R. D., 1975: Predicting profiles of snowdrifts in
c   topographic catchments.  Proceedings of the 43rd Annual Western
c   Snow Conference, San Diego, California, 87-97.

c     implicit none

c     include 'snowmodel.inc'

c     integer i,j,nx,ny
c     integer k,istart,iend,jstart,jend

c     real snow_d_extra,snow_sfc,tabler,tabler_dir

c     integer index_ue(ny_max,2*nx_max+1)
c     integer index_uw(ny_max,2*nx_max+1)
c     integer index_vn(nx_max,2*ny_max+1)
c     integer index_vs(nx_max,2*ny_max+1)

c     real snow_d(nx_max,ny_max)
c     real snow_d1(nx_max,ny_max)
c     real snow_d2(nx_max,ny_max)
c     real tabler_nn(nx_max,ny_max)
c     real tabler_ss(nx_max,ny_max)
c     real tabler_ee(nx_max,ny_max)
c     real tabler_ww(nx_max,ny_max)
c     real tabler_ne(nx_max,ny_max)
c     real tabler_se(nx_max,ny_max)
c     real tabler_sw(nx_max,ny_max)
c     real tabler_nw(nx_max,ny_max)
c     real uwind_grid(nx_max,ny_max)
c     real vwind_grid(nx_max,ny_max)

c     real weight_u(nx_max,ny_max)
c     real weight_v(nx_max,ny_max)

c This is just a summary of all of the possibilities.
cc          if(winddir(i,j).gt.337.5.or.winddir(i,j).le.22.5)then
cc            tabler = tabler_nn(i,j)
cc          elseif(winddir(i,j).gt.22.5.and.winddir(i,j).le.67.5)then
cc            tabler = tabler_ne(i,j)
cc          elseif(winddir(i,j).gt.67.5.and.winddir(i,j).le.112.5)then
cc            tabler = tabler_ee(i,j)
cc          elseif(winddir(i,j).gt.112.5.and.winddir(i,j).le.157.5)then
cc            tabler = tabler_se(i,j)
cc          elseif(winddir(i,j).gt.157.5.and.winddir(i,j).le.202.5)then
cc            tabler = tabler_ss(i,j)
cc          elseif(winddir(i,j).gt.202.5.and.winddir(i,j).le.247.5)then
cc            tabler = tabler_sw(i,j)
cc          elseif(winddir(i,j).gt.247.5.and.winddir(i,j).le.292.5)then
cc            tabler = tabler_ww(i,j)
cc          elseif(winddir(i,j).gt.292.5.and.winddir(i,j).le.337.5)then
cc            tabler = tabler_nw(i,j)
cc          endif

c Create a copy of the incoming snow depth distribution.  Also define
c   the u and v weighting functions.
c     do j=1,ny
c       do i=1,nx
c         snow_d1(i,j) = snow_d(i,j)
c         snow_d2(i,j) = snow_d(i,j)

c         weight_u(i,j) = abs(uwind_grid(i,j)) /
c    &          sqrt(uwind_grid(i,j)**2 + vwind_grid(i,j)**2)
c         weight_v(i,j) = abs(vwind_grid(i,j)) /
c    &          sqrt(uwind_grid(i,j)**2 + vwind_grid(i,j)**2)
c       enddo
c     enddo

c Consider WESTERLY winds.
c     do j=1,ny
c       do k=1,index_uw(j,1)
c         istart = index_uw(j,k*2)+1
c         iend = index_uw(j,k*2+1)
c         do i=istart,iend

c           if(tabler_dir.gt.337.5.and.tabler_dir.le.360.0.or.
c    &        tabler_dir.ge.0.0.and.tabler_dir.le.22.5)then
c             tabler = tabler_nn(i,j)
c           elseif(tabler_dir.gt.157.5.and.tabler_dir.le.202.5)then
c             tabler = tabler_ss(i,j)
c           elseif(tabler_dir.gt.202.5.and.tabler_dir.le.247.5)then
c             tabler = tabler_sw(i,j)
c           elseif(tabler_dir.gt.247.5.and.tabler_dir.le.292.5)then
c             tabler = tabler_ww(i,j)
c           elseif(tabler_dir.gt.292.5.and.tabler_dir.le.337.5)then
c             tabler = tabler_nw(i,j)
c           endif

c           snow_sfc = tabler

c           if (snow_d1(i,j).gt.snow_sfc) then
c             snow_d_extra = (snow_d1(i,j) - snow_sfc) * weight_u(i,j)
c             snow_d1(i,j) = snow_d1(i,j) - snow_d_extra
c             if (i.lt.nx) then
c               snow_d1(i+1,j) = snow_d1(i+1,j) + snow_d_extra
c             else
c               snow_d1(i,j) = snow_d1(i,j)
c             endif
c           endif

c         enddo
c       enddo
c     enddo

c Consider EASTERLY winds.
c     do j=1,ny
c       do k=1,index_ue(j,1)
c         iend = index_ue(j,k*2)
c         istart = index_ue(j,k*2+1)-1
c         do i=istart,iend,-1

c           if(tabler_dir.gt.337.5.and.tabler_dir.le.360.0.or.
c    &        tabler_dir.ge.0.0.and.tabler_dir.le.22.5)then
c             tabler = tabler_nn(i,j)
c           elseif(tabler_dir.gt.22.5.and.tabler_dir.le.67.5)then
c             tabler = tabler_ne(i,j)
c           elseif(tabler_dir.gt.67.5.and.tabler_dir.le.112.5)then
c             tabler = tabler_ee(i,j)
c           elseif(tabler_dir.gt.112.5.and.tabler_dir.le.157.5)then
c             tabler = tabler_se(i,j)
c           elseif(tabler_dir.gt.157.5.and.tabler_dir.le.202.5)then
c             tabler = tabler_ss(i,j)
c           endif

c           snow_sfc = tabler

c           if (snow_d1(i,j).gt.snow_sfc) then
c             snow_d_extra = (snow_d1(i,j) - snow_sfc) * weight_u(i,j)
c             snow_d1(i,j) = snow_d1(i,j) - snow_d_extra
c             if (i.gt.1) then
c               snow_d1(i-1,j) = snow_d1(i-1,j) + snow_d_extra
c             else
c               snow_d1(i,j) = snow_d1(i,j)
c             endif
c           endif
c         enddo
c       enddo
c     enddo

c Consider SOUTHERLY winds.
c     do i=1,nx
c       do k=1,index_vs(i,1)
c         jstart = index_vs(i,k*2)+1
c         jend = index_vs(i,k*2+1)
c         do j=jstart,jend

c           if(tabler_dir.gt.67.5.and.tabler_dir.le.112.5)then
c             tabler = tabler_ee(i,j)
c           elseif(tabler_dir.gt.112.5.and.tabler_dir.le.157.5)then
c             tabler = tabler_se(i,j)
c           elseif(tabler_dir.gt.157.5.and.tabler_dir.le.202.5)then
c             tabler = tabler_ss(i,j)
c           elseif(tabler_dir.gt.202.5.and.tabler_dir.le.247.5)then
c             tabler = tabler_sw(i,j)
c           elseif(tabler_dir.gt.247.5.and.tabler_dir.le.292.5)then
c             tabler = tabler_ww(i,j)
c           endif

c           snow_sfc = tabler

c           if (snow_d2(i,j).gt.snow_sfc) then
c             snow_d_extra = (snow_d2(i,j) - snow_sfc) * weight_v(i,j)
c             snow_d2(i,j) = snow_d2(i,j) - snow_d_extra
c             if (j.lt.ny) then
c               snow_d2(i,j+1) = snow_d2(i,j+1) + snow_d_extra
c             else
c               snow_d2(i,j) = snow_d2(i,j)
c             endif
c           endif
c         enddo
c       enddo
c     enddo

c Consider NORTHERLY winds.
c     do i=1,nx
c       do k=1,index_vn(i,1)
c         jend = index_vn(i,k*2)
c         jstart = index_vn(i,k*2+1)-1
c         do j=jstart,jend,-1

c           if(tabler_dir.gt.337.5.and.tabler_dir.le.360.0.or.
c    &        tabler_dir.ge.0.0.and.tabler_dir.le.22.5)then
c             tabler = tabler_nn(i,j)
c           elseif(tabler_dir.gt.22.5.and.tabler_dir.le.67.5)then
c             tabler = tabler_ne(i,j)
c           elseif(tabler_dir.gt.67.5.and.tabler_dir.le.112.5)then
c             tabler = tabler_ee(i,j)
c           elseif(tabler_dir.gt.247.5.and.tabler_dir.le.292.5)then
c             tabler = tabler_ww(i,j)
c           elseif(tabler_dir.gt.292.5.and.tabler_dir.le.337.5)then
c             tabler = tabler_nw(i,j)
c           endif

c           snow_sfc = tabler

c           if (snow_d2(i,j).gt.snow_sfc) then
c             snow_d_extra = (snow_d2(i,j) - snow_sfc) * weight_v(i,j)
c             snow_d2(i,j) = snow_d2(i,j) - snow_d_extra
c             if (j.gt.1) then
c               snow_d2(i,j-1) = snow_d2(i,j-1) + snow_d_extra
c             else
c               snow_d2(i,j) = snow_d2(i,j)
c             endif
c           endif

c         enddo
c       enddo
c     enddo

c Update the snow depths resulting from these redistributions.
c     do j=1,ny
c       do i=1,nx
c         snow_d(i,j) = snow_d1(i,j) * weight_u(i,j) +
c    &      snow_d2(i,j) * weight_v(i,j)
c       enddo
c     enddo

c Clean up the boundaries.  Make the boundary values equal to
c   the values just inside the boundaries.
c     do i=2,nx-1
c       snow_d(i,1) = snow_d(i,2)
c       snow_d(i,ny) = snow_d(i,ny-1)
c     enddo
c     do j=1,ny
c       snow_d(1,j) = snow_d(2,j)
c       snow_d(nx,j) = snow_d(nx-1,j)
c     enddo

c     return
c     end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine tabler_3d(nx,ny,topo_land,deltax,deltay,
     &  tabler_ww,tabler_ee,tabler_ss,
     &  tabler_nn,tabler_ne,tabler_se,
     &  tabler_sw,tabler_nw,slope_adjust)

c This subroutine uses Tabler (1975) to define equilibrium profiles
c   for the topographic drift catchments, for the case of winds
c   from the EAST, WEST, NORTH, and SOUTH, and anywhere inbetween.

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,irotate_flag

      real deltax,deltay,slope_adjust

      real topo_land(nx_max,ny_max)

      real tabler_nn(nx_max,ny_max)
      real tabler_ss(nx_max,ny_max)
      real tabler_ee(nx_max,ny_max)
      real tabler_ww(nx_max,ny_max)

      real tabler_ne(nx_max,ny_max)
      real tabler_se(nx_max,ny_max)
      real tabler_sw(nx_max,ny_max)
      real tabler_nw(nx_max,ny_max)

c Here we generate maximum snow accumulation surfaces for n, ne, e,
c   se, s, sw, w, and nw winds.  I call these "tabler surfaces".  
c
c They are valid for the wind direction ranges: N=337.5-22.5,
c   NE=22.5-67.5, E=67.5-112.5, SE=112.5-157.5, S=157.5-202.5,
c   SW=202.5-247.5, W=247.5-292.5, and NW=292.5-337.5.
c
c These Tabler Surfaces define a "potential" snow surface that
c   represents the maximum possible snow-accumulation depth from winds
c   coming from these directions.
c
c Tabler, R. D., 1975: Predicting profiles of snowdrifts in
c   topographic catchments.  Proceedings of the 43rd Annual Western
c   Snow Conference, San Diego, California, 87-97.

c Consider N winds.
      irotate_flag = 1
      call tabler_n(nx,ny,topo_land,tabler_nn,deltay,
     &  irotate_flag,slope_adjust)

c Consider NE winds.
      irotate_flag = 2
      call tabler_e(nx,ny,topo_land,tabler_ne,1.41*deltax,
     &  irotate_flag,slope_adjust)

c Consider E winds.
      irotate_flag = 1
      call tabler_e(nx,ny,topo_land,tabler_ee,deltax,
     &  irotate_flag,slope_adjust)

c Consider SE winds.
      irotate_flag = 2
      call tabler_s(nx,ny,topo_land,tabler_se,1.41*deltay,
     &  irotate_flag,slope_adjust)

c Consider S winds.
      irotate_flag = 1
      call tabler_s(nx,ny,topo_land,tabler_ss,deltay,
     &  irotate_flag,slope_adjust)

c Consider SW winds.
      irotate_flag = 2
      call tabler_w(nx,ny,topo_land,tabler_sw,1.41*deltax,
     &  irotate_flag,slope_adjust)

c Consider W winds.
      irotate_flag = 1
      call tabler_w(nx,ny,topo_land,tabler_ww,deltax,
     &  irotate_flag,slope_adjust)

c Consider NW winds.
      irotate_flag = 2
      call tabler_n(nx,ny,topo_land,tabler_nw,1.41*deltay,
     &  irotate_flag,slope_adjust)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine tabler_w(nx,ny,topo_land,tabler_ww,deltax,
     &  irotate_flag,slope_adjust)

c This subroutine uses Tabler (1975) to define equilibrium profiles
c   for the topographic drift catchments, for the case of winds
c   from the WEST and SOUTHWEST.

c Tabler, R. D., 1975: Predicting profiles of snowdrifts in
c   topographic catchments.  Proceedings of the 43rd Annual Western
c   Snow Conference, San Diego, California, 87-97.

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,istart,iend,JJ,irotate_flag,nny,nnx,
     &  ii,iii,nx_max_tabler,maxlines
      real deltax,y,slope_adjust,xmax_slope,dx,test,
     &  x,x1,x2,x3,x4,y1,y2,t1,t2,t3,t4,t5

      real topo_land(nx_max,ny_max)
      real tabler_ww(nx_max,ny_max)
      real tabler(nx_max)
      real topo_line(nx_max)
      real drift_start_topo(nx_max)

      parameter (nx_max_tabler=nx_max*1000)
      real topo_1m(nx_max_tabler)
      real tabler_1m(nx_max_tabler)
      real drift_start_topo_1m(nx_max_tabler)

c This program:
c   1) takes the coarse model topography and generates 1.0-m grid
c        increment topo lines;
c   2) uses those to generate the Tabler surface profiles; and
c   3) extracts the profiles at the coarse model grid cells.

c Fill the snow array with topo data.
      do j=1,ny
        do i=1,nx
          tabler_ww(i,j) = topo_land(i,j)
        enddo
      enddo

c Define the length of the j-looping, depending on whether there
c   is any rotation done to get 45 deg winds.
      if (irotate_flag.eq.2) then
        nny = nx+ny-1
      else
        nny = ny
      endif

c Required parameters.
      if (irotate_flag.eq.2) then
        dx = 1.41
        test = amod(deltax/1.41,dx/1.41)
      else
        dx = 1.0
        test = amod(deltax,dx)
      endif

      xmax_slope = -0.20

c This Tabler program has not been made general enough to deal
c   with deltax and deltay values that are not evenly divisible
c   by 1.0 (or 1.41 for diagonal profiles).
      if (abs(test).gt.1.0e10-5) then
        print *,'To generate the Tabler surfaces, deltax and deltay'
        print *,'  must be evenly divisible by 1.0 or 1.41.'
        print *,'  deltax = ',deltax
        stop
      endif

c Define the number of 1-m grid cells in each model grid cell, and
c   calculate how many of these are in the entire nx domain.
      nnx = nint(deltax/dx)
      maxlines = (nx - 1) * nnx + 1

c Define the starting and ending points of the line we are going to
c   work with.  This is done to make sure we are never looking
c   outside the data for numbers to work with.
      istart = 1
      if (irotate_flag.eq.2) then
        iend = maxlines - (32+11+10+11)
      else
        iend = maxlines - (45+15+15+15)
      endif

c Extract the line we are going to work with.
      do j=1,nny

        if (irotate_flag.eq.2) then
          do i=1,nx
            JJ = j + i - nx
            drift_start_topo(i) = 0.0
            if (JJ.le.0) then
              tabler(i) = tabler_ww(1-j+nx,1)
              topo_line(i) = tabler_ww(1-j+nx,1)
            elseif (JJ.gt.ny) then
              tabler(i) = tabler(i-1)
              topo_line(i) = tabler(i-1)
            else
              tabler(i) = tabler_ww(i,JJ)
              topo_line(i) = tabler_ww(i,JJ)
            endif
          enddo
        else
          do i=1,nx
            tabler(i) = tabler_ww(i,j)
            topo_line(i) = topo_land(i,j)
            drift_start_topo(i) = 0.0
          enddo
        endif

c To build the 1.0 m line, use linear interpolation between the
c   model topo data.  Include the end point.
        do i=1,nx-1
          do ii=1,nnx
            iii = (i - 1) * nnx + ii
            x1 = 0.0
            x = real(ii - 1) * dx
            y2 = topo_line(i+1)
            y1 = topo_line(i)
            topo_1m(iii) = y1 + ((y2 - y1)/deltax) * (x - x1)
          enddo
        enddo
        topo_1m((nx - 1) * nnx + 1) = topo_line(nx)

c Use this topo array to be the starting point for generating the
c   Tabler surfaces.
        do i=1,maxlines
          tabler_1m(i) = topo_1m(i)
          drift_start_topo_1m(i) = 0.0
        enddo

c Run the Tabler model.
        do i=istart,iend
          if (irotate_flag.eq.2) then
            t1 = tabler_1m(i)
            t2 = tabler_1m(i+31)
            t3 = tabler_1m(i+31+11)
            t4 = tabler_1m(i+31+21)
            t5 = tabler_1m(i+31+32)

            x1 = (t2 - t1) / 45.0
            x2 = max((t3 - t2) / 15.0,xmax_slope)
            x3 = max((t4 - t3) / 15.0,xmax_slope)
            x4 = max((t5 - t4) / 15.0,xmax_slope)

            y = 0.25*x1 + 0.55*x2 + 0.15*x3 + 0.05*x4

            tabler_1m(i+32) = max(topo_1m(i+32),
     &        tabler_1m(i+31) + y * slope_adjust * dx)
          else
            t1 = tabler_1m(i)
            t2 = tabler_1m(i+44)
            t3 = tabler_1m(i+44+15)
            t4 = tabler_1m(i+44+30)
            t5 = tabler_1m(i+44+45)

            x1 = (t2 - t1) / 45.0
            x2 = max((t3 - t2) / 15.0,xmax_slope)
            x3 = max((t4 - t3) / 15.0,xmax_slope)
            x4 = max((t5 - t4) / 15.0,xmax_slope)

            y = 0.25*x1 + 0.55*x2 + 0.15*x3 + 0.05*x4

            tabler_1m(i+45) = max(topo_1m(i+45),
     &        tabler_1m(i+44) + y * slope_adjust * dx)
          endif
        enddo

c Extract the profile at the model grid points.
        do i=1,nx
          ii = (i - 1) * nnx + 1
          tabler(i) = tabler_1m(ii)
          drift_start_topo(i) = drift_start_topo_1m(ii)
        enddo

c Use the 1-D arrays to fill in the 2-D tabler-surface array.
        do i=1,nx
          if (irotate_flag.eq.2) then
            JJ = j + i - nx
            if (JJ.ge.1 .and. JJ.le.ny) then
              tabler_ww(i,JJ) = tabler(i) + drift_start_topo(i)
            endif
          else
            tabler_ww(i,j) = tabler(i) + drift_start_topo(i)
          endif
        enddo

      enddo

c Convert snow_traps back to actual snow depths instead of
c   depth plus topography.
      do j=1,ny
        do i=1,nx
          tabler_ww(i,j) = tabler_ww(i,j) - topo_land(i,j)
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine tabler_e(nx,ny,topo_land,tabler_ee,deltax,
     &  irotate_flag,slope_adjust)

c This subroutine uses Tabler (1975) to define equilibrium profiles
c   for the topographic drift catchments, for the case of winds
c   from the EAST and NORTHEAST.

c Tabler, R. D., 1975: Predicting profiles of snowdrifts in
c   topographic catchments.  Proceedings of the 43rd Annual Western
c   Snow Conference, San Diego, California, 87-97.

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,istart,iend,JJ,irotate_flag,nny,nnx,
     &  ii,iii,nx_max_tabler,maxlines
      real deltax,y,slope_adjust,xmax_slope,dx,test,
     &  x,x1,x2,x3,x4,y1,y2,t1,t2,t3,t4,t5

      real topo_land(nx_max,ny_max)
      real tabler_ee(nx_max,ny_max)
      real tabler(nx_max)
      real topo_line(nx_max)
      real drift_start_topo(nx_max)

      parameter (nx_max_tabler=nx_max*1000)
      real topo_1m(nx_max_tabler)
      real tabler_1m(nx_max_tabler)
      real drift_start_topo_1m(nx_max_tabler)

c This program:
c   1) takes the coarse model topography and generates 1.0-m grid
c        increment topo lines;
c   2) uses those to generate the Tabler surface profiles; and
c   3) extracts the profiles at the coarse model grid cells.

c Fill the snow array with topo data.
      do j=1,ny
        do i=1,nx
          tabler_ee(i,j) = topo_land(i,j)
        enddo
      enddo

c Define the length of the j-looping, depending on whether there
c   is any rotation done to get 45 deg winds.
      if (irotate_flag.eq.2) then
        nny = nx+ny-1
      else
        nny = ny
      endif

c Required parameters.
      if (irotate_flag.eq.2) then
        dx = 1.41
        test = amod(deltax/1.41,dx/1.41)
      else
        dx = 1.0
        test = amod(deltax,dx)
      endif

      xmax_slope = -0.20

c This Tabler program has not been made general enough to deal
c   with deltax and deltay values that are not evenly divisible
c   by 1.0 (or 1.41 for diagonal profiles).
      if (abs(test).gt.1.0e10-5) then
        print *,'To generate the Tabler surfaces, deltax and deltay'
        print *,'  must be evenly divisible by 1.0 or 1.41.'
        print *,'  deltax = ',deltax
        stop
      endif

c Define the number of 1-m grid cells in each model grid cell, and
c   calculate how many of these are in the entire nx domain.
      nnx = nint(deltax/dx)
      maxlines = (nx - 1) * nnx + 1

c Define the starting and ending points of the line we are going to
c   work with.  This is done to make sure we are never looking
c   outside the data for numbers to work with.
      istart = maxlines
      if (irotate_flag.eq.2) then
        iend = 1 + (32+11+10+11)
      else
        iend = 1 + (45+15+15+15)
      endif

c Extract the line we are going to work with.
      do j=1,nny

        if (irotate_flag.eq.2) then
          do i=1,nx
            JJ = j + i - nx
            drift_start_topo(i) = 0.0
            if (JJ.le.0) then
              tabler(i) = tabler_ee(1-j+nx,1)
              topo_line(i) = tabler_ee(1-j+nx,1)
            elseif (JJ.gt.ny) then
              tabler(i) = tabler(i-1)
              topo_line(i) = tabler(i-1)
            else
              tabler(i) = tabler_ee(i,JJ)
              topo_line(i) = tabler_ee(i,JJ)
            endif
          enddo
        else
          do i=1,nx
            tabler(i) = tabler_ee(i,j)
            topo_line(i) = topo_land(i,j)
            drift_start_topo(i) = 0.0
          enddo
        endif

c To build the 1.0 m line, use linear interpolation between the
c   model topo data.  Include the end point.
        do i=1,nx-1
          do ii=1,nnx
            iii = (i - 1) * nnx + ii
            x1 = 0.0
            x = real(ii - 1) * dx
            y2 = topo_line(i+1)
            y1 = topo_line(i)
            topo_1m(iii) = y1 + ((y2 - y1)/deltax) * (x - x1)
          enddo
        enddo
        topo_1m((nx - 1) * nnx + 1) = topo_line(nx)

c Use this topo array to be the starting point for generating the
c   Tabler surfaces.
        do i=1,maxlines
          tabler_1m(i) = topo_1m(i)
          drift_start_topo_1m(i) = 0.0
        enddo

c Run the Tabler model.
        do i=istart,iend,-1
          if (irotate_flag.eq.2) then
            t1 = tabler_1m(i)
            t2 = tabler_1m(i-31)
            t3 = tabler_1m(i-31-11)
            t4 = tabler_1m(i-31-21)
            t5 = tabler_1m(i-31-32)

            x1 = (t2 - t1) / 45.0
            x2 = max((t3 - t2) / 15.0,xmax_slope)
            x3 = max((t4 - t3) / 15.0,xmax_slope)
            x4 = max((t5 - t4) / 15.0,xmax_slope)

            y = 0.25*x1 + 0.55*x2 + 0.15*x3 + 0.05*x4

            tabler_1m(i-32) = max(topo_1m(i-32),
     &        tabler_1m(i-31) + y * slope_adjust * dx)
          else
            t1 = tabler_1m(i)
            t2 = tabler_1m(i-44)
            t3 = tabler_1m(i-44-15)
            t4 = tabler_1m(i-44-30)
            t5 = tabler_1m(i-44-45)

            x1 = (t2 - t1) / 45.0
            x2 = max((t3 - t2) / 15.0,xmax_slope)
            x3 = max((t4 - t3) / 15.0,xmax_slope)
            x4 = max((t5 - t4) / 15.0,xmax_slope)

            y = 0.25*x1 + 0.55*x2 + 0.15*x3 + 0.05*x4

            tabler_1m(i-45) = max(topo_1m(i-45),
     &        tabler_1m(i-44) + y * slope_adjust * dx)
          endif
        enddo

c Extract the profile at the model grid points.
        do i=1,nx
          ii = (i - 1) * nnx + 1
          tabler(i) = tabler_1m(ii)
          drift_start_topo(i) = drift_start_topo_1m(ii)
        enddo

c Use the 1-D arrays to fill in the 2-D tabler-surface array.
        do i=1,nx
          if (irotate_flag.eq.2) then
            JJ = j + i - nx
            if (JJ.ge.1 .and. JJ.le.ny) then
              tabler_ee(i,JJ) = tabler(i) + drift_start_topo(i)
            endif
          else
            tabler_ee(i,j) = tabler(i) + drift_start_topo(i)
          endif
        enddo

      enddo

c Convert snow_traps back to actual snow depths instead of
c   depth plus topography.
      do j=1,ny
        do i=1,nx
          tabler_ee(i,j) = tabler_ee(i,j) - topo_land(i,j)
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine tabler_s(nx,ny,topo_land,tabler_ss,deltay,
     &  irotate_flag,slope_adjust)

c This subroutine uses Tabler (1975) to define equilibrium profiles
c   for the topographic drift catchments, for the case of winds
c   from the SOUTH and SOUTHEAST.

c Tabler, R. D., 1975: Predicting profiles of snowdrifts in
c   topographic catchments.  Proceedings of the 43rd Annual Western
c   Snow Conference, San Diego, California, 87-97.

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,jstart,jend,II,irotate_flag,nny,nnx,
     &  jj,jjj,ny_max_tabler,maxlines
      real deltay,y,slope_adjust,xmax_slope,dy,test,
     &  x,x1,x2,x3,x4,y1,y2,t1,t2,t3,t4,t5

      real topo_land(nx_max,ny_max)
      real tabler_ss(nx_max,ny_max)
      real tabler(ny_max)
      real topo_line(ny_max)
      real drift_start_topo(ny_max)

      parameter (ny_max_tabler=ny_max*1000)
      real topo_1m(ny_max_tabler)
      real tabler_1m(ny_max_tabler)
      real drift_start_topo_1m(ny_max_tabler)

c This program:
c   1) takes the coarse model topography and generates 1.0-m grid
c        increment topo lines;
c   2) uses those to generate the Tabler surface profiles; and
c   3) extracts the profiles at the coarse model grid cells.

c Fill the snow array with topo data.
      do j=1,ny
        do i=1,nx
          tabler_ss(i,j) = topo_land(i,j)
        enddo
      enddo

c Define the length of the i-looping, depending on whether there
c   is any rotation done to get 45 deg winds.
      if (irotate_flag.eq.2) then
        nnx = nx+ny-1
      else
        nnx = nx
      endif

c Required parameters.
      if (irotate_flag.eq.2) then
        dy = 1.41
        test = amod(deltay/1.41,dy/1.41)
      else
        dy = 1.0
        test = amod(deltay,dy)
      endif

      xmax_slope = -0.20

c This Tabler program has not been made general enough to deal
c   with deltax and deltay values that are not evenly divisible
c   by 1.0 (or 1.41 for diagonal profiles).
      if (abs(test).gt.1.0e10-5) then
        print *,'To generate the Tabler surfaces, deltax and deltay'
        print *,'  must be evenly divisible by 1.0 or 1.41.'
        print *,'  deltay = ',deltay
        stop
      endif

c Define the number of 1-m grid cells in each model grid cell, and
c   calculate how many of these are in the entire nx domain.
      nny = nint(deltay/dy)
      maxlines = (ny - 1) * nny + 1

c Define the starting and ending points of the line we are going to
c   work with.  This is done to make sure we are never looking
c   outside the data for numbers to work with.
      jstart = 1
      if (irotate_flag.eq.2) then
        jend = maxlines - (32+11+10+11)
      else
        jend = maxlines - (45+15+15+15)
      endif

c Extract the line we are going to work with.
      do i=1,nnx
        if (irotate_flag.eq.2) then
          do j=1,ny
            II = i - j + 1
            drift_start_topo(j) = 0.0
            if (II.le.0) then
              tabler(j) = tabler(j-1)
              topo_line(j) = tabler(j-1)
            elseif (II.gt.nx) then
              tabler(j) = tabler_ss(nx,i-nx+1)
              topo_line(j) = tabler_ss(nx,i-nx+1)
            else
              tabler(j) = tabler_ss(II,j)
              topo_line(j) = tabler_ss(II,j)
            endif
          enddo
        else
          do j=1,ny
            tabler(j) = tabler_ss(i,j)
            topo_line(j) = topo_land(i,j)
            drift_start_topo(j) = 0.0
          enddo
        endif

c To build the 1.0 m line, use linear interpolation between the
c   model topo data.  Include the end point.
        do j=1,ny-1
          do jj=1,nny
            jjj = (j - 1) * nny + jj
            x1 = 0.0
            x = real(jj - 1) * dy
            y2 = topo_line(j+1)
            y1 = topo_line(j)
            topo_1m(jjj) = y1 + ((y2 - y1)/deltay) * (x - x1)
          enddo
        enddo
        topo_1m((ny - 1) * nny + 1) = topo_line(ny)

c Use this topo array to be the starting point for generating the
c   Tabler surfaces.
        do j=1,maxlines
          tabler_1m(j) = topo_1m(j)
          drift_start_topo_1m(j) = 0.0
        enddo

c Run the Tabler model.
        do j=jstart,jend
          if (irotate_flag.eq.2) then
            t1 = tabler_1m(j)
            t2 = tabler_1m(j+31)
            t3 = tabler_1m(j+31+11)
            t4 = tabler_1m(j+31+21)
            t5 = tabler_1m(j+31+32)

            x1 = (t2 - t1) / 45.0
            x2 = max((t3 - t2) / 15.0,xmax_slope)
            x3 = max((t4 - t3) / 15.0,xmax_slope)
            x4 = max((t5 - t4) / 15.0,xmax_slope)

            y = 0.25*x1 + 0.55*x2 + 0.15*x3 + 0.05*x4

            tabler_1m(j+32) = max(topo_1m(j+32),
     &        tabler_1m(j+31) + y * slope_adjust * dy)
          else
            t1 = tabler_1m(j)
            t2 = tabler_1m(j+44)
            t3 = tabler_1m(j+44+15)
            t4 = tabler_1m(j+44+30)
            t5 = tabler_1m(j+44+45)

            x1 = (t2 - t1) / 45.0
            x2 = max((t3 - t2) / 15.0,xmax_slope)
            x3 = max((t4 - t3) / 15.0,xmax_slope)
            x4 = max((t5 - t4) / 15.0,xmax_slope)

            y = 0.25*x1 + 0.55*x2 + 0.15*x3 + 0.05*x4

            tabler_1m(j+45) = max(topo_1m(j+45),
     &        tabler_1m(j+44) + y * slope_adjust * dy)
          endif
        enddo

c Extract the profile at the model grid points.
        do j=1,ny
          jj = (j - 1) * nny + 1
          tabler(j) = tabler_1m(jj)
          drift_start_topo(j) = drift_start_topo_1m(jj)
        enddo

c Use the 1-D arrays to fill in the 2-D tabler-surface array.
        do j=1,ny
          if (irotate_flag.eq.2) then
            II = i - j + 1
            if (II.ge.1 .and. II.le.nx) then
              tabler_ss(II,j) = tabler(j) + drift_start_topo(j)
            endif
          else
            tabler_ss(i,j) = tabler(j) + drift_start_topo(j)
          endif
        enddo

      enddo

c Convert snow_traps back to actual snow depths instead of
c   depth plus topography.
      do j=1,ny
        do i=1,nx
          tabler_ss(i,j) = tabler_ss(i,j) - topo_land(i,j)
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine tabler_n(nx,ny,topo_land,tabler_nn,deltay,
     &  irotate_flag,slope_adjust)

c This subroutine uses Tabler (1975) to define equilibrium profiles
c   for the topographic drift catchments, for the case of winds
c   from the NORTH and NORTHWEST.

c Tabler, R. D., 1975: Predicting profiles of snowdrifts in
c   topographic catchments.  Proceedings of the 43rd Annual Western
c   Snow Conference, San Diego, California, 87-97.

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,jstart,jend,II,irotate_flag,nny,nnx,
     &  jj,jjj,ny_max_tabler,maxlines
      real deltay,y,slope_adjust,xmax_slope,dy,test,
     &  x,x1,x2,x3,x4,y1,y2,t1,t2,t3,t4,t5

      real topo_land(nx_max,ny_max)
      real tabler_nn(nx_max,ny_max)
      real tabler(ny_max)
      real topo_line(ny_max)
      real drift_start_topo(ny_max)

      parameter (ny_max_tabler=ny_max*1000)
      real topo_1m(ny_max_tabler)
      real tabler_1m(ny_max_tabler)
      real drift_start_topo_1m(ny_max_tabler)

c This program:
c   1) takes the coarse model topography and generates 1.0-m grid
c        increment topo lines;
c   2) uses those to generate the Tabler surface profiles; and
c   3) extracts the profiles at the coarse model grid cells.

c Fill the snow array with topo data.
      do j=1,ny
        do i=1,nx
          tabler_nn(i,j) = topo_land(i,j)
        enddo
      enddo

c Define the length of the i-looping, depending on whether there
c   is any rotation done to get 45 deg winds.
      if (irotate_flag.eq.2) then
        nnx = nx+ny-1
      else
        nnx = nx
      endif

c Required parameters.
      if (irotate_flag.eq.2) then
        dy = 1.41
        test = amod(deltay/1.41,dy/1.41)
      else
        dy = 1.0
        test = amod(deltay,dy)
      endif

      xmax_slope = -0.20

c This Tabler program has not been made general enough to deal
c   with deltax and deltay values that are not evenly divisible
c   by 1.0 (or 1.41 for diagonal profiles).
      if (abs(test).gt.1.0e10-5) then
        print *,'To generate the Tabler surfaces, deltax and deltay'
        print *,'  must be evenly divisible by 1.0 or 1.41.'
        print *,'  deltay = ',deltay
        stop
      endif

c Define the number of 1-m grid cells in each model grid cell, and
c   calculate how many of these are in the entire nx domain.
      nny = nint(deltay/dy)
      maxlines = (ny - 1) * nny + 1

c Define the starting and ending points of the line we are going to
c   work with.  This is done to make sure we are never looking
c   outside the data for numbers to work with.
      jstart = maxlines
      if (irotate_flag.eq.2) then
        jend = 1 + (32+11+10+11)
      else
        jend = 1 + (45+15+15+15)
      endif

c Extract the line we are going to work with.
      do i=1,nnx
        if (irotate_flag.eq.2) then
          do j=1,ny
            II = i - j + 1
            drift_start_topo(j) = 0.0
            if (II.le.0) then
              tabler(j) = tabler(j-1)
              topo_line(j) = tabler(j-1)
            elseif (II.gt.nx) then
              tabler(j) = tabler_nn(nx,i-nx+1)
              topo_line(j) = tabler_nn(nx,i-nx+1)
            else
              tabler(j) = tabler_nn(II,j)
              topo_line(j) = tabler_nn(II,j)
            endif
          enddo
        else
          do j=1,ny
            tabler(j) = tabler_nn(i,j)
            topo_line(j) = topo_land(i,j)
            drift_start_topo(j) = 0.0
          enddo
        endif

c To build the 1.0 m line, use linear interpolation between the
c   model topo data.  Include the end point.
        do j=1,ny-1
          do jj=1,nny
            jjj = (j - 1) * nny + jj
            x1 = 0.0
            x = real(jj - 1) * dy
            y2 = topo_line(j+1)
            y1 = topo_line(j)
            topo_1m(jjj) = y1 + ((y2 - y1)/deltay) * (x - x1)
          enddo
        enddo
        topo_1m((ny - 1) * nny + 1) = topo_line(ny)

c Use this topo array to be the starting point for generating the
c   Tabler surfaces.
        do j=1,maxlines
          tabler_1m(j) = topo_1m(j)
          drift_start_topo_1m(j) = 0.0
        enddo

c Run the Tabler model.
        do j=jstart,jend,-1
          if (irotate_flag.eq.2) then
            t1 = tabler_1m(j)
            t2 = tabler_1m(j-31)
            t3 = tabler_1m(j-31-11)
            t4 = tabler_1m(j-31-21)
            t5 = tabler_1m(j-31-32)

            x1 = (t2 - t1) / 45.0
            x2 = max((t3 - t2) / 15.0,xmax_slope)
            x3 = max((t4 - t3) / 15.0,xmax_slope)
            x4 = max((t5 - t4) / 15.0,xmax_slope)

            y = 0.25*x1 + 0.55*x2 + 0.15*x3 + 0.05*x4

            tabler_1m(j-32) = max(topo_1m(j-32),
     &        tabler_1m(j-31) + y * slope_adjust * dy)
          else
            t1 = tabler_1m(j)
            t2 = tabler_1m(j-44)
            t3 = tabler_1m(j-44-15)
            t4 = tabler_1m(j-44-30)
            t5 = tabler_1m(j-44-45)

            x1 = (t2 - t1) / 45.0
            x2 = max((t3 - t2) / 15.0,xmax_slope)
            x3 = max((t4 - t3) / 15.0,xmax_slope)
            x4 = max((t5 - t4) / 15.0,xmax_slope)

            y = 0.25*x1 + 0.55*x2 + 0.15*x3 + 0.05*x4

            tabler_1m(j-45) = max(topo_1m(j-45),
     &        tabler_1m(j-44) + y * slope_adjust * dy)
          endif
        enddo

c Extract the profile at the model grid points.
        do j=1,ny
          jj = (j - 1) * nny + 1
          tabler(j) = tabler_1m(jj)
          drift_start_topo(j) = drift_start_topo_1m(jj)
        enddo

c Use the 1-D arrays to fill in the 2-D tabler-surface array.
        do j=1,ny
          if (irotate_flag.eq.2) then
            II = i - j + 1
            if (II.ge.1 .and. II.le.nx) then
              tabler_nn(II,j) = tabler(j) + drift_start_topo(j)
            endif
          else
            tabler_nn(i,j) = tabler(j) + drift_start_topo(j)
          endif
        enddo

      enddo

c Convert snow_traps back to actual snow depths instead of
c   depth plus topography.
      do j=1,ny
        do i=1,nx
          tabler_nn(i,j) = tabler_nn(i,j) - topo_land(i,j)
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine surface_snow_1(Tair,windspd_2m,prec,ro_soft_snow,
     &    Utau_t,ro_soft_snow_old,dt,ro_nsnow)

      implicit none

      real C,alfa,ro_min,ro_max,prec,Tair,ro_nsnow,dt,Tf,
     &  Utau_t,ro_soft_snow_old,ro_soft_snow,windspd_2m

c Define the density rate coefficients.
      C = 0.10

c Define alfa. 
      alfa = 0.2

c Define the minimum and maximum snow density that should be
c   simulated.
      ro_min = 50.0
      ro_max = 450.0

c Freezing temperature.
      Tf = 273.15

c Calculate the new snow density.  First calculate this under the
c   assumption of no wind using the standard SnowModel formulation,
c   then calculate the offset for wind speeds > 5 m/s.
      if (prec.gt.0.0) then
        ro_soft_snow = ro_nsnow
        ro_soft_snow = min(ro_soft_snow,ro_max)
        ro_soft_snow = max(ro_soft_snow,ro_min)
        if (ro_soft_snow.le.300.0) then
          Utau_t = 0.10 * exp(0.003 * ro_soft_snow)
        else
          Utau_t = 0.005 * exp(0.013 * ro_soft_snow)
        endif
        ro_soft_snow_old = ro_soft_snow
      else
        call surface_snow_2(ro_soft_snow_old,ro_soft_snow,Utau_t,
     &    dt,Tair,windspd_2m,C,ro_max,ro_min,alfa,Tf)
        ro_soft_snow_old = ro_soft_snow
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine surface_snow_2(ro_soft_snow_old,ro_soft_snow,Utau_t,
     &  dt,Tair,windspd_2m,C,ro_max,ro_min,alfa,Tf)

      implicit none

      real Tf,Tsnow,Tair,ro_soft_snow,dt,A1,A2,B,U,C,
     &  windspd_2m,Utau_t,ro_soft_snow_old,ro_max,ro_min,alfa

      A1 = 0.0013
      A2 = 0.021
      B = 0.08

c Evolve the near-surface snow density under the influence of
c   temperature and snow-transporting wind speeds.

c Assume that the near-surface snow temperature equals the air
c   temperature, but is not above the melting point.
      Tsnow = min(Tf,Tair)

c Update the snow density of the soft snow layer.  Eliminate the
c   wind speed influence for speeds below 5 m/s, but account for it
c   if speeds are >= 5 m/s.
      if (windspd_2m.ge.5.0) then
        U = 5.0 + 15.0 * (1.0 - exp(-(alfa*(windspd_2m - 5.0))))
      else
        U = 1.0
      endif

      ro_soft_snow = ro_soft_snow_old + dt *
     &  (C * A1 * U * ro_soft_snow_old *
     &  exp((- B)*(Tf-Tsnow)) * exp((- A2)*ro_soft_snow_old))

c Bound the calculated density.
      ro_soft_snow = min(ro_max,ro_soft_snow)
      ro_soft_snow = max(ro_min,ro_soft_snow)

c Calculate the snow threshold friction velocity.
      if (ro_soft_snow.le.300.0) then
        Utau_t = 0.10 * exp(0.003 * ro_soft_snow)
      else
        Utau_t = 0.005 * exp(0.013 * ro_soft_snow)
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

