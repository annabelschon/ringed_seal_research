c 1_write_csv_sm_points_annual.f

c Read SnowModel .gdat file outputs, and output time series at
c   specific grid cells, for different variables.

c NOTE THAT THE .csv FORMAT OPTION REQUIRES COMPILING WITH
c   GFORTRAN.  THIS FORMAT IS NICE BECAUSE IT SHOULD WORK
c   WITHOUT FORMAT CHANGES WHEN YOU CHANGE THE VARIABLES AND
c   THE NUMBER OF VARIABLES YOU ARE WRITING OUT.

      implicit none

      integer nx,ny,npts,nyears,nvars,ioffset

c Number of points in the SnowModel grid.
      parameter (nx=232,ny=448)

c Number of time writes.  THIS IS THE NUMBER OF YEARS NOW.
      parameter (nyears=13)

c Number of grid cells to extract.
      parameter (npts=73)

c Number of variables in the yearly .gdat file.
      parameter (nvars=18)

      real prec_sum(nx,ny)
      real rpre_sum(nx,ny)
      real spre_sum(nx,ny)

      real snod_max(nx,ny)
      real snod_max_dos(nx,ny)
      real snod_max_doy(nx,ny)

      real swed_max(nx,ny)
      real tair_ave(nx,ny)

      real snow_onset_dos(nx,ny)
      real snow_onset_doy(nx,ny)
      real snow_free_dos(nx,ny)
      real snow_free_doy(nx,ny)
      real core_snow_days(nx,ny)

      real snow_first_dos(nx,ny)
      real snow_first_doy(nx,ny)
      real snow_last_dos(nx,ny)
      real snow_last_doy(nx,ny)
      real total_snow_days(nx,ny)

      real xlat(npts),xlon(npts)

      integer ii(npts),jj(npts)

      integer i,j,iter,iyr_start,k,iyr

      character*2 stnid
      character*4 stn_name

c Define the header line.
      character header*(*) 
      parameter (header=
     &  'site_id,year,'//
     &  'longitude(decimal_degrees),latitude(decimal_degrees),'//

     &  'total_annual_water_equiv_precip(m),'//
     &  'total_annual_rainfall(m),'//
     &  'total_annual_snowfall(m),'//

     &  'maximum_snow_depth(m),'//
     &  'day_of_maximum_snow_depth(day_since_1Sep),'//
     &  'day_of_maximum_snow_depth(day_of_year),'//

     &  'maximum_SWE_depth(m),'//

     &  'average_air_temperature(deg_C),'//

     &  'core_snow_season_snow_onset_day(day_since_1Sep),'//
     &  'core_snow_season_snow_onset_day(day_of_year),'//
     &  'core_snow_season_snow_free_day(day_since_1Sep),'//
     &  'core_snow_season_snow_free_day(day_of_year),'//
     &  'length_of_core_snow_season(number_of_days),'//

     &  'first_snow_occurrence(day_since_1Sep),'//
     &  'first_snow_occurrence(day_of_year),'//
     &  'last_snow_occurrence(day_since_1Sep),'//
     &  'last_snow_occurrence(day_of_year),'//
     &  'total_number_of_days_with_snow(number_of_days)')

c Define the path where the SnowModel start time is located.
      character path1*(*) 
      parameter (path1='../../met/merra2/')
     
c Define the path where the SnowModel output data are located.
      character path2*(*) 
      parameter (path2='../3_daily_to_yearly/1_mk_yearly_data/')

c Define the path where the station (i,j) information is located.
      character path3*(*)
      parameter (path3='../2_extract_data_all_sites/3_mk_stn_ij/')

c Define the path where the station lon-lat information is located.
      character path4*(*)
      parameter (path4='../2_extract_data_all_sites/2_ll_to_proj/')

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Read the station lat-lon data.
      open (45,file=path4//'stn_ll.dat')
      do k=1,npts
        read (45,*) xlon(k),xlat(k)
      enddo

c Read the station (i,j) data.
      open (31,file=path3//'stn_ij_coords_col.dat')
      do k=1,npts
        read (31,*) ii(k),jj(k)
      enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Read in the start date for this run.
      open (51,file=path1//
     &  '4_maxiter_offset/start_end_dates_maxiter_ioffset.dat')

      read (51,99) iyr_start

   99 format (14x,i10)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Open the SnowModel output files.
c The record length of this gdat file is 4*nx*ny*the number of
c   annual summary variables. If you look at the .ctl file
c   and the program used to produce this .gdat file at file 
c   path2, you will see which annual summary variables were
c   saved in the .gdat file.
      open (unit=21,file=path2//'yearly_variables.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Run the data extraction and write procedure.  Write out a
c   separate file for each "station" point.
      do k=1,npts

c Define the station names and file names.
        stn_name = 'Site'
        write (stnid,'(i2.2)') k

c Open the output files.
        open (unit=200+k,file=
     &    'data_annual/SM_Data_annual_'//stn_name//stnid//'.csv')

c Write out the header to the output files.
        write (200+k,999) header

      enddo

c Read and write the time-series data to each file.
      do iter=1,nyears

        iyr = iter - 1 + iyr_start + 1

        ioffset = (iter - 1) * nvars

        print *, iter,ioffset

c Read in the data.
        read (21,rec=ioffset+1) ((prec_sum(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+2) ((rpre_sum(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+3) ((spre_sum(i,j),i=1,nx),j=1,ny)

        read (21,rec=ioffset+4) ((snod_max(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+5) ((snod_max_dos(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+6) ((snod_max_doy(i,j),i=1,nx),j=1,ny)

        read (21,rec=ioffset+7) ((swed_max(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+8) ((tair_ave(i,j),i=1,nx),j=1,ny)

        read (21,rec=ioffset+9) ((snow_onset_dos(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+10) ((snow_onset_doy(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+11) ((snow_free_dos(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+12) ((snow_free_doy(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+13) ((core_snow_days(i,j),i=1,nx),j=1,ny)

        read (21,rec=ioffset+14) ((snow_first_dos(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+15) ((snow_first_doy(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+16) ((snow_last_dos(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+17) ((snow_last_doy(i,j),i=1,nx),j=1,ny)
        read (21,rec=ioffset+18) ((total_snow_days(i,j),i=1,nx),j=1,ny)

c Extract the point data and write each station's data to a
c   separate .csv file.  The "nint"s here are just making the
c   numbers (decimal days) into integer days.
        do k=1,npts

          write (200+k,998)
     &      k,iyr,

c Note below in the "998 format" statement that I am writing out
c   the lon-lat data to 5 decimal places (the equivalent to ~1m
c   resolution; anything more hardly seems justified unless it
c   was collected with a differential GPS system, or something).
     &      xlon(k),xlat(k),

     &      prec_sum(ii(k),jj(k)),
     &      rpre_sum(ii(k),jj(k)),
     &      spre_sum(ii(k),jj(k)),

     &      snod_max(ii(k),jj(k)),
     &      nint(snod_max_dos(ii(k),jj(k))),
     &      nint(snod_max_doy(ii(k),jj(k))),

     &      swed_max(ii(k),jj(k)),
     &      tair_ave(ii(k),jj(k)),

     &      nint(snow_onset_dos(ii(k),jj(k))),
     &      nint(snow_onset_doy(ii(k),jj(k))),
     &      nint(snow_free_dos(ii(k),jj(k))),
     &      nint(snow_free_doy(ii(k),jj(k))),
     &      nint(core_snow_days(ii(k),jj(k))),

     &      nint(snow_first_dos(ii(k),jj(k))),
     &      nint(snow_first_doy(ii(k),jj(k))),
     &      nint(snow_last_dos(ii(k),jj(k))),
     &      nint(snow_last_doy(ii(k),jj(k))),
     &      nint(total_snow_days(ii(k),jj(k)))

        enddo

      enddo

c The .csv (comma delimited) format option.  NOTE: this requires
c   compiling with the gfortran compiler.

c Here the "*" says to process all of the output numbers that
c   are provided; "G" means adjust the format to account for
c   character, integer, real or logical values (G=generic); the
c   "0" means take up no more space than necessary; the "8"
c   means output 8 significant digits for real numbers; the ":"
c   terminates the format if the input list after all of the
c   variables are written out (this means there is no comma
c   after the last number); and the "," puts a comma between
c   the numbers.

c 999 format (*(G0.8,:,","))

c By leaving off the .8, it writes out all available precision.
c   These files are small, so I don't think it matters if you
c   do this or not.

c Note: here I am forcing the lon-lat writes to be written out
c   to 5 decimal places, and the moisture variables to be written
c   out to 6 decimal places (=0.001 mm), and the rest of the
c   values to be general, as described above.
  998 format (2(I0,","),2(F0.5,","),*(G0.6,:,","))

  999 format (*(G0,:,","))

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

