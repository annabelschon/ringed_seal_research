c 1_seaice_aves_trends.f

c This calculates both the spatially distributed and domain
c   averaged trends.

c CALCULATE THE LINEAR TRENDS THROUGH EACH GRID POINT.

      implicit none

      integer nx,ny,nyears,nmonths,nmonths_all

      parameter (nx=304,ny=448)
      parameter (nyears=43)
      parameter (nmonths=12)
      parameter (nmonths_all=nyears*nmonths)

c All data.
      real conc(nx,ny,nmonths_all)

c Variables.
      real conc_month(nx,ny,nyears)

c Averages.
      real a_conc_month(nx,ny)

c Slopes.
      real s_conc_month(nx,ny)

c Domain-averaged trends.
      real t_conc_month(5,nyears)
      real t_conc_month_all(5,nmonths,nyears)

c Misc.
      integer i,j,nyear,iyr_start,n,nmo,nyr,irec_month
      real undef
      real undef_mask(nx,ny)

c Define where the data are located.
      character path*(*)
      parameter (path =
     & '/data3/annabel/seals/1_nsidc_0051/5_gdat_monthly/')

c Define the variable names that correspond to the .gdat file
c   writes.  I list them in a column here so it better corresponds
c   to the .gdat writes listed below; these must be listed in the
c   same order as the data writes, and they must be in single
c   quotes.
      character*20 var_string

      data var_string /'conc_month'/

c Undefined value.
      undef = -9999.0

c Start date.
      iyr_start = 1980

c Input file.
      open (21,file=path//'annabel_monthly.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Output files.
      open (31,file='averages_2D.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      open (32,file='trends_2D.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      open (41,file='domain_average_trends.gdat',
     &  form='unformatted',access='direct',recl=4*5*nmonths,
     &  status='replace')

c Read in all of the data.
      print *
      print *, 'reading in monthly data'
      print *
      do nmo=1,nmonths_all
        read (21,rec=nmo) ((conc(i,j,nmo),i=1,nx),j=1,ny)
      enddo

c Loop through each month in the year (1-12), extracting the
c   specific month of interest, and building a new input array
c   that has dimensions of (nx,ny,nyears), for each month.  And
c   then send that new array in for the trend analysis.  Repeat
c   this for months 1-12. 

      do nmo=1,nmonths

        do nyr=1,nyears

          irec_month = (nyr - 1)*nmonths+nmo

c         Given the start month of simulation year is August,
c            nmo= 1 = August, 2 = September, ..., 12 = July)
          print *, 'Month:', nmo, 'Year:', nyr, 'Record:', irec_month

          do j=1,ny
            do i=1,nx
              conc_month(i,j,nyr) = conc(i,j,irec_month)
            enddo
          enddo
        enddo

c For these kinds of average and trend calculations, if there are
c   undef values in the time-series for a particular grid cell,
c   it is critical that they are undef for the entire time-series
c   at that grid cell.  So, clean that up here, before doing the
c   next calculations.

c First, initialize an undef mask array.
        do j=1,ny
          do i=1,nx
            undef_mask(i,j) = 1.0
          enddo
        enddo

c Then, find any undef grid cells in the entire time-series.
        do nyr=1,nyears
          do j=1,ny
            do i=1,nx
              if (conc_month(i,j,nyr).eq.undef) undef_mask(i,j) = undef
            enddo
          enddo
        enddo

c Now, set any undef grid cells in the undef_mask to undef values
c   in the entire time-series.
        do nyr=1,nyears
          do j=1,ny
            do i=1,nx
              if (undef_mask(i,j).eq.undef) conc_month(i,j,nyr) = undef
            enddo
          enddo
        enddo

c Calculate the trends.
        print *,'calculating trends, nmonth =',nmo
        call linear1 (nx,ny,nyears,conc_month,s_conc_month,a_conc_month)

c Save the averages.
        write (31,rec=nmo) ((a_conc_month(i,j),i=1,nx),j=1,ny)

c Save the slopes.
        write (32,rec=nmo) ((s_conc_month(i,j),i=1,nx),j=1,ny)

c CALCULATE THE DOMAIN-AVERAGED AVERAGES AND TRENDS.

c Calculate the slope, intercept, and r^2 for the variables, over
c   the SnowModel domain.

c Note that here I have adopted the following convention:
c   i=x=1 = t_var(1,k) = a_var(k) = domain average for each year
c   i=x=2 = t_var(2,k) = s_var * real(k) + b_var = linear fit
c   i=x=3 = t_var(3,k) = s_var = slope of the line
c   i=x=4 = t_var(4,k) = b_var = y intercept
c   i=x=5 = t_var(5,k) = r2_var = r^2 fit to the linear line

        call calc_domain_ave_stats (nx,ny,nyears,conc_month,undef,
     &    conc_month,t_conc_month)

c Build an array that holds the domain-averaged information (for
c   each month).  This is being done to make it easier to write
c   the data to a .gdat file (because the .gdat file is expecting
c   all of the data to march forward in time).
        do nyear=1,nyears
          do i=1,5
            t_conc_month_all(i,nmo,nyear) = t_conc_month(i,nyear)
          enddo
        enddo

      enddo

c Save the domain-averaged results.  Note that I am treating the
c   nmo as y, in the .gdat file, and marching forward in time
c   (years).  This is going to allow me to pick a month of
c   interest, and plot the 42 years of data.
      do nyear=1,nyears
        write (41,rec=nyear)
     &    ((t_conc_month_all(i,n,nyear),i=1,5),n=1,nmonths)
      enddo

c Make the grads .ctl files.
      call make_2d_averages_ctl_file (nx,ny,1,1001,
     &  var_string)

      call make_2d_trends_ctl_file (nx,ny,1,1001,
     &  var_string)

      call make_domain_ave_trends_ctl_file (1,nyears,
     &  iyr_start,var_string)

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine linear1 (nx,ny,npts,var,s,a)

      implicit none

      integer nx,ny,npts,i,j,n

      real var(nx,ny,npts),s(nx,ny),a(nx,ny)

      real sumx,sumx2,sumxy,sumy,sumy2,x,y

c Pull the time series from each grid point, fit a line through the
c   data, and save the slope (trend).
      do j=1,ny
        do i=1,nx
          sumx  = 0.0
          sumx2 = 0.0
          sumxy = 0.0
          sumy  = 0.0
          sumy2 = 0.0
          do n=1,npts
             x = real(n)
             y = var(i,j,n)

             sumx  = sumx + x
             sumx2 = sumx2 + x * x
             sumxy = sumxy + x * y
             sumy  = sumy + y
             sumy2 = sumy2 + y * y
          enddo

c Compute average
          a(i,j) = sumy / real(npts)

c Compute slope
          s(i,j) = (real(npts) * sumxy - sumx * sumy) /
     &      (real(npts) * sumx2 - sumx**2)

c Compute y-intercept
c         b = (sumy * sumx2 - sumx * sumxy) /
c           (real(npts) * sumx2 - sumx**2)

c Compute correlation coefficient
c         r = (sumxy - sumx * sumy / real(npts)) /
c    &      sqrt((sumx2 - sumx**2/real(npts)) *
c    &      (sumy2 - sumy**2/real(npts)))
c         r2 = r*r

        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_2d_averages_ctl_file (nx,ny,nvars,iyr_start,
     &  var_string)

      implicit none

      integer nx,ny,iyr_start,nvars,nvar

      character*20 var_string(nvars)

      open (71,file='averages_2D.ctl')

      write (71,51)
      write (71,52)
      write (71,53)
      write (71,54) nx
      write (71,55) ny

      write (71,56)
      write (71,57) 12,iyr_start
      write (71,58) nvars

      do nvar=1,nvars
        write (71,100) var_string(nvar)
      enddo

      write (71,68)

      close (71)

   51 format ('DSET ^averages_2D.gdat')
   52 format ('TITLE multi-year SnowModel averages')
   53 format ('UNDEF -9999.0')

   54 format ('XDEF ',i8,' LINEAR  1.0  1.0')
   55 format ('YDEF ',i8,' LINEAR  1.0  1.0')
   56 format ('ZDEF        1 LINEAR  1.0  1.0')

   57 format ('TDEF ',i8,' LINEAR  01AUG',i4,' 1mo')

   58 format ('VARS     ',i4)

  100 format (a,' 0  0  xxxxxxxxxx')

   68 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_2d_trends_ctl_file (nx,ny,nvars,iyr_start,
     &  var_string)

      implicit none

      integer nx,ny,iyr_start,nvars,nvar

      character*20 var_string(nvars)

      open (71,file='trends_2D.ctl')

      write (71,51)
      write (71,52)
      write (71,53)
      write (71,54) nx
      write (71,55) ny

      write (71,56)
      write (71,57) 12,iyr_start
      write (71,58) nvars

      do nvar=1,nvars
        write (71,100) var_string(nvar)
      enddo

      write (71,68)

      close (71)

   51 format ('DSET ^trends_2D.gdat')
   52 format ('TITLE multi-year SnowModel trends')
   53 format ('UNDEF -9999.0')

   54 format ('XDEF ',i8,' LINEAR  1.0  1.0')
   55 format ('YDEF ',i8,' LINEAR  1.0  1.0')
   56 format ('ZDEF        1 LINEAR  1.0  1.0')

   57 format ('TDEF ',i8,' LINEAR  01AUG',i4,' 1mo')

   58 format ('VARS     ',i4)

  100 format (a,' 0  0  xxxxxxxxxx')

   68 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine calc_domain_ave_stats (nx,ny,nyears,tair_ave,undef,
     &  var,t_var)

      implicit none

      integer nx,ny,k,nyears,i,j

      real count,undef,s_var,b_var,r2_var

      real tair_ave(nx,ny,nyears)
      real var(nx,ny,nyears)
      real t_var(5,nyears)
      real a_var(nyears)

c Average the area and variable of interest.
      do k=1,nyears
        count = 0.0
        a_var(k) = 0.0
        do j=1,ny
          do i=1,nx
            if (tair_ave(i,j,k).ne.undef) then
              count = count + 1.0
              a_var(k) = a_var(k) + var(i,j,k)
            endif
          enddo
        enddo
        a_var(k) = a_var(k) / count
      enddo

c Calculate the trends.
      call linear2 (nyears,a_var,s_var,b_var,r2_var)

c Build the lines.
      do k=1,nyears
        t_var(1,k) = a_var(k)
        t_var(2,k) = s_var * real(k) + b_var
        t_var(3,k) = s_var
        t_var(4,k) = b_var
        t_var(5,k) = r2_var
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine linear2 (npts,var,s,b,r2)

      implicit none

      integer npts,n

      real var(npts),s

      real sumx,sumx2,sumxy,sumy,sumy2,x,y,b,r,r2

c Pull the time series from each grid point, fit a line through the
c   data, and save the slope (trend).
      sumx  = 0.0
      sumx2 = 0.0
      sumxy = 0.0
      sumy  = 0.0
      sumy2 = 0.0
      do n=1,npts
         x = real(n)
         y = var(n)

         sumx  = sumx + x
         sumx2 = sumx2 + x * x
         sumxy = sumxy + x * y
         sumy  = sumy + y
         sumy2 = sumy2 + y * y
      enddo

c Compute slope
      s = (npts * sumxy - sumx * sumy) /
     &  (npts * sumx2 - sumx**2)

c Compute y-intercept
      b = (sumy * sumx2 - sumx * sumxy) / (npts * sumx2 - sumx**2)

c Compute correlation coefficient
      r = (sumxy - sumx * sumy / npts) /
     &  sqrt((sumx2 - sumx**2/npts) * (sumy2 - sumy**2/npts))
      r2 = r*r

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_domain_ave_trends_ctl_file (nvars,nyears,
     &  iyr_start,var_string)

      implicit none

      integer iyr_start,nvars,nvar,nyears

      character*20 var_string(nvars)

      open (71,file='domain_average_trends.ctl')

      write (71,51)
      write (71,52)
      write (71,53)
      write (71,54) 5
      write (71,55) 12

      write (71,56)
      write (71,57) nyears,iyr_start+1
      write (71,58) nvars

      do nvar=1,nvars
        write (71,100) var_string(nvar)
      enddo

      write (71,68)

      close (71)

   51 format ('DSET ^domain_average_trends.gdat')
   52 format ('TITLE yearly SnowModel trends')
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

