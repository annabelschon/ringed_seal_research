c SnowModel_aves_trends.f

c This calculates both the spatially distributed and domain
c   averaged trends.

c CALCULATE THE LINEAR TRENDS THROUGH EACH GRID POINT.

      implicit none

      integer nx,ny,i,j,nyears,nyear,nvars,iyr_start,
     &  imo_start,idy_start

      parameter (nx=690,ny=990)
      parameter (nyears=34)
      parameter (nvars=7)

c Variables.
      real conc_a1a(nx,ny,nyears)
      real spre_sum(nx,ny,nyears)
      real snod_1ap(nx,ny,nyears)
      real swed_1ap(nx,ny,nyears)
      real smlt_s1a(nx,ny,nyears)
      real bsfx_s1a(nx,ny,nyears)
      real tair_a1a(nx,ny,nyears)

c Averages.
      real a_conc_a1a(nx,ny)
      real a_spre_sum(nx,ny)
      real a_snod_1ap(nx,ny)
      real a_swed_1ap(nx,ny)
      real a_smlt_s1a(nx,ny)
      real a_bsfx_s1a(nx,ny)
      real a_tair_a1a(nx,ny)

c Slopes.
      real s_conc_a1a(nx,ny)
      real s_spre_sum(nx,ny)
      real s_snod_1ap(nx,ny)
      real s_swed_1ap(nx,ny)
      real s_smlt_s1a(nx,ny)
      real s_bsfx_s1a(nx,ny)
      real s_tair_a1a(nx,ny)

c Domain-averaged trends.
      real t_conc_a1a(5,nyears)
      real t_spre_sum(5,nyears)
      real t_snod_1ap(5,nyears)
      real t_swed_1ap(5,nyears)
      real t_smlt_s1a(5,nyears)
      real t_bsfx_s1a(5,nyears)
      real t_tair_a1a(5,nyears)

c Misc.
      real undef

c Define the variable names that correspond to the .gdat file
c   writes.  I list them in a column here so it better corresponds
c   to the .gdat writes listed below; these must be listed in the
c   same order as the data writes, and they must be in single
c   quotes.
      character*20 var_string(nvars)

      data var_string /
     &  'conc_a1a',
     &  'spre_sum',
     &  'snod_1ap',
     &  'swed_1ap',
     &  'smlt_s1a',
     &  'bsfx_s1a',
     &  'tair_a1a'/

      undef = -9999.0

      iyr_start = 1987
      imo_start = 9
      idy_start = 1

c Input file.
      open (21,file='../1_mk_yearly_data/yearly_variables.gdat',
     &  form='unformatted',access='direct',recl=4*nvars*nx*ny)

c Read in all of the data.
      do nyear=1,nyears

        print *,'reading year = ',nyear

        read (21,rec=nyear)
     &    ((conc_a1a(i,j,nyear),i=1,nx),j=1,ny),
     &    ((spre_sum(i,j,nyear),i=1,nx),j=1,ny),
     &    ((snod_1ap(i,j,nyear),i=1,nx),j=1,ny),
     &    ((swed_1ap(i,j,nyear),i=1,nx),j=1,ny),
     &    ((smlt_s1a(i,j,nyear),i=1,nx),j=1,ny),
     &    ((bsfx_s1a(i,j,nyear),i=1,nx),j=1,ny),
     &    ((tair_a1a(i,j,nyear),i=1,nx),j=1,ny)
      enddo

c Calculate the trends.
      print *,'calculating trends'
      call linear1(nx,ny,nyears,conc_a1a,s_conc_a1a,a_conc_a1a)
      call linear1(nx,ny,nyears,spre_sum,s_spre_sum,a_spre_sum)
      call linear1(nx,ny,nyears,snod_1ap,s_snod_1ap,a_snod_1ap)
      call linear1(nx,ny,nyears,swed_1ap,s_swed_1ap,a_swed_1ap)
      call linear1(nx,ny,nyears,smlt_s1a,s_smlt_s1a,a_smlt_s1a)
      call linear1(nx,ny,nyears,bsfx_s1a,s_bsfx_s1a,a_bsfx_s1a)
      call linear1(nx,ny,nyears,tair_a1a,s_tair_a1a,a_tair_a1a)

c Before saving the data, set undefined values back to undefined.
      do j=1,ny
        do i=1,nx

          if (conc_a1a(i,j,1).eq.undef) then
            a_conc_a1a(i,j) = undef
            s_conc_a1a(i,j) = undef
          endif

          if (spre_sum(i,j,1).eq.undef) then
            a_spre_sum(i,j) = undef
            s_spre_sum(i,j) = undef
          endif

          if (snod_1ap(i,j,1).eq.undef) then
            a_snod_1ap(i,j) = undef
            s_snod_1ap(i,j) = undef
          endif

          if (swed_1ap(i,j,1).eq.undef) then
            a_swed_1ap(i,j) = undef
            s_swed_1ap(i,j) = undef
          endif

          if (smlt_s1a(i,j,1).eq.undef) then
            a_smlt_s1a(i,j) = undef
            s_smlt_s1a(i,j) = undef
          endif

          if (bsfx_s1a(i,j,1).eq.undef) then
            a_bsfx_s1a(i,j) = undef
            s_bsfx_s1a(i,j) = undef
          endif

          if (tair_a1a(i,j,1).eq.undef) then
            a_tair_a1a(i,j) = undef
            s_tair_a1a(i,j) = undef
          endif

        enddo
      enddo

c Save the averages.
      open (31,file='averages_2D.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      write (31,rec=1) ((a_conc_a1a(i,j),i=1,nx),j=1,ny)
      write (31,rec=2) ((a_spre_sum(i,j),i=1,nx),j=1,ny)
      write (31,rec=3) ((a_snod_1ap(i,j),i=1,nx),j=1,ny)
      write (31,rec=4) ((a_swed_1ap(i,j),i=1,nx),j=1,ny)
      write (31,rec=5) ((a_smlt_s1a(i,j),i=1,nx),j=1,ny)
      write (31,rec=6) ((a_bsfx_s1a(i,j),i=1,nx),j=1,ny)
      write (31,rec=7) ((a_tair_a1a(i,j),i=1,nx),j=1,ny)

c Save the slopes.
      open (32,file='trends_2D.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      write (32,rec=1) ((s_conc_a1a(i,j),i=1,nx),j=1,ny)
      write (32,rec=2) ((s_spre_sum(i,j),i=1,nx),j=1,ny)
      write (32,rec=3) ((s_snod_1ap(i,j),i=1,nx),j=1,ny)
      write (32,rec=4) ((s_swed_1ap(i,j),i=1,nx),j=1,ny)
      write (32,rec=5) ((s_smlt_s1a(i,j),i=1,nx),j=1,ny)
      write (32,rec=6) ((s_bsfx_s1a(i,j),i=1,nx),j=1,ny)
      write (32,rec=7) ((s_tair_a1a(i,j),i=1,nx),j=1,ny)

c Make the grads .ctl files.  Here it just defines the grid in
c   (i,j) units.  The 'years' correspond to the date of the
c   ending simulation year (so if it started in Sep 2004, the
c   year will be 2005).
      call make_2d_averages_ctl_file (nx,ny,nvars,9999,
     &  var_string)

      call make_2d_trends_ctl_file (nx,ny,nvars,9999,
     &  var_string)

c CALCULATE THE DOMAIN-AVERAGED AVERAGES AND TRENDS.

c Calculate the slope, intercept, and r^2 for the variables, over
c   the SnowModel domain.

c Because I can't average a domain that has "doy" values that span
c   over the 1 Jan date boundary (like 1 and 365 give you 183),
c   here I am going to calculate the "dos" averages and then
c   convert them to "doy" values.

c Actually, for now, I am going to just do the calcuations for the
c   "doy" arrays.  Then if there is ever a problem with the line
c   fits, etc., I will figure out how to fix these calculations.
      call calc_domain_ave_stats(nx,ny,nyears,conc_a1a,undef,
     &  conc_a1a,t_conc_a1a)
      call calc_domain_ave_stats(nx,ny,nyears,spre_sum,undef,
     &  spre_sum,t_spre_sum)
      call calc_domain_ave_stats(nx,ny,nyears,snod_1ap,undef,
     &  snod_1ap,t_snod_1ap)
      call calc_domain_ave_stats(nx,ny,nyears,swed_1ap,undef,
     &  swed_1ap,t_swed_1ap)
      call calc_domain_ave_stats(nx,ny,nyears,smlt_s1a,undef,
     &  smlt_s1a,t_smlt_s1a)
      call calc_domain_ave_stats(nx,ny,nyears,bsfx_s1a,undef,
     &  bsfx_s1a,t_bsfx_s1a)
      call calc_domain_ave_stats(nx,ny,nyears,tair_a1a,undef,
     &  tair_a1a,t_tair_a1a)

c Save the results.  Note that here I have adopted the following
c   convention:
c   i=x=1 = t_var(1,k) = a_var(k) = domain average for each year
c   i=x=2 = t_var(2,k) = s_var * real(k) + b_var = linear fit
c   i=x=3 = t_var(3,k) = s_var = slope of the line
c   i=x=4 = t_var(4,k) = b_var = y intercept
c   i=x=5 = t_var(5,k) = r2_var = r^2 fit to the linear line

      open (41,file='domain_average_trends.gdat',
     &  form='unformatted',access='direct',recl=4*5*nvars,
     &  status='replace')

      do nyear=1,nyears
        write (41,rec=nyear)
     &    (t_conc_a1a(i,nyear),i=1,5),
     &    (t_spre_sum(i,nyear),i=1,5),
     &    (t_snod_1ap(i,nyear),i=1,5),
     &    (t_swed_1ap(i,nyear),i=1,5),
     &    (t_smlt_s1a(i,nyear),i=1,5),
     &    (t_bsfx_s1a(i,nyear),i=1,5),
     &    (t_tair_a1a(i,nyear),i=1,5)
      enddo

c Make the associated grads .ctl file.
      call make_domain_ave_trends_ctl_file (nvars,nyears,
     &  iyr_start,var_string)

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine dos_to_doy (nx,ny,imo_start,idy_start,
     &  snow_dos,snow_doy)

      implicit none

      integer i,j,nx,ny,imo_start,idy_start,idoy_start_of_sim,
     &  iyr_beg,ioptn,idy,imo,idoy_31Dec

      real doy_start_of_sim,doy_31Dec,delta_days
      real snow_dos(nx,ny)
      real snow_doy(nx,ny)

c I don't see how to deal with leap years here, because I am
c   dealing with multi-year-average data.  So, I am going
c   to just assume that they are not an issue here and that
c   the year is 365 days long.

c Define an arbitrary year that hs 365 days in it.
      iyr_beg = 2001

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

c Convert the day-of-simulation to day-of-year.
      delta_days = doy_31Dec - doy_start_of_sim + 1

c     print *,doy_start_of_sim,doy_31Dec,delta_days

      do j=1,ny
        do i=1,nx
          if (snow_dos(i,j).le.delta_days) then
            snow_doy(i,j) = snow_dos(i,j) +
     &        doy_start_of_sim - 1.0
          else
            snow_doy(i,j) = snow_dos(i,j) - delta_days
          endif
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine linear1(nx,ny,npts,var,s,a)

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
      write (71,57) 1,iyr_start
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

   57 format ('TDEF ',i8,' LINEAR  01JAN',i4,' 1yr')

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
      write (71,57) 1,iyr_start
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

   57 format ('TDEF ',i8,' LINEAR  01JAN',i4,' 1yr')

   58 format ('VARS     ',i4)

  100 format (a,' 0  0  xxxxxxxxxx')

   68 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine calc_domain_ave_stats(nx,ny,nyears,tair_ave,undef,
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
      call linear2(nyears,a_var,s_var,b_var,r2_var)

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

      subroutine linear2(npts,var,s,b,r2)

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
      write (71,55) 1

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

