c 1_mk_daily_from_3hrly_gdat.f

c Read in 3-hourly .gdat from SnowModel outputs, write out daily
c   .gdat files data.

c You can have 3 kinds of variables here:
c   1) variables that are averaged over the day (e.g., tair)
c   2) variables that are summed over the day (e.g., spre)
c   3) variables extracted at the end of the day (e.g., snod)

c Averaging the wind direction is a special case that I have not
c   addressed here.  There are other places where you can find
c   examples of how to do that (like in my MERRA2 archive update
c   processing codes).

c Also, averaging things like sden that "sometimes" have undefs
c   in them will also need something special done to make the
c   values turn out correct.

c NOTE: So that the new (daily) outputs fit all of the other
c   things going on with SnowModel outputs (.ctl files, plotting
c   scripts, conversion to yearly values, etc.), the input and
c   output files here have the same names; what is different is
c   the path that defines where they are located.  This is
c   critical to understand; otherwise you will write over your
c   3-hourly SnowModel outputs with the daily data!

      implicit none

      integer nx,ny,nvars

      parameter (nx=60,ny=41)

c This is the number of variables or SnowModel files you are
c   going to process.
      parameter (nvars=6)

c Examples:

c Averages.
c     real tair(nx,ny)
c     real relh(nx,ny)
c     real wspd(nx,ny)

c Sums.
c     real prec(nx,ny)
c     real spre(nx,ny)
c     real rpre(nx,ny)

c End of day values.
c     real swed(nx,ny)
c     real snod(nx,ny)
c     real sden(nx,ny)

      integer nvar,maxiter_daily,iyr_start,imo_start,idy_start

      character path1*(*) 
      character path2*(*) 
      character path3*(*) 

c Input path.
      parameter (path1 =
     &  '/data1/working/snowmodel/development/3hrly_to_daily/'//
     &  '3hrly/outputs/wo_assim/')

c Output path.
      parameter (path2 =
     &  '/data1/working/snowmodel/development/3hrly_to_daily/'//
     &  '3hrly/outputs/wo_assim_daily/')

c Path to maxiter information.
      parameter (path3 =
     &  '../../met/nldas2/4_maxiter_offset/')

c These two arrays is how you define what variables (SnowModel
c   outputs) will be processed, and how they will be processed.
      character*30 cvar_name(nvars)
      integer ivar_flag(nvars)

c After defining your variables, you must pick how they will be
c   processed.  These are the options (you must pick one for each
c   variable):
c   ivar_flag = 1 = averaged over the day (e.g., tair).
c   ivar_flag = 2 = summed over the day (e.g., spre).
c   ivar_flag = 3 = extracted at the end of the day (e.g., snod).

      data cvar_name /
     &  'tair',
     &  'rainfall_precip',
     &  'snow_precipitation',
     &  'swed',
     &  'snod',
     &  'sden'/

      data ivar_flag /
     &  1,
     &  2,
     &  2,
     &  3,
     &  3,
     &  3/

c Read in the daily maxiter value for this model run.
      open (31,file=path3//'start_end_dates_maxiter_ioffset.dat')
      read (31,99) iyr_start
      read (31,99) imo_start
      read (31,99) idy_start
      read (31,*)
      read (31,*)
      read (31,*)
      read (31,*)
      read (31,*)
      read (31,*)
      read (31,*)
      read (31,*)
      read (31,99) maxiter_daily
c     print *, maxiter_daily
   99 format (14x,i10)

c Loop through the variables.
      do nvar=1,nvars

c Create daily data from 3-hourly data.
        call mk_daily_from_3hrly (nx,ny,path1,path2,maxiter_daily,
     &    trim(cvar_name(nvar)),ivar_flag(nvar))

c Create the associated .ctl file.
        call make_daily_ctl_file (nx,ny,maxiter_daily,iyr_start,
     &    imo_start,idy_start,trim(cvar_name(nvar)),path2)

      enddo

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_daily_from_3hrly (nx,ny,path1,path2,maxiter_daily,
     &  cvar_name,ivar_flag)

      implicit none

      integer nx,ny

      real data(nx,ny)
      real data_daily(nx,ny)

      character*(*) path1
      character*(*) path2

      character*(*) cvar_name
      integer ivar_flag

      integer i,j,n_3hrs_in_day,k,kk,kkk,maxiter_daily
      real undef

c Required constants.
      n_3hrs_in_day = 8
      undef = -9999.0

c Open the input file.
      open (unit=21,file=path1//cvar_name//'.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Open the output file.
      open (unit=41,file=path2//cvar_name//'.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

c Create daily data from 3-hourly data.
      print *
      do k=1,maxiter_daily

c       print *,k

c Extract the end of day value (irec = 8, 16, 24, etc.).
        if (ivar_flag.eq.3) then

          kkk = (k - 1) * n_3hrs_in_day + n_3hrs_in_day
c         print *, k,kk,kkk
          read (21,rec=kkk) ((data_daily(i,j),i=1,nx),j=1,ny)

        else

c Initialize the summing arrays.
          do j=1,ny
            do i=1,nx
              data_daily(i,j) = 0.0
            enddo
          enddo

c Summing loop.
          do kk=1,n_3hrs_in_day
            kkk = (k - 1) * n_3hrs_in_day + kk
c           print *, k,kk,kkk
            read (21,rec=kkk) ((data(i,j),i=1,nx),j=1,ny)
            do j=1,ny
              do i=1,nx
                data_daily(i,j) = data_daily(i,j) + data(i,j)
              enddo
            enddo
          enddo

c Calculate the daily average.
          if (ivar_flag.eq.1) then

            do j=1,ny
              do i=1,nx
                data_daily(i,j) = data_daily(i,j) / real(n_3hrs_in_day)
              enddo
            enddo

c Leave it as a sum.  But correct the problem if you are in an
c   undef area, like over oceans (so the value now is 8*undef).
          elseif (ivar_flag.eq.2) then

            do j=1,ny
              do i=1,nx
                if (data(i,j).eq.undef) then
                  data_daily(i,j) = undef
c               else
c                 data_daily(i,j) = data_daily(i,j)
                endif
              enddo
            enddo

          endif

        endif

c Save the data.
        if (mod(k,100).eq.0)
     &    print *,'saving daily record      ',cvar_name,k

        write (41,rec=k) ((data_daily(i,j),i=1,nx),j=1,ny)

      enddo

      print *

      close (21)
      close (41)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_daily_ctl_file (nx,ny,ndays,iyr_start,
     &  imo_start,idy_start,var_string,path)

      implicit none

      integer ndays,iyr_start,imo_start,idy_start,nx,ny

      character*3 cmo(12)
      data cmo /'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/
 
      character*(*) path
      character*(*) var_string

      open (71,file=var_string//'_daily.ctl')

      write (71,51) path,var_string
      write (71,52)
      write (71,53)
      write (71,54) nx
      write (71,55) ny

      write (71,56)
      write (71,57) ndays,idy_start,cmo(imo_start),iyr_start
      write (71,58) 1

      write (71,101) var_string

      write (71,68)

      close (71)

   51 format ('DSET ',a,a,'.gdat')
   52 format ('TITLE daily SnowModel data')
   53 format ('UNDEF -9999.0')

   54 format ('XDEF ',i8,' LINEAR  1.0  1.0')
   55 format ('YDEF ',i8,' LINEAR  1.0  1.0')
   56 format ('ZDEF        1 LINEAR  1.0  1.0')

   57 format ('TDEF ',i8,' LINEAR  ',i2,a3,i4,' 1dy')

   58 format ('VARS     ',i4)

  101 format (a,' 0  0  xxxxxxxxxxxxxx')

   68 format ('ENDVARS')

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

