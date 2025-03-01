c mk_1hrly_to_daily_single_station_mm.f

c Read in 1-hourly data and write out daily data.

c This program assumes that you already have an hourly met
c   station file that is in MicroMet format, and all you want
c   to do is convert it from a 1-hourly to daily MicroMet
c   input file.  It also assumes that you have full 24-hour
c   days of data (for example, if your data file has 365 days
c   of data, the file is 24*365 lines long).

      implicit none

      integer ndays,n1hours,k,n,n_1hrs_in_1day,kk,i,idy,imo,iyr

      parameter (ndays=181)
      parameter (n_1hrs_in_1day=24)
      parameter (n1hours=n_1hrs_in_1day*ndays)

      integer iiyr(n1hours)
      integer iimo(n1hours)
      integer iidy(n1hours)
      real xxhr(n1hours)

      real ta(n1hours)
      real rh(n1hours)
      real ws(n1hours)
      real wd(n1hours)
      real pp(n1hours)

      real northing(n1hours)
      real easting(n1hours)
      real elev(n1hours)
      integer id(n1hours)

      real temp,relh,wspd,wdir,prec,uwnd,vwnd
      real undef,pi,degtrad,radtdeg,xhr

      undef = -9999.0

      pi = 2.0 * acos(0.0)
      degtrad = pi / 180.0
      radtdeg = 180.0 / pi

c Input file.
c     open (21,file='met_station_1hrly.dat')
      open (21,file='../micromet_preproc/preproc.dat')

c Output file.
      open (41,file='met_station_daily.dat')

c Read the data.
      do n=1,n1hours
        read (21,*) iiyr(n),iimo(n),iidy(n),xxhr(n),id(n),
     &    easting(n),northing(n),elev(n),
     &    ta(n),rh(n),ws(n),wd(n),pp(n)
      enddo

c Create the 3-hourly data.
      do k=1,ndays

c Initialize the summing arrays.
        temp = 0.0
        relh = 0.0
        wspd = 0.0
        prec = 0.0
        uwnd = 0.0
        vwnd = 0.0

c Summing loop.
        do i=1,n_1hrs_in_1day
          kk = (k-1) * n_1hrs_in_1day + i

          temp = temp + ta(kk)
          relh = relh + rh(kk)
          wspd = wspd + ws(kk)
          prec = prec + pp(kk)

          uwnd = uwnd - sin(wd(kk)*degtrad)
          vwnd = vwnd - cos(wd(kk)*degtrad)
        enddo

c Calculate the daily averages.
        wspd = wspd / real(n_1hrs_in_1day)
        temp = temp / real(n_1hrs_in_1day)
        relh = relh / real(n_1hrs_in_1day)

c Leave the precipitation as the sum of the hourly values.
c       prec = prec

c Deal with the wind direction.  Some compilers don't allow both
c   variables to be zero in the atan2 calculation.
        if (abs(uwnd).lt.1e-10) uwnd = 1e-10
        wdir = radtdeg * atan2(uwnd,vwnd)
        if (wdir.ge.180.0) then
          wdir = wdir - 180.0
        else
          wdir = wdir + 180.0
        endif

c Set the hour card to the middle of the day, following standard
c   MicroMet/SnowModel conventions.  Also define the right yr,
c   mo, and dy.
        xhr = 12.0
        idy = iidy(kk-1)
        imo = iimo(kk-1)
        iyr = iiyr(kk-1)

c Save the data in the MicroMet required format.
        write (41,91) iyr,imo,idy,xhr,
     &    id(kk),easting(kk),northing(kk),elev(kk),
     &    temp,relh,wspd,wdir,prec
      enddo

  91  format (i5,i3,i3,f6.2,i9,2f12.1,f8.1,4f9.2,f10.3)

      end

