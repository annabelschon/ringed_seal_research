c mk_1hrly_to_3hrly_single_station_mm.f

c Read in 1-hourly data and write out 3-hourly data.

c This program assumes that you already have an hourly met
c   station file that is in MicroMet format, and all you want
c   to do is convert it from a 1-hourly to 3-hourly MicroMet
c   input file.  It also assumes that you have full 24-hour
c   days of data (for example, if your data file has 365 days
c   of data, the file is 24*365 lines long).

      implicit none

      integer ndays,n1hours,n3hours,k,n,n_1hrs_in_3hrs,kk,i

      parameter (ndays=319)
      parameter (n1hours=24*ndays)
      parameter (n3hours=8*ndays)

      integer iyr(n1hours)
      integer imo(n1hours)
      integer idy(n1hours)
      real xhr(n1hours)

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
      real undef,pi,degtrad,radtdeg

      undef = -9999.0

      pi = 2.0 * acos(0.0)
      degtrad = pi / 180.0
      radtdeg = 180.0 / pi

      n_1hrs_in_3hrs = 3

c Input file.
      open (21,file='met_station_408_1hrly.dat')

c Output file.
      open (41,file='met_station_408_3hrly.dat')

c Read the data.
      do n=1,n1hours
        read (21,*) iyr(n),imo(n),idy(n),xhr(n),id(n),
     &    easting(n),northing(n),elev(n),
     &    ta(n),rh(n),ws(n),wd(n),pp(n)
      enddo

c Create the 3-hourly data.
      do k=1,n3hours

c Initialize the summing arrays.
        temp = 0.0
        relh = 0.0
        wspd = 0.0
        prec = 0.0
        uwnd = 0.0
        vwnd = 0.0

c Summing loop.
        do i=1,n_1hrs_in_3hrs
          kk = (k-1) * n_1hrs_in_3hrs + i

          temp = temp + ta(kk)
          relh = relh + rh(kk)
          wspd = wspd + ws(kk)
          prec = prec + pp(kk)

          uwnd = uwnd - sin(wd(kk)*degtrad)
          vwnd = vwnd - cos(wd(kk)*degtrad)
        enddo

c Calculate the daily averages.
        wspd = wspd / real(n_1hrs_in_3hrs)
        temp = temp / real(n_1hrs_in_3hrs)
        relh = relh / real(n_1hrs_in_3hrs)

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

c Save the data in the MicroMet required format.
        write (41,91) iyr(kk),imo(kk),idy(kk),xhr(kk),
     &    id(kk),easting(kk),northing(kk),elev(kk),
     &    temp,relh,wspd,wdir,prec
      enddo

  91  format (i5,i3,i3,f6.2,i9,2f12.1,f8.1,4f9.2,f10.3)

      end

