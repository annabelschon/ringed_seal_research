c 1_mk_snod_assim_file.f

c Make a snod assim input file.

      implicit none

      integer nyears,nyear,nmonths,nobsdates,nobs_dates,nmonts,
     &  id1,id2,id3,nstns,n,k,nstns_tmp,nyears_max,nstns_max

      parameter (nyears_max=50)
      parameter (nstns_max=4000)

      parameter (nmonths=12)

      parameter (nobsdates=nyears_max*nmonths)

      double precision xstn(nobsdates),ystn(nobsdates)
      real undef,elev
      real snod(nstns_max,nobsdates)

      integer nyr(nobsdates),nmo(nobsdates),ndy(nobsdates)

      character*4 state
      character*8 id

c Define the path where the original swe assim data are located.
      character path*(*)
      parameter (path=
     &  '../../../../../swe_assim/mk_swed_assim_file/')

      undef = -9999.0

c Read in the number of stations in your SnowModel domain.
      open (71,file=path//'2_extract_space/n_stations.dat')
      read (71,*) nstns

c Read in the number of years in the simulation.
      open (51,file=path//'3_extract_time/nyears_in_simulation.dat')
      read (51,*) nyears
      read (51,*) nmonts
      if (nmonts.ne.0) nyears = nyears + 1

c Read in the station data.
      open (21,file=
     &  path//'2_extract_space/stations_in_your_sm_domain.dat')

      do k=1,nstns
        read (21,92) id1,id2,id3,state,id,xstn(k),ystn(k),elev
      enddo

  92  format (3i6,4x,a4,a8,2f12.1,f10.1)

c Save a snod data assimilation file.
      open (41,file='snod_obs.dat')

c The first line is the number of years in the model run.
      write (41,95) nyears

c Read in the data that will be assimilated.  And write it back
c   out as a snod assim file.
      open (31,file=
     &  '../8_extract_1st_mo_snod_obs/snod_obs_for_assim.dat')

c Process one year at a time.
      do nyear=1,nyears

        read (31,*) nobs_dates

c Write the number of obs dates in this year.
        if (nobs_dates.eq.0) then

          write (41,95) 0

        else

          do n=1,nobs_dates
            read (31,*) nyr(n),nmo(n),ndy(n),(snod(k,n),k=1,nstns)

            if (n.eq.1) then
              write (41,95) nobs_dates
            endif

c Write out the date.
            write (41,96) nyr(n),nmo(n),ndy(n)

c Count the number of stations on this date.
            nstns_tmp = 0
            do k=1,nstns
              if (snod(k,n).ne.undef) then
                nstns_tmp = nstns_tmp + 1
              endif
            enddo

            write (41,97) nstns_tmp

c Station id, x, y, snod.
            do k=1,nstns
              if (snod(k,n).ne.undef) then
                if (snod(k,n).ne.0.0) then
                  write (41,98) k,xstn(k),ystn(k),snod(k,n)
                else
                  print *,'snod probably should not be zero; k=',k
                  stop
                endif
              endif
            enddo

          enddo

        endif

      enddo

  95  format (i8)
  96  format (3i6)
  97  format (i8)
  98  format (i8,2f14.0,f12.3)

      end

