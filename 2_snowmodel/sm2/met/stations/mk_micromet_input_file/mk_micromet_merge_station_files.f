c mk_micromet_merge_station_files.f

c This reads in two station datasets that are already in standard
c   MicroMet format, and writes them out in a merged MicroMet
c   input file ready for use by SnowModel.

      implicit none

      integer n_days,n_records,n_stations,n_station,iter

      parameter (n_days=319)
      parameter (n_stations=2)

      parameter (n_records=n_days*8)

      real tair,relh,wspd,wdir,prec
      real x_loc,y_loc,elev

      integer iyr,imo,idy,id
      real xhr

c Input files.
      open (21,file='met_station_408_3hrly_w-missing.dat')
      open (22,file='merra2_single_point_3hrly_w-missing.dat')

c Define the name of the MicroMet output file.
      open (31,file='mm_3hrly_2002-2003.dat')

c Run the data extraction and write procedure.  Loop through all of
c   the data, writing a station count before writing the data for
c   the stations.

      do iter=1,n_records

        write (31,980) n_stations

        do n_station=1,n_stations
          read (20+n_station,*) iyr,imo,idy,xhr,id,x_loc,y_loc,elev,
     &      tair,relh,wspd,wdir,prec
          write (31,990) iyr,imo,idy,xhr,id,x_loc,y_loc,elev,
     &      tair,relh,wspd,wdir,prec
        enddo

      enddo

  980 format (i6)
  990 format (i5,i3,i3,f6.2,i9,2f12.1,f8.1,4f9.2,f10.3)

      end

