c 1_mk_ll_only.f

c Extract the lat-lon data so gdal can convert it to the SnowModel
c   projection.

      implicit none

      integer id,iyr,imo,idy,max_lines,k
      real x,y

      parameter (max_lines=1000000)

      open (21,file='../1_orig_gps_data/AMRO_GPS_2018_orig.dat')
      open (31,file='ll_coords.dat')

c Read past the header line.
      read (21,*)

c Read the rest of the lines.
      do k=1,max_lines
        read (21,*,end=99) id,iyr,imo,idy,x,y
        write (31,91) x,y
c       print *, k
      enddo
   99 continue

   91 format (2f14.6)

      end

