c 1_mk_domain_coords_info.f

      implicit none

      integer nx,ny
      real dx,dy,xmn,ymn
      real xmin,ymin,xmax,ymax

      open (21,file='/home/aschoen/seals/2_snowmodel/sm2/topo_vege/'//
     &  'pan_arctic/SM_domain_config_info_OUTPUTS.dat')

      read (21,91) nx
      read (21,91) ny
      read (21,92) dx
      read (21,92) dy
      read (21,93) xmn
      read (21,93) ymn

  91  format (9x,i8)
  92  format (9x,f8.1)
  93  format (9x,f11.2)

      print *, nx
      print *, ny
      print *, dx
      print *, dy
      print *, xmn
      print *, ymn

c Use this information to create the cropping coordinates that
c   correspond to the sea ice projection and domain.  These are
c   the corners of the lower left and upper right grid cells
c   of the domain.
      xmin = xmn - dx / 2.0
      ymin = ymn - dy / 2.0

      xmax = xmin + dx * real(nx)
      ymax = ymin + dy * real(ny)

c Write these data to files to be read by the gdal script.
      open (unit=31,file='dx.dat')
      open (unit=32,file='dy.dat')

      write (31,94) dx
      write (32,94) dy

      open (41,file='cropping_coords.dat')

      write (41,95) xmin,ymin,xmax,ymax

  94  format (f8.2)
  95  format (4f12.2)

      end

