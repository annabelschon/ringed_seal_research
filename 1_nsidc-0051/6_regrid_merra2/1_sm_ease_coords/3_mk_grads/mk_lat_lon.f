c mk_lat_lon.f

c This is the EASE grid.
      parameter (nx=361,ny=361)

      real xlon(nx,ny)
      real xlat(nx,ny)

c Open the output files.
      open (unit=51,file='grid_lon.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (unit=52,file='grid_lat.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Read in the lat-lon data for the EASE grid.
      open (31,file='../2_ease_to_ll/ll_coords.txt')
      do j=1,ny
        do i=1,nx
          read (31,*) xlon(i,j),xlat(i,j),dummy
        enddo
      enddo

      write (51,rec=1) ((xlon(i,j),i=1,nx),j=1,ny)
      write (52,rec=1) ((xlat(i,j),i=1,nx),j=1,ny)

      end

