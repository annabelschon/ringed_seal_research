c get_nearest_fastest_possible.f

c Find the i,j locations of the one MERRA2 grid point closest
c  to each of the EASE grid cell positions.

c nnx, nny defines the MERRA2 grid.
c nx, ny defines the SnowModel grid.

      implicit none

      integer nx,ny,nnx,nny

      parameter (nx=361,ny=361,nnx=576,nny=161)

      real del_lon_merra,del_lat_merra,xmerra_lon_ll,ymerra_lat_ll

      integer i,j,ii,jj

      real sm_lon(nx,ny)
      real sm_lat(nx,ny)

      real x_ipos(nx,ny)
      real y_jpos(nx,ny)

c Provide some lat lon info about the MERRA2 grid; the lat-lon
c   grid increments, and the coords of the lower-left corner.
      del_lon_merra = 5.0/8.0
      del_lat_merra = 0.5
      xmerra_lon_ll = -180.0
      ymerra_lat_ll = 10.0

c Read in the lat-lon coordinates of the SnowModel grid.
      open (21,file='../1_sm_ease_coords/2_ease_to_ll/ll_coords.txt')
      do j=1,ny
        do i=1,nx
          read (21,*) sm_lon(i,j),sm_lat(i,j)
        enddo
      enddo

c Search for the MERRA grid points that are nearest to the
c   SnowModel grid point of interest.
      do j=1,ny
        do i=1,nx

c Because I have MERRA grid-cell data on a lat-lon grid, I can
c   get the nearest grid point very efficiently as follows.
          ii = nint((sm_lon(i,j) - xmerra_lon_ll)/del_lon_merra + 1.0)
          jj = nint((sm_lat(i,j) - ymerra_lat_ll)/del_lat_merra + 1.0)
 
c The above will produce ii = 577 along the vertical line of grid
c   cells in the center of the northern half of the domain.
          if (ii.eq.nnx+1) ii = 1

          x_ipos(i,j) = real(ii)
          y_jpos(i,j) = real(jj)

        enddo
      enddo

      open (unit=51,file='nearest_ij.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      write(51,rec=1) ((x_ipos(i,j),i=1,nx),j=1,ny)
      write(51,rec=2) ((y_jpos(i,j),i=1,nx),j=1,ny)

      end

