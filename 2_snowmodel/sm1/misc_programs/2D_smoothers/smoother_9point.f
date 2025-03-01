c smoother_9point.f

      parameter(nx=181,ny=181)

      real topo(nx,ny)

c Initialize the topo array.
      do i=1,nx
        do j=1,ny
          topo(i,j) = 0.0
        enddo
      enddo

c Put a square block in the middle of the domain,
c   15-m by 15-m, by 3-m tall.
      do i=90,92
        do j=90,92
          topo(i,j) = 3.0
        enddo
      enddo

c Smooth the block.
      do k=1,5
        call smoother9(nx,ny,topo)
      enddo

      open (unit=31,file='block_topo_5m.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      write (31,rec=1) ((topo(i,j),i=1,nx),j=1,ny)

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine smoother9(nx,ny,snow)

      implicit none

      integer i,j,nx,ny
      real snow(nx,ny)
      real snow_tmp(nx,ny)

c Performs a 9-point smoothing operation.

c The result at each grid point is a weighted average of the grid
c   point and the surrounding 8 points.  The center point receives
c   a weight of 1.0, the points at each side and above and below
c   receive a weight of 0.5, and corner points receive a weight of
c   0.3.  All points are multiplied by their weights and summed,
c   then divided by the total weight.

c Do the interior.
      do i=2,nx-1
        do j=2,ny-1
          snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) +
     &      snow(i,j+1) + snow(i-1,j) + snow(i+1,j)) + 0.3 *
     &      (snow(i-1,j-1) + snow(i+1,j+1) + snow(i-1,j+1) +
     &      snow(i+1,j-1))) / 4.2
        enddo
      enddo

c Do the sides.
      j = 1
      do i=2,nx-1
        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i-1,j) +
     &    snow(i+1,j)) + 0.3 * (snow(i+1,j+1) + snow(i-1,j+1))) / 3.1
      enddo

      j = ny
      do i=2,nx-1
        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i-1,j) +
     &    snow(i+1,j)) + 0.3 * (snow(i+1,j-1) + snow(i-1,j-1))) / 3.1
      enddo

      i = 1
      do j=2,ny-1
        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i,j+1) +
     &    snow(i+1,j)) + 0.3 * (snow(i+1,j-1) + snow(i+1,j+1))) / 3.1
      enddo

      i = nx
      do j=2,ny-1
        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i,j+1) +
     &    snow(i-1,j)) + 0.3 * (snow(i-1,j-1) + snow(i-1,j+1))) / 3.1
      enddo

c Do the corners.
      i = 1
      j = 1
      snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i+1,j)) +
     &  0.3 * snow(i+1,j+1)) / 2.3

      i = nx
      j = 1
      snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i-1,j)) +
     &  0.3 * snow(i-1,j+1)) / 2.3

      i = 1
      j = ny
      snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i+1,j)) +
     &  0.3 * snow(i+1,j-1)) / 2.3

      i = nx
      j = ny
      snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i-1,j)) +
     &  0.3 * snow(i-1,j-1)) / 2.3

c Return the smoothed array.
      do i=1,nx
        do j=1,ny
          snow(i,j) = snow_tmp(i,j)
        enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

