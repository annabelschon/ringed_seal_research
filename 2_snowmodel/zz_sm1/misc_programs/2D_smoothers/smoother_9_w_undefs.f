c smoother_9_w_undefs.f

      undef = -9999.0

c Smooth the field a little.
      do k=1,3
        call smoother9_w_undefs(nx,ny,undef,topo)
      enddo

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine smoother9_w_undefs(nx,ny,undef,snow)

      implicit none

      integer i,j,nx,ny
      real snow(nx,ny)
      real snow_tmp(nx,ny)
      real undef

c Performs a 9-point smoothing operation.

c If there is an undef value involved, it does not smooth that
c   grid cell.

c The result at each grid point is a weighted average of the grid
c   point and the surrounding 8 points.  The center point receives
c   a weight of 1.0, the points at each side and above and below
c   receive a weight of 0.5, and corner points receive a weight of
c   0.3.  All points are multiplied by their weights and summed,
c   then divided by the total weight.

c Do the interior.
      do i=2,nx-1
        do j=2,ny-1
          if (snow(i,j).ne.undef .and. snow(i,j-1).ne.undef .and.
     &      snow(i,j+1).ne.undef .and. snow(i-1,j).ne.undef .and.
     &      snow(i+1,j).ne.undef .and. snow(i-1,j-1).ne.undef .and.
     &      snow(i+1,j+1).ne.undef .and. snow(i-1,j+1).ne.undef .and.
     &      snow(i+1,j-1).ne.undef) then

            snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) +
     &        snow(i,j+1) + snow(i-1,j) + snow(i+1,j)) + 0.3 *
     &        (snow(i-1,j-1) + snow(i+1,j+1) + snow(i-1,j+1) +
     &        snow(i+1,j-1))) / 4.2
          else
            snow_tmp(i,j) = snow(i,j)
          endif
        enddo
      enddo

c Do the sides.
      j = 1
      do i=2,nx-1
        if (snow(i,j).ne.undef .and. snow(i,j+1).ne.undef .and.
     &    snow(i-1,j).ne.undef .and. snow(i+1,j).ne.undef .and.
     &    snow(i+1,j+1).ne.undef .and. snow(i-1,j+1).ne.undef) then
 
         snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i-1,j) +
     &      snow(i+1,j)) + 0.3 * (snow(i+1,j+1) + snow(i-1,j+1))) / 3.1
        else
          snow_tmp(i,j) = snow(i,j)
        endif
      enddo

      j = ny
      do i=2,nx-1
        if (snow(i,j).ne.undef .and. snow(i,j-1).ne.undef .and.
     &    snow(i-1,j).ne.undef .and. snow(i+1,j).ne.undef .and.
     &    snow(i+1,j-1).ne.undef .and. snow(i-1,j-1).ne.undef) then

         snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i-1,j) +
     &      snow(i+1,j)) + 0.3 * (snow(i+1,j-1) + snow(i-1,j-1))) / 3.1
        else
          snow_tmp(i,j) = snow(i,j)
        endif
      enddo

      i = 1
      do j=2,ny-1
        if (snow(i,j).ne.undef .and. snow(i,j-1).ne.undef .and.
     &    snow(i,j+1).ne.undef .and. snow(i+1,j).ne.undef .and.
     &    snow(i+1,j-1).ne.undef .and. snow(i+1,j+1).ne.undef) then

         snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i,j+1) +
     &      snow(i+1,j)) + 0.3 * (snow(i+1,j-1) + snow(i+1,j+1))) / 3.1
        else
          snow_tmp(i,j) = snow(i,j)
        endif
      enddo

      i = nx
      do j=2,ny-1
        if (snow(i,j).ne.undef .and. snow(i,j-1).ne.undef .and.
     &    snow(i,j+1).ne.undef .and. snow(i-1,j).ne.undef .and.
     &    snow(i-1,j-1).ne.undef .and. snow(i-1,j+1).ne.undef) then

        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i,j+1) +
     &    snow(i-1,j)) + 0.3 * (snow(i-1,j-1) + snow(i-1,j+1))) / 3.1
        else
          snow_tmp(i,j) = snow(i,j)
        endif
      enddo

c Do the corners.
      i = 1
      j = 1
      if (snow(i,j).ne.undef .and. snow(i,j+1).ne.undef .and.
     &  snow(i+1,j).ne.undef .and. snow(i+1,j+1).ne.undef) then

        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i+1,j)) +
     &    0.3 * snow(i+1,j+1)) / 2.3
      else
        snow_tmp(i,j) = snow(i,j)
      endif

      i = nx
      j = 1
      if (snow(i,j).ne.undef .and. snow(i,j+1).ne.undef .and.
     &  snow(i-1,j).ne.undef .and. snow(i-1,j+1).ne.undef) then

        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j+1) + snow(i-1,j)) +
     &    0.3 * snow(i-1,j+1)) / 2.3
      else
        snow_tmp(i,j) = snow(i,j)
      endif

      i = 1
      j = ny
      if (snow(i,j).ne.undef .and. snow(i,j-1).ne.undef .and.
     &  snow(i+1,j).ne.undef .and. snow(i+1,j-1).ne.undef) then

        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i+1,j)) +
     &    0.3 * snow(i+1,j-1)) / 2.3
      else
        snow_tmp(i,j) = snow(i,j)
      endif

      i = nx
      j = ny
      if (snow(i,j).ne.undef .and. snow(i,j-1).ne.undef .and.
     &  snow(i-1,j).ne.undef .and. snow(i-1,j-1).ne.undef) then

        snow_tmp(i,j) = (snow(i,j) + 0.5 * (snow(i,j-1) + snow(i-1,j)) +
     &    0.3 * snow(i-1,j-1)) / 2.3
      else
        snow_tmp(i,j) = snow(i,j)
      endif

c Return the smoothed array.
      do i=1,nx
        do j=1,ny
          snow(i,j) = snow_tmp(i,j)
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

