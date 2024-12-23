c mk_ease_coords.f

c Create the ease coords of the simulation domain.

      parameter (nx=361,ny=361)

      open (21,file='ease_coords.txt')

c Grid increment of ease grid.
      dx = 25.0
      dy = dx

c For a 361x361 ease domain, the following is true:
c   coordinates of center of lower left grid cell of the
c   ease grid  = -180 * 25.0.  This should put 0.0 at
c   the center of the grid.
      xmn = -4500.0
      ymn = -4500.0

c Create a file of the ease grid coords for the simulation domain.
      do j=1,ny
        do i=1,nx
          x = 1000.0 * (xmn + (real(i) - 1.0) * dx)
          y = 1000.0 * (ymn + (real(j) - 1.0) * dy)
          write (21,88) x,y
        enddo
      enddo

   88 format (2f16.1)

      end

