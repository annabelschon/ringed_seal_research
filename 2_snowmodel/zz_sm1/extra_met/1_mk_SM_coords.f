c 1_mk_SM_coords.f

c Create the SM grid coords over the simulation domain.

      parameter (nx=304,ny=448)

      double precision dx,dy,xmn,ymn,x,y

      open (21,file='proj_coords.txt')

      dx = 25000.0D0
      dy = dx
      xmn = -3837500.00D0
      ymn = -5337500.00D0

      do j=1,ny
        do i=1,nx
          x = xmn + (real(i) - 1.0) * dx
          y = ymn + (real(j) - 1.0) * dy
          write (21,88) x,y
        enddo
      enddo

   88 format (2f18.1)

      end

