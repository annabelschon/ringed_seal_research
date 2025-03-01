c 3_mk_r_data.f

      implicit none

      integer nx,ny,i,j

      parameter (nx=2649,ny=2528)

      real topo(nx,ny)

      real undef,x,y,xmn,ymn,dx,dy

      undef = -9999.0

      open (21,file='../../../../../topo_vege/NoAm_30m/topo_vege.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      read (21,rec=1) ((topo(i,j),i=1,nx),j=1,ny)

c Write out the info r needs: x y snod.
      open (31,file='coords_topo.dat')
      open (32,file='../2_r_calcs/r_in_out/coords_topo.dat')

      dx = 100.0
      dy = 100.0

      xmn = 50.0
      ymn = 50.0

      do j=1,ny
        do i=1,nx
          x = xmn + (real(i) - 1.0) * dx
          y = ymn + (real(j) - 1.0) * dy

c Convert to km.
          x = x / 1000.0
          y = y / 1000.0

c Here I am writing out just the line I am interested in.
          if (i.eq.750) then
            write (31,88) x,y,topo(i,j)
            write (32,88) x,y,topo(i,j)
          endif

        enddo
      enddo

   88 format (2f16.5,f16.4)

      end

