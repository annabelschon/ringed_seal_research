c 1_spherical.f

      parameter (nx=20001)

      real sph(nx)
      real y_plot(nx)
      real sill,y_nugget,x_lag

      open (21,file='../3_mk_grads/semivario_stats.dat')

      open (31,file='../3_mk_grads/semivario.gdat',
     &  form='unformatted',access='direct',recl=4*nx)

      open (41,file='semivario_spherical_model.gdat',
     &  form='unformatted',access='direct',recl=4*nx)

      read (21,*)
      read (21,*) sill,y_nugget,x_lag

      read (31,rec=1) (y_plot(i),i=1,nx)

c In these 'r' outputs, what is called the 'sill' does not
c   include the nugget.  Add that back in here.
      sill = sill + y_nugget

      do k=1,nx

        x = real(k-1) / 1000.0

        if (x.eq.0.0) then
          sph(k) = y_nugget
        elseif (x.gt.x_lag) then
          sph(k) = sill
        else
          sph(k) = y_nugget + (sill - y_nugget) *
     &      (1.5 * (x/x_lag) - 0.5 * (x/x_lag)**3)
        endif

      enddo

      write (41,rec=1) (y_plot(k),k=1,nx)
      write (41,rec=2) (sph(k),k=1,nx)

      end

