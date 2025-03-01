c 1_mk_grads.f

      implicit none

      integer nx
      parameter (nx=13)

      real x_length_km
      integer nvalues_1km
      parameter (x_length_km=1000.0*20.0)

c Calculate the number of 1-km values along the x axis.
c   nvalues_1km = 2001.
      parameter (nvalues_1km=nint(1000.0*x_length_km)+1)

      real xx(nx)
      real yy(nx)

      real y_plot(nvalues_1km)

      real sill
      real y_nugget
      real x_lag

      integer i,kk
      real undef

      character*80 fname_sph
      character*80 fname_var

      data fname_sph /
     &  '../2_r_calcs/r_in_out/coords_topo.dat.sph'/

      data fname_var /
     &  '../2_r_calcs/r_in_out/coords_topo.dat.var'/

      undef = -9999.0

      open (41,file='semivario.gdat',
     &  form='unformatted',access='direct',recl=4*nvalues_1km,
     &  status='replace')

      open (62,file='semivario_stats.dat')

c Read in the data.
      open (21,file=fname_sph)
      open (23,file=fname_var)

      read (21,*)
      read (21,*)
      read (21,*) sill
      read (21,*)
      read (21,*)
      read (21,*) y_nugget
      read (21,*)
      read (21,*) x_lag

      read (23,*) (xx(i),i=1,nx)
      read (23,*) (yy(i),i=1,nx)

      write (62,*) '     sill    nugget       lag'
      write (62,87) sill+y_nugget,y_nugget,x_lag
   87 format (3f10.3)

c Place a valid y value in the appropriate x position.
c   Initialize with undefs.
      do i=1,nvalues_1km
        y_plot(i) = undef
      enddo

      do i=1,nx
        kk = nint(1000.0*xx(i))
        y_plot(kk) = yy(i)
      enddo

c Save the data as a grads file.
      write (41,rec=1) (y_plot(i),i=1,nvalues_1km)

      end

