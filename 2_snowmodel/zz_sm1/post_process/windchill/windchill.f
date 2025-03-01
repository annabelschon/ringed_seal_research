c windchill.f

c The equation solved here is from:

c   Osczevski, R., and M. Bluestein, 2005: The new Wind Chill
c   Equivalent Temperature chart. Bull. Amer. Meteor. Soc., 86,
c   1453-1458. 

c The equation parameters have been converted from wind speed
c   units of km/hr to m/s, consistent with MicroMet and SnowModel
c   outputs.

      implicit none

      integer k,maxiter  
      integer i,nx,j,ny

c maxiter is the number of time steps that were printed out
c   (the model was run at a 3-hr time step, but the data were
c   written out at a daily time step).
      parameter (maxiter=731)
      parameter (nx=4056, ny=3277)

      real wspd(nx,ny)
      real tair(nx,ny)
      real chil(nx,ny) 

      real undef

      undef = -9999.0

c Input files.
      open (21,file=
     & '/data4/adele/Powell_UW_grouse/outputs/wo_assim/tair.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (31,file=
     & '/data4/adele/Powell_UW_grouse/outputs/wo_assim/wspd.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Output file.
      open (41,file=
     & 'windchil.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k=1,maxiter

        read (21,rec=k) ((tair(i,j),i=1,nx),j=1,ny)
        read (31,rec=k) ((wspd(i,j),i=1,nx),j=1,ny)

        do j=1,ny
          do i=1,nx
            if (wspd(i,j).ne.undef .and. tair(i,j).ne.undef) then
c m/s.
              chil(i,j) = 13.12 + 0.6215 * tair(i,j) -
     &          13.95 * wspd(i,j)**0.16 +
     &          0.4867 * tair(i,j) * wspd(i,j)**0.16
c km/hr.
c             chil(i,j) = 13.12 + 0.6215 * tair(i,j) -
c    &          11.37 * wspd(i,j)**0.16 +
c    &          0.3965 * tair(i,j) * wspd(i,j)**0.16
            else
              chil(i,j) = undef
            endif
          enddo
        enddo

        write (41,rec=k) ((chil(i,j),i=1,nx),j=1,ny)

      enddo

      end

