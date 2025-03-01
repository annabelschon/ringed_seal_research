c 2_mk_scaled_temp.f

c Here I am using the cap index and tair and tmch (the air
c   temperature at the height of the moose collar) to define
c   the temperature the moose might be walking through.

c Tamc = CAPS * Tmch + (1 - CAPS) * Tair

c   where amc = air & mch & caps.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

      integer nx,ny,i,j,nt,n

      parameter (nx=2649,ny=2528)
      parameter (nt=731)

      real vege(nx,ny)

      real tair(nx,ny)
      real tmch(nx,ny)
      real tamc(nx,ny)

      real caps(nx,ny)

      real undef

      undef = -9999.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Inputs.
      open (21,file='../../../topo_vege/NoAm_30m/topo_vege.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (21,rec=2) ((vege(i,j),i=1,nx),j=1,ny)

      open (31,file='/data4/moose/oldcrow/process/tair.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (32,file='/data4/moose/oldcrow/process/tmch.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (41,file='../1_mk_cap_index/caps.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (41,rec=1) ((caps(i,j),i=1,nx),j=1,ny)

c Outputs.
      open (61,file='/data4/moose/oldcrow/process/tamc.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      do n=1,nt

        print *, n

        read (31,rec=n) ((tair(i,j),i=1,nx),j=1,ny)
        read (32,rec=n) ((tmch(i,j),i=1,nx),j=1,ny)

        do j=1,ny
          do i=1,nx

c Calculate the moose-environment temperature.
            tamc(i,j) =
     &        caps(i,j) * tmch(i,j) + (1.0 - caps(i,j)) * tair(i,j)

c Mask out the ocean.
            if (vege(i,j).eq.24.0) tamc(i,j) = undef

          enddo
        enddo

        write (61,rec=n) ((tamc(i,j),i=1,nx),j=1,ny)

      enddo

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

