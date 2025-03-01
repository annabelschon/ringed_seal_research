c 1_temp_profile_sm.f

c Here we are calculating the air temperature at the moose collar
c   height (Tmch), using the temperature profile between the
c   surface (Tsfc) and 10 meters (Tair).

      implicit none

      integer nx,ny,i,j,nt,n

      parameter (nx=2649,ny=2528)
      parameter (nt=731)

      real vege(nx,ny)

      real tair(nx,ny)
      real tsfc(nx,ny)
      real wspd(nx,ny)

      real tmch(nx,ny)

      real stability,ht_moose,ht_windobs,z0,phi,top,bot,Tf,undef

c Inputs.
      open (21,file='../../../topo_vege/NoAm_30m/topo_vege.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (21,rec=2) ((vege(i,j),i=1,nx),j=1,ny)

      open (31,file='/data4/moose/oldcrow/process/tair.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (32,file='/data4/moose/oldcrow/process/tsfc.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (33,file='/data4/moose/oldcrow/outputs/wo_assim/wspd.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Outputs.
      open (61,file='/data4/moose/oldcrow/process/tmch.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      undef = -9999.0

c Define the moose collar height.
      ht_moose = 1.0

c Define the height of the MERRA2 met forcing.
      ht_windobs = 10.0

c The surface roughness, z0 == snow_z0 from SnowModel.
      z0 = 0.001

c Conversion to K required by the stability function.
      Tf = 273.15

c Loop through the time step.
      do n=1,nt

c Read in the data at this time step.
        read (31,rec=n) ((Tair(i,j),i=1,nx),j=1,ny)
        read (32,rec=n) ((Tsfc(i,j),i=1,nx),j=1,ny)
        read (33,rec=n) ((wspd(i,j),i=1,nx),j=1,ny)

c Process each grid cell at this time.
        do j=1,ny
          do i=1,nx

c Compute the stability function.  These have to be in K.
            CALL STABLEFN(stability,Tair(i,j)+Tf,Tsfc(i,j)+Tf,wspd(i,j),
     &        ht_windobs,z0)

c Convert this stability (0 to 2, or above and below 1.0), to
c   -1 to 1.  stability now == z/L.
            stability = - (stability - 1.0)

c Don't let the stability be greater than 1.0 or less than -1.0.
            stability = min(1.0,stability)
            stability = max(-1.0,stability)

c Unstable.
            if (stability.lt.0.0) then
              phi = (1.0 - 16.0 * stability)**(-0.5)
c Stable.
            elseif (stability.gt.0.0) then
              phi = 1.0 + 5.0 * stability
c Neutral.
            else
              phi = 1.0
            endif

c In Fortran, log == ln.
            top = log(ht_moose / z0) - phi
            bot = log(ht_windobs / z0) - phi

c Don't let top be negative, or you will get temperatures above
c   the surface that are lower than the surface.
            top = max(0.0,top)

c Calculate the temperature at the moose collar height (Tmch).
            Tmch(i,j) = Tsfc(i,j) + (Tair(i,j)-Tsfc(i,j)) * (top / bot)

c Mask out the ocean.
            if (vege(i,j).eq.24.0) Tmch(i,j) = undef

          enddo
        enddo

        write (61,rec=n) ((Tmch(i,j),i=1,nx),j=1,ny)

      enddo

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE STABLEFN(stability,Tair,Tsfc,windspd,
     &  ht_windobs,z0)

      implicit none

      real C1,C2,B1,B2,stability,Tair,Tsfc,windspd,ht_windobs,
     &  gravity,xkappa,z0,B8,B3,z0_tmp

      gravity = 9.81
      xkappa = 0.4

      z0_tmp = min(0.25*ht_windobs,z0)
      C1 = 5.3 * 9.4 * (xkappa/(log(ht_windobs/z0_tmp)))**2 *
     &  sqrt(ht_windobs/z0_tmp)
      C2 = gravity * ht_windobs / (Tair * windspd**2)
      B1 = 9.4 * C2
      B2 = C1 * sqrt(C2)

      if (Tsfc.gt.Tair) then
c Unstable case.
        B3 = 1.0 + B2 * sqrt(Tsfc - Tair)
        stability = 1.0 + B1 * (Tsfc - Tair) / B3
      elseif (Tsfc.lt.Tair) then
c Stable case.
        B8 = B1 / 2.0
        stability = 1.0 / ((1.0 + B8 * (Tair - Tsfc))**2)
      else
c Neutrally stable case.
        stability = 1.0
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

