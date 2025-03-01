c 2_mk_caps.f

c This is creating my version of the Lundquist et al. (2008)
c   cold-air pooling (CAP) model for complex terrain.  CAPS
c   means I have introduced a smoothing function to CAP.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

      integer nx,ny,i,j,ijdelta,ijdelta_sm

      parameter (nx=2649,ny=2528)

      real deltax,deltay,curve_len_scale,xy_dim,dxdy,wavelength,
     &  slope_threshold,curvature_threshold,frac_lower_threshold

      real topo(nx,ny)
      real vege(nx,ny)
      real curvature(nx,ny)
      real terrain_slope(nx,ny)
      real frac(nx,ny)
c     real percentile(nx,ny)
      real cap(nx,ny)
      real cap_smth(nx,ny)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Grid increment.
      deltax = 100.0
      deltay = 100.0

c Wavelength and curvature.  This came from my semivariogram
c   analysis.  The curvature is equal to half the distance from
c   peak to peak, with the valley between, or half the wavelength
c   of the topographic features of interest.  The semivariograph
c   analysis produced a range of 11500 m, or a wavelength of
c   23000.  The range equals the curvature length scale.
      wavelength = 23000.0

      curve_len_scale = 0.5 * wavelength

c Topography and land cover data.
      open (21,file='../../../topo_vege/NoAm_30m/topo_vege.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (21,rec=1) ((topo(i,j),i=1,nx),j=1,ny)
      read (21,rec=2) ((vege(i,j),i=1,nx),j=1,ny)

c Calculate slope and large-scale curvature.
      call topo_data(nx,ny,deltax,deltay,topo,
     &  curvature,terrain_slope,curve_len_scale)

c Calculate the fraction of grid cells that are lower than
c   each grid cell, over a distance xy_dim from that point.
c   This is just the first loop of Gallant and Dowling (2003).

c The number of grid cells I am comparing in each direction.
c   dxdy = the grid increment (m), 2*xy_dim = the distance you
c   want to average over (m), e.g., 10,000m = a 10-km square
c   centered on the grid point of interest.  xy_dim is defined
c   to equal the curvature length scale; this assumption
c   reproduced the Lundquist et al. (2008) CAP distributions.
c   Then ijdelta is half of this xy_dim, in grid cell units,
c   so it is like the radius of the xy box.
      dxdy = (deltax + deltay) / 2.0
      xy_dim = curve_len_scale
      ijdelta = nint(xy_dim / dxdy / 2.0)
      print *, 'ijdelta =',ijdelta

      do j=1,ny
        do i=1,nx
          call fraction_lower (nx,ny,i,j,ijdelta,topo,frac(i,j))
        enddo
      enddo

c Percentile calculation.  I don't understand this, and am not
c   using it in any way.  It is not required for me to reproduce
c   Jessica's 2008 paper figures.
c     do j=1,ny
c       do i=1,nx
c         percentile(i,j) = - 1.0/60.0 * terrain_slope(i,j) + 0.5
c       enddo
c     enddo

c CAP calculation.  0.0 = no CAP, 1.0 = CAP possible, and 2.0
c   = CAP likely.  These values give a CAP index of 0, 1, and 2.
c   The slope_threshold and curvature_threshold come from Lundquist
c   et al. (2008).  A frac_lower_threshold of 0.3 was found to
c   reproduce the figures in Lundquist et al. (2008).
      slope_threshold = 30.0
      curvature_threshold = 0.0
c     frac_lower_threshold = 0.3
c     frac_lower_threshold = 0.2
c     frac_lower_threshold = 0.1
      frac_lower_threshold = 0.05

      do j=1,ny
        do i=1,nx

c These are the possible CAP areas.
          cap(i,j) = 1.0

c These are the areas where there is no CAP.
          if (terrain_slope(i,j).ge.slope_threshold) cap(i,j) = 0.0
          if (curvature(i,j).gt.curvature_threshold) cap(i,j) = 0.0

c These are the areas where there is potential/likely CAP.
          if (frac(i,j).le.frac_lower_threshold) cap(i,j) = 2.0

c Also assume there will be likely CAP when the slopes are very
c   low and the curvature is negative or only slightly above zero.
          if (terrain_slope(i,j).le.1.0 .and. curvature(i,j).le.0.01)
     &      cap(i,j) = 2.0

        enddo
      enddo

c Run a smoother over the cap distribution to create a smoothly
c   varying distibution.  First convert from the 0, 1, 2 indices
c   to CAP values that range from 0-1.
      do j=1,ny
        do i=1,nx
          cap_smth(i,j) = 0.5 * cap(i,j)
        enddo
      enddo

c It is not clear to me how best to define this.
      ijdelta_sm = nint(real(ijdelta) / 8.0)
      print *, 'ijdelta_sm =',ijdelta_sm
      call smoother_n (nx,ny,ijdelta_sm,cap_smth)

c Clean up values that are out of range.
      do j=1,ny
        do i=1,nx
          cap_smth(i,j) = max(0.0,cap_smth(i,j))
          cap_smth(i,j) = min(1.0,cap_smth(i,j))
        enddo
      enddo

c Set the ocean points to 0.0 instead of 1.0
      do j=1,ny
        do i=1,nx
          if (vege(i,j).eq.24.0) cap_smth(i,j) = 0.0
        enddo
      enddo

c Save the data.
      open (31,file='cap_info.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')
      write (31,rec=1) ((topo(i,j),i=1,nx),j=1,ny)
      write (31,rec=2) ((terrain_slope(i,j),i=1,nx),j=1,ny)
      write (31,rec=3) ((curvature(i,j),i=1,nx),j=1,ny)
      write (31,rec=4) ((frac(i,j),i=1,nx),j=1,ny)
      write (31,rec=5) ((cap(i,j),i=1,nx),j=1,ny)
      write (31,rec=6) ((cap_smth(i,j),i=1,nx),j=1,ny)

      open (32,file='caps.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')
      write (32,rec=1) ((cap_smth(i,j),i=1,nx),j=1,ny)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine topo_data(nx,ny,deltax,deltay,topo,
     &  curvature,terrain_slope,curve_len_scale)

      implicit none

      integer i,j,nx,ny,inc

      real pi,rad2deg,deltax,deltay,deltaxy,curve_len_scale,curve_max

      real topo(nx,ny)
      real dzdx(nx,ny)
      real dzdy(nx,ny)
      real curvature(nx,ny)
      real terrain_slope(nx,ny)

c Compute the topographic information required to run the wind
c   model.

c Deal with the model running at a point, or along single or double
c   lines.
      if (nx.le.2  .or.  ny.le.2) then
        do i=1,nx
          do j=1,ny
            curvature(i,j) = 0.0
            terrain_slope(i,j) = 0.0
          enddo
        enddo
      else

c Define the required constants.
        pi = 2.0 * acos(0.0)
        rad2deg = 180.0 / pi

c CURVATURE CALCULATIONS.

c Compute the average grid increment.
        deltaxy = 0.5 * (deltax + deltay)

c Convert the length scale to an appropriate grid increment.
        inc = max(1,nint(curve_len_scale/deltaxy))

c Compute the curvature.
        do i=1,nx
          do j=1,ny
            curvature(i,j) = (4.0 * topo(i,j) -
     &        topo(max(1,i-inc),max(1,j-inc)) -
     &        topo(min(nx,i+inc),min(ny,j+inc)) -
     &        topo(min(nx,i+inc),max(1,j-inc)) -
     &        topo(max(1,i-inc),min(ny,j+inc))) /
     &        (sqrt(2.0) * 16.0 * real(inc) * deltaxy) +
     &        (4.0 * topo(i,j) -
     &        topo(min(nx,i+inc),j) - topo(max(1,i-inc),j) -
     &        topo(i,min(ny,j+inc)) - topo(i,max(1,j-inc))) /
     &        (16.0 * real(inc) * deltaxy)
          enddo
        enddo

c Scale the curvature such that the max abs(curvature) has a value
c   of abs(0.5).  Include a 1 mm curvature in curve_max to prevent
c   divisions by zero in flat terrain where the curvature is zero.
        curve_max = 0.0 + 0.001
        do j=1,ny
          do i=1,nx
            curve_max = max(curve_max,abs(curvature(i,j)))
          enddo
        enddo
        do j=1,ny
          do i=1,nx
            curvature(i,j) = curvature(i,j) / (2.0 * curve_max)
          enddo
        enddo

c SLOPE CALCULATIONS.

c Find dzdx.
        do j=1,ny
          dzdx(1,j) = (topo(2,j) - topo(1,j)) / deltax
          do i=2,nx-1
            dzdx(i,j) = (topo(i+1,j) - topo(i-1,j)) / (2.0 * deltax)
          enddo
          dzdx(nx,j) = (topo(nx,j) - topo(nx-1,j)) / deltax
        enddo

c Find dzdy.
        do i=1,nx
          dzdy(i,1) = (topo(i,2) - topo(i,1)) / deltay
          do j=2,ny-1
            dzdy(i,j) = (topo(i,j+1) - topo(i,j-1)) / (2.0 * deltay)
          enddo
          dzdy(i,ny) = (topo(i,ny) - topo(i,ny-1)) / deltay
        enddo

c Calculate the terrain slope.
        do i=1,nx
          do j=1,ny

c Some compilers will not allow dzdx and dzdy to both be 0.0 in
c   the atan2 computation.
            if (abs(dzdx(i,j)).lt.1e-10) dzdx(i,j) = 1e-10
            if (abs(dzdy(i,j)).lt.1e-10) dzdy(i,j) = 1e-10

c Compute the slope of the terrain.
            terrain_slope(i,j) = rad2deg *
     &        atan(sqrt(dzdx(i,j)*dzdx(i,j) + dzdy(i,j)*dzdy(i,j)))

          enddo
        enddo

      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine fraction_lower (nx,ny,ii,jj,ijdelta,topo,frac)

      implicit none

      integer nx,ny,ijdelta,i,j,ii,jj,jsrt,jend,isrt,iend,npts

      real topo(nx,ny)
      real count,frac

c Sweep through the data, one '2*ijdelta x 2*ijdelta' block at a
c   time, averaging the values over that data block.
      isrt = ii - ijdelta
      iend = ii + ijdelta

      jsrt = jj - ijdelta
      jend = jj + ijdelta

c Make sure you don't go outside of the simulation domain.
      if (isrt.lt.1) then
c       print *,'isrt < 1',isrt
        isrt =  1
      endif
      if (iend.gt.nx) then
c       print *,'iend > nx',iend
        iend =  nx
      endif

      if (jsrt.lt.1) then
c       print *,'jsrt < 1',jsrt
        jsrt =  1
      endif
      if (jend.gt.ny) then
c       print *,'jend > ny',jend
        jend =  ny
      endif

c Count the number of values near topo(ii,jj) that are less than
c   topo(ii,jj).
      count = 0.0
      do j=jsrt,jend
        do i=isrt,iend
          if (topo(i,j).lt.topo(ii,jj)) then
            count = count + 1.0
          endif
        enddo
      enddo
      npts = (iend - isrt + 1) * (jend - jsrt + 1) - 1
      frac = count / real(npts)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine smoother_n (nx,ny,ijdelta,topo)

      implicit none

      integer i,j,nx,ny,ii,jj,ijdelta,jsrt,jend,isrt,iend,npts
      real topo(nx,ny)
      real topo_smth(nx,ny)
      real points

c Perform an n-point smoothing operation.

c The result at each grid point is a weighted average of the grid
c   point and the surrounding (1+2*n) x (1+2*n) points.

c n is defined to be the number of points outside the center
c   point that will be used in the average.  So, if n = 2, then
c   you will be averaging (2+1+2)^2 = 25 points.
      do j=1,ny

        if (mod(j,100).eq.0) print *,'j =',j

        do i=1,nx

          isrt = i - ijdelta
          iend = i + ijdelta

          jsrt = j - ijdelta
          jend = j + ijdelta

c Make sure you don't go outside of the simulation domain.
          if (isrt.lt.1) then
c           print *,'isrt < 1',isrt
            isrt =  1
          endif
          if (iend.gt.nx) then
c           print *,'iend > nx',iend
            iend =  nx
          endif

          if (jsrt.lt.1) then
c           print *,'jsrt < 1',jsrt
            jsrt =  1
          endif
          if (jend.gt.ny) then
c           print *,'jend > ny',jend
            jend =  ny
          endif

          topo_smth(i,j) = 0.0
          points = 0.0
          do jj=jsrt,jend
            do ii=isrt,iend
              points = points + 1.0
              topo_smth(i,j) = topo_smth(i,j) + topo(ii,jj)
            enddo
          enddo

          npts = (iend - isrt + 1) * (jend - jsrt + 1) - 1
          topo_smth(i,j) = topo_smth(i,j) / real(npts)

        enddo
      enddo

c Update the final smoothed array that will be sent back.
      do j=1,ny
        do i=1,nx
          topo(i,j) = topo_smth(i,j)
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

