c make_lg_curve_wt.f

c This program looks at the large-scale curvature in the domain and
c   uses that information to generate a weighting array that is
c   used in SnowTran-3D to reduce the threshold shear strength
c   (Utau_t) of the snowpack.  This is a way to help blow snow off
c   of high, steep mountain slopes in the simulation domain.

c The user defines the curve_length_scale of interest.  Then this
c   is used to calculate the curvature over the topography, which
c   is ultimtely scaled to range from -0.5 to 0.5.  The goal is to
c   create a weighting array that will be multiplied by Utau_t in
c   SnowTran-3D to create areas of lower snow-transport threshold.
c   So the weighting array will have values that range from 0.0 to
c   1.0.

c Areas of large positive curvature values can be assigned a small
c   (constant) weight, and areas of large negative curvature can
c   be assigned a large (constant) weight.  Then the code below
c   linearly interpolates between these two constant values to
c   make a smooth transition between the two constant values.

c The small values will allow the snow to blow off of the steep,
c   mountain slopes more easily, and the broad, flat mountain
c   valleys with the large-weight values will hold their snow
c   more tightly.

c In the code below, x1 and x2 define the curvature values above
c   and below which have constant large-scale curvature weight.
c   y1 and y2 are the constant weights corresponding to x1 and
c   x2, respectively (see the plot below).

c So, curvature values between -0.5 and x1 will equal y1, and
c   curvature values between x2 and 0.5 will equal y2.  And
c   curvature values between x1 and x2 will produce y values
c   that vary linearly from y1 and y2.

c       curve_wt_lg
c            |
c     y1=1.0 |-------
c            |          -
c            |               -
c            |                   -
c     y2=0.2 |                       ----------------
c            |
c            |
c            |________________________________________
c          -0.5      x1  0.0         x2             0.5
c                 (x1=-0.1)      (x2=0.2)
c                          curvature

      implicit none

      integer nx,ny,i,j

c     parameter (nx=3250,ny=2500)
      parameter (nx=500,ny=500)

      real topo(nx,ny)
      real curvature(nx,ny)
      real curve_wt_lg(nx,ny)

      real deltax,deltay,curve_len_scale,x1,x2,y1,y2

c Define the model grid increment.
      deltax = 30.0
      deltay = 30.0

c Define the length scale for the large topographic features in
c   the simulation domain.  This is roughly one-half the wavelength
c   of the features of interest, in meters.
      curve_len_scale = 3000.0

c Define the coefficents in the weighting curve.
      x1 = -0.1
      x2 = 0.2
      y1 = 1.0
      y2 = 0.2

c Read the topo data.
c     open (21,file='../../final/wol_dem_veg_gla_sea_30m.gdat',
      open (21,file='../../wol_topo_veg/sub_domain/wol_subdom_30m.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (21,rec=1) ((topo(i,j),i=1,nx),j=1,ny)

      call get_curvature(nx,ny,deltax,deltay,topo,
     &  curvature,curve_len_scale)

c Use this large-scale curvature array to define a weighting
c   distribution for Utau_t.  This will be used to make the snow
c   blow more easily from steep, big-mountain hillsides.
      do i=1,nx
        do j=1,ny
          if (curvature(i,j).lt.x1) then
            curve_wt_lg(i,j) = y1
          elseif (curvature(i,j).gt.x2) then
            curve_wt_lg(i,j) = y2
          else
            curve_wt_lg(i,j) = (y2-y1)/(x2-x1)*(curvature(i,j)-x1)+y1
          endif
        enddo
      enddo

c Save the data.
      open (31,file='large_curvature_wt.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      write (31,rec=1) ((curve_wt_lg(i,j),i=1,nx),j=1,ny)
      write (31,rec=2) ((curvature(i,j),i=1,nx),j=1,ny)
      write (31,rec=3) ((topo(i,j),i=1,nx),j=1,ny)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_curvature(nx,ny,deltax,deltay,topo,
     &  curvature,curve_len_scale)

      implicit none

      integer i,j,nx,ny,inc

      real deltax,deltay,deltaxy,curve_len_scale,curve_max

      real topo(nx,ny)
      real curvature(nx,ny)

c Deal with the model running at a point, or along single or double
c   lines.
      if (nx.le.2  .or.  ny.le.2) then
        do i=1,nx
          do j=1,ny
            curvature(i,j) = 0.0
          enddo
        enddo
      else

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

      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

