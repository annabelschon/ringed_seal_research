c barnes_interp_w-beta.f

      implicit none

      integer nx,ny,nstns_max

      parameter (nx=1001,ny=1001)
      parameter (nstns_max=10000)

      integer i,j,k          ! col, row counters
      real deltax            ! grid increment in x
      real deltay            ! grid increment in y
      double precision xmn   !center x coords of lower left grid cell
      double precision ymn   !center y coords of lower left grid cell

      double precision xstn(nstns_max) ! input stn x coords
      double precision ystn(nstns_max) ! input stn y coords

      real var(nstns_max)    ! input values
      real dn                ! average observation spacing

      real grid(nx,ny)   ! output values

      character*80 infname   ! input (station data) file name
      character*80 outfname  ! output file name

      real undef       ! undefined value
      integer ifill    ! flag (=1) forces a value in every cell
      integer iobsint  ! flag (=1) use dn value from .par file

      integer kk,nstns,nbetas
      real beta,stnid

c Provide the input information.
      xmn = 410011.
      ymn = 400999.

      deltax = 30.0
      deltay = 30.0

      infname = 'fraser_swe_2003_03_25.dat'
      outfname = 'obs_gridded.gdat'

      undef = -9999.0
      ifill = 1
      iobsint = 0

c Read in the station data.
      open (21,file=infname,form='formatted')

      read (21,*) nstns
      do k=1,nstns
        read (21,*) stnid,xstn(k),ystn(k),var(k)
      enddo

c Open the grads output data file.
      open(41,file=outfname,
     &  form='unformatted',access='direct',recl=4*nx*ny)

c  Beta controls the interpolation distance weights.  Beta = 1.0
c    will give you a very smooth field, and correction factor
c    distributions that may not produce swe's that exactly match
c    the observations.  Beta << 1.0 will give you correction factor
c    fields that go right through the data.  If you just have one
c    data point/area, beta is not used.
c     beta = 1.0
c     beta = 0.5
c     beta = 0.1

      nbetas = 10

c Loop through the beta values you are interested in.
      do kk=1,nbetas

        beta = 1.0 - 0.1 * real(kk-1)

c Use the barnes oi scheme to grid the station data.
        call get_dn(nx,ny,deltax,deltay,nstns,dn,iobsint)

        dn = beta * dn

        print *,'Working on beta =',kk,beta
        print *,'You are assuming an average obs spacing (in m) of',dn

        call barnes_oi(nx,ny,deltax,deltay,xmn,ymn,
     &    nstns,xstn,ystn,var,dn,grid,undef,ifill)

c Write the output data to a grads file.
        write(41,rec=kk) ((grid(i,j),i=1,nx),j=1,ny)

      enddo

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_dn(nx,ny,deltax,deltay,nstns,dn,iobsint)

      implicit none

      integer nx,ny,nstns
      real deltax,deltay,dn
      real dn_max           ! the max obs spacing, dn_r
      real dn_min           ! dn_r, for large n
      integer iobsint       ! flag (=1) use dn value from .par file

c Calculate an appropriate filtered wavelength value.  First
c   calculate dn for the case of severely nonuniform data, and
c   then for the case where there is just about a station for
c   every grid cell.  Then assume that the average of these two
c   is a reasonable value to use in the interpolation.
        dn_max = sqrt(deltax*real(nx) * deltay*real(ny)) *
     &    ((1.0 + sqrt(real(nstns))) / (real(nstns) - 1.0))
        dn_min = sqrt((deltax*real(nx) * deltay*real(ny)) /
     &    real(nstns))

        if (iobsint.eq.1) then
c         dn = dn
        else
          dn = 0.5 * (dn_min + dn_max)
        endif

c       print *,'You are using an average obs spacing of',dn
c       print *,'  the program indicates a min, max range of',
c    &    dn_min,dn_max

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine barnes_oi(nx,ny,deltax,deltay,xmn,ymn,
     &  nstns,xstn,ystn,var,dn,grid,undef,ifill)

c This is an implementation of the Barnes objective analysis scheme
c   as described in:
c
c   Koch, S. E., M. DesJardins, and P. J. Kocin, 1983: An
c   interactive Barnes objective map analysis scheme for use with
c   satellite and conventional data. J. Climate and Applied
c   Meteorology, 22(9), 1487-1503.

      implicit none

      real gamma
      parameter (gamma=0.2)
      real pi

      integer nx       ! number of x output values
      integer ny       ! number of y output values
      real deltax      ! grid increment in x
      real deltay      ! grid increment in y
      double precision xmn !center x coords of lower left grid cell
      double precision ymn !center y coords of lower left grid cell

      integer nstns        ! number of input values, all good
      double precision xstn(nstns) ! input stn x coords
      double precision ystn(nstns) ! input stn y coords
      real var(nstns)      ! input values
      integer nflag        ! determines if output will be undef value
      real undef           ! undefined value

      real dn                  ! average observation spacing
      real grid(nx,ny) ! output values

      integer i,j      ! col, row counters
      integer mm,nn    ! station counters
      integer ifill    ! flag (=1) forces a value in every cell

      double precision xg,yg !temporary x and y coords of current cell
      real w1,w2       ! weights for Gauss-weighted average
      real wtot1,wtot2 ! sum of weights
      real ftot1,ftot2 ! accumulators for values, corrections
      real dsq         ! delx**2 + dely**2
      double precision xa,ya       ! x, y coords of current station
      double precision xb,yb       ! x, y coords of current station
      real dvar(nstns)   ! estimated error

      real xkappa_1    ! Gauss constant for first pass
      real xkappa_2    ! Gauss constant for second pass
      real rmax_1      ! maximum scanning radii, for first
      real rmax_2      ! and second passes
      real anum_1      ! numerator, beyond scanning radius,
      real anum_2      ! for first and second passes

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Compute the first and second pass values of the scaling parameter
c   and the maximum scanning radius used by the Barnes scheme.
c   Values above this maximum will use a 1/r**2 weighting.  Here I
c   have assumed a gamma value of 0.2.

c First-round values, Eqn (13).
      pi = 2.0 * acos(0.0)
      xkappa_1 = 5.052 * (2.0*dn/pi)**2

c Define the maximum scanning radius to have weight defined by
c   wt = 1.0 x 10**(-30) = exp(-rmax_1/xkappa_1)
c Also scale the 1/r**2 wts so that when dsq = rmax, the wts match.
      rmax_1 = xkappa_1 * 30.0 * log(10.0)
      anum_1 = 1.0e-30 * rmax_1

c Second-round values, Eqn (4).
      xkappa_2 = gamma * xkappa_1
      rmax_2 = rmax_1 * gamma
      anum_2 = 1.0e-30 * rmax_2

c Scan each input data point and construct estimated error, dvar, at
c   that point.
      do 222 nn=1,nstns

        xa = xstn(nn)
        ya = ystn(nn)
        wtot1 = 0.0
        ftot1 = 0.0

        do 111 mm=1,nstns

          xb = xstn(mm)
          yb = ystn(mm)
          dsq = (xb - xa)**2 + (yb - ya)**2

          if (dsq.le.rmax_1) then

            w1 = exp((- dsq)/xkappa_1)

          else

c Assume a 1/r**2 weight.
            w1 = anum_1/dsq

          endif

          wtot1 = wtot1 + w1
          ftot1 = ftot1 + w1 * var(mm)

  111   continue    ! end loop on sites m

        if (wtot1.eq.0.0) print *,'stn wt totals zero'

        dvar(nn) = var(nn) - ftot1/wtot1

  222 continue        ! end prediction loop on sites nn

c Grid-prediction loop.  Generate the estimate using first set of
c   weights, and correct using error estimates, dvar, and second
c   set of weights.

      do 666 j=1,ny
      do 555 i=1,nx

c xcoords of grid nodes at index i,j
c ycoords of grid nodes at index i,j
        xg = xmn + deltax * (real(i) - 1.0)
        yg = ymn + deltay * (real(j) - 1.0)

c Scan each input data point.
        ftot1 = 0.0
        wtot1 = 0.0
        ftot2 = 0.0
        wtot2 = 0.0
        nflag = 0

        do 333 nn=1,nstns
           
          xa = xstn(nn)
          ya = ystn(nn)
          dsq = (xg - xa)**2 + (yg - ya)**2

          if (dsq.le.rmax_2) then

            w1 = exp((- dsq)/xkappa_1)
            w2 = exp((- dsq)/xkappa_2)

          elseif (dsq.le.rmax_1) then

            w1 = exp((- dsq)/xkappa_1)
            w2 = anum_2/dsq

          else

c Assume a 1/r**2 weight.
            w1 = anum_1/dsq
            nflag = nflag + 1
c With anum_2/dsq.
            w2 = gamma * w1

          endif

          wtot1 = wtot1 + w1
          wtot2 = wtot2 + w2
          ftot1 = ftot1 + w1 * var(nn)
          ftot2 = ftot2 + w2 * dvar(nn)
           
  333   continue    ! end loop on data sites nn

        if (wtot1.eq.0.0 .or. wtot2.eq.0.0) print *,'wts total zero'

        if (ifill.eq.1) then
          grid(i,j) = ftot1/wtot1 + ftot2/wtot2
        else
          if (nflag.lt.nstns) then
            grid(i,j) = ftot1/wtot1 + ftot2/wtot2
          else
            grid(i,j) = undef
          endif
        endif

  555 continue         ! end loop on cols i
  666 continue         ! end loop on rows j

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

