c 1_mk_inversion_index.f

c Here I am using the caps index and tair and tsfc to define the
c   strength and locations and timing of temperature inversions.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

      integer nx,ny,i,j,nt,n,nn

      parameter (nx=2649,ny=2528)
      parameter (nt=731)

      real topo(nx,ny)
      real vege(nx,ny)

      real tair(nx,ny)
      real tsfc(nx,ny)

      real tdif(nx,ny)
      real cap_smth(nx,ny)
      real zinversion_index(nx,ny)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Inputs.
      open (21,file=
     &  '../1_mk_cap_index/1_topo_vege/NoAm_30m/topo_vege.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (21,rec=1) ((topo(i,j),i=1,nx),j=1,ny)
      read (21,rec=2) ((vege(i,j),i=1,nx),j=1,ny)

      open (31,file='/data4/moose/oldcrow/process/tair.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (32,file='/data4/moose/oldcrow/process/tsfc.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (41,file='../1_mk_cap_index/cap_info.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (41,rec=6) ((cap_smth(i,j),i=1,nx),j=1,ny)

c Outputs.
      open (61,file='/data4/moose/oldcrow/process/inversion_index.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      do n=1,nt

        print *, n

        read (31,rec=n) ((tair(i,j),i=1,nx),j=1,ny)
        read (32,rec=n) ((tsfc(i,j),i=1,nx),j=1,ny)

c Despeckle tsfc.  The assumption here is that isolated tree
c   grid cell heat will be smoothed out by the surrounding tdif
c   conditions.
        nn = 2
        call smoother_n (nx,ny,nn,tsfc)

        do j=1,ny
          do i=1,nx

c Calculate the inversion strength.
            tdif(i,j) = tair(i,j) - tsfc(i,j)

c Only consider inversions (tair is greater then tsfc).
            tdif(i,j) = max(0.0,tdif(i,j))

c Don't consider what is happening over oceans.
            if (vege(i,j).eq.24.0) tdif(i,j) = 0.0

c Calculate the inversion index.
            zinversion_index(i,j) = cap_smth(i,j) * tdif(i,j)

          enddo
        enddo

        write (61,rec=n) ((zinversion_index(i,j),i=1,nx),j=1,ny)

      enddo

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine smoother_n (nx,ny,ijdelta,topo)

      implicit none

      integer i,j,nx,ny,ii,jj,ijdelta,jsrt,jend,isrt,iend
      real topo(nx,ny)
      real topo_smth(nx,ny)
      real points,undef

      undef = -9999.0

c Perform an n-point smoothing operation.

c The result at each grid point is a weighted average of the grid
c   point and the surrounding (1+2*n) x (1+2*n) points.

c n is defined to be the number of points outside the center
c   point that will be used in the average.  So, if n = 2, then
c   you will be averaging (2+1+2)^2 = 25 points.
      do j=1,ny

c       if (mod(j,100).eq.0) print *,'j =',j

        do i=1,nx

          if (topo(i,j).eq.undef) then

            topo_smth(i,j) = undef

          else

            isrt = i - ijdelta
            iend = i + ijdelta

            jsrt = j - ijdelta
            jend = j + ijdelta

c Make sure you don't go outside of the simulation domain.
            if (isrt.lt.1) then
c             print *,'isrt < 1',isrt
              isrt =  1
            endif
            if (iend.gt.nx) then
c             print *,'iend > nx',iend
              iend =  nx
            endif

            if (jsrt.lt.1) then
c             print *,'jsrt < 1',jsrt
              jsrt =  1
            endif
            if (jend.gt.ny) then
c             print *,'jend > ny',jend
              jend =  ny
            endif

            topo_smth(i,j) = 0.0
            points = 0.0

            do jj=jsrt,jend
              do ii=isrt,iend
                if (topo(ii,jj).ne.undef) then
                  points = points + 1.0
                  topo_smth(i,j) = topo_smth(i,j) + topo(ii,jj)
                endif
              enddo
            enddo

            topo_smth(i,j) = topo_smth(i,j) / points

          endif

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

