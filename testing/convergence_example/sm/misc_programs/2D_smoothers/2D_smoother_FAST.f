c 2D_smoother_FAST.f

c This implements Ethan Gutmann's super-fast 2D block-averaging
c   scheme.  There is NO reason to do this any other way. :)

c By commenting and uncommenting the 'cc' code below, you can get
c   either a tiny test domain that you can see the numbers from,
c   or a larger domain.

      implicit none

      integer nx,ny,i,j,n_windowsize

cc    parameter (nx=2649,ny=2528)
      parameter (nx=8,ny=5)

      real topo(nx,ny)
      real topo_ave1(nx,ny)
      real topo_ave2(nx,ny)

      real user_sys(2),t1,t2

c Read in the topography data.
cc    open (21,file='topo.gdat',
cc   &  form='unformatted',access='direct',recl=4*nx*ny)
cc    read (21,rec=1) ((topo(i,j),i=1,nx),j=1,ny)

c A small, visual, test domain.
      do j=1,ny
        do i=1,nx
          topo(i,j) = real(i-1) + real(j-1)
c         topo(i,j) = real(i)
c         topo(i,j) = 10.0
        enddo
      enddo

      print *
      do j=1,ny
        write(*,91) (topo(i,j),i=1,nx)
      enddo
      print *

c Halfwidth = 1/2 of window to smooth over.  This defines a
c   smoothing window of 2*windowsize+1 in each direction.  For
c   example, a window size of 25 means a 51 x 51 pixel window.
cc    n_windowsize = 25
      n_windowsize = 2

c This is the slow way.

c Smooth the data.
      print *, 'in slow way'
      t1=dtime(user_sys)

      call smooth_2d_array_SLOW (nx,ny,n_windowsize,topo,topo_ave1)

      t2=dtime(user_sys)
      print *, t2 - t1
      print *, 'out slow way'

      print *
      do j=1,ny
        write(*,91) (topo_ave1(i,j),i=1,nx)
      enddo
      print *

c This one comes back with the same name that goes in.
      do j=1,ny
        do i=1,nx
          topo_ave2(i,j) = topo(i,j)
        enddo
      enddo

      print *, 'in fast way'
      t1=dtime(user_sys)

      call smooth_2d_array_FAST (nx,ny,topo_ave2,n_windowsize)

      t2=dtime(user_sys)
      print *, t2 - t1
      print *, 'out fast way'

      print *
      do j=1,ny
        write(*,91) (topo_ave2(i,j),i=1,nx)
      enddo
      print *

c Save the data.
cc    open (31,file='topo_smoothed.gdat',
cc   &  form='unformatted',access='direct',recl=4*nx*ny,
cc   &  status='replace')
cc    write (31,rec=1) ((topo(i,j),i=1,nx),j=1,ny)
cc    write (31,rec=2) ((topo_ave1(i,j),i=1,nx),j=1,ny)
cc    write (31,rec=3) ((topo_ave2(i,j),i=1,nx),j=1,ny)

   91 format (8f5.1)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine smooth_2d_array_FAST (nx,ny,topo,n_windowsize)

c This is Ethan Gutmann's 2D smoother.  Here I have just
c   implemented with old-style Fortran so you can see exactly
c   what the code is doing.  How much faster this is depends on
c   the size of the block, but a block about 50 x 50 pixels in
c   size can be something like 50 to 100 times faster than using
c   standard, brute-force, block averaging.

      implicit none

      integer nx,ny,i,j,startx,endx,starty,ncols,nrows,jj,endy

c 2D input field to be smoothed.
      real topo(nx,ny)

c Halfwidth = 1/2 of window to smooth over.  This defines a
c   smoothing window of 2*windowsize+1 in each direction.
      integer n_windowsize

      real topo_tmp(nx,ny)

c Intermediate sums to speed up the computation.
      real rowsums(nx),rowmeans(nx)

      real sum

c Make a copy so we always use the unsmoothed data when computing
c   the smoothed data.
      do j=1,ny
        do i=1,nx
          topo_tmp(i,j) = topo(i,j)
        enddo
      enddo

c Number of rows and columns in the total smoothing window.
      if (n_windowsize*2+1.gt.nx .or. n_windowsize*2+1.gt.ny) then
        write (*,*) 'WARNING: can not operate if windowsize*2+1 is'
        write (*,*) 'larger than nx or ny'
        write (*,*) 'nx           = ', nx
        write (*,*) 'ny           = ', ny
        write (*,*) 'n_windowsize = ', n_windowsize
        stop
      endif

c Do this looping with x on the inside and y on the outside.
      do j=1,ny

        if (mod(j,1000).eq.0) print *, 'smoothing, j = ', j

c Compute the sum over rows for each column in the current window.
c   So, this is calculating the values over a column window
c   for each x position.
        do i=1,nx
          rowsums(i) = 0.0
        enddo

        starty = max(1,j-n_windowsize)
        endy = min(ny,j+n_windowsize)
        nrows = endy - starty + 1

        do i=1,nx
          do jj=starty,endy
            rowsums(i) = rowsums(i) + topo_tmp(i,jj)
          enddo
        enddo

c Calculate the row means.  Again, these are the means for a
c   window worth of rows, for each column.
        do i=1,nx
          rowmeans(i) = rowsums(i) / nrows
        enddo

c Sum the row means just inside of the left edge.
        sum = 0.0
        do i=1,n_windowsize+1
          sum = sum + rowmeans(i)
        enddo

        do i=1,nx

c We are at the left edge.
          if (i.eq.1) then
            startx = 1
            endx   = 1 + n_windowsize
            ncols  = endx - startx + 1
            sum = sum

c We are near the left edge.
          elseif (i.le.n_windowsize+1) then
            startx = 1
            endx   = i + n_windowsize
            ncols  = endx - startx + 1
            sum = sum + rowmeans(endx)

c We are near the right edge.
          elseif (i + n_windowsize.gt.nx) then
            startx = i - n_windowsize
            endx   = nx
            ncols  = endx - startx + 1
            sum = sum - rowmeans(startx-1)

c We are in the middle, away from the edges.
          else
            startx = i - n_windowsize
            endx   = i + n_windowsize
            ncols  = endx - startx + 1
            sum = sum - rowmeans(startx-1) + rowmeans(endx)
          endif

          topo(i,j) = sum / ncols

        enddo

      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine smooth_2d_array_SLOW (nx,ny,n_windowsize,var,avevar)

c This is just a brute-force block-averaging smoother.

      real var(nx,ny)
      real avevar(nx,ny)

c Smoothing window.
      ksmooths = 2 * n_windowsize + 1

c Loop through each of the data points, in each direction.
      do i=1,nx

        if (mod(i,100).eq.0) print *, 'smoothing, i = ', i

        do j=1,ny
          jpts = int(ksmooths/2)
          jstart = max(1,j-jpts)
          jend = min(ny,j+jpts)
          njpts = jend - jstart + 1

          ipts = int(ksmooths/2)
          istart = max(1,i-ipts)
          iend = min(nx,i+ipts)
          nipts = iend - istart + 1

          avevar(i,j) = 0.0

          do jj=jstart,jend
            do ii=istart,iend
              avevar(i,j) = avevar(i,j) + var(ii,jj)
            enddo
          enddo
          avevar(i,j) = avevar(i,j) / real(njpts*nipts)
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

