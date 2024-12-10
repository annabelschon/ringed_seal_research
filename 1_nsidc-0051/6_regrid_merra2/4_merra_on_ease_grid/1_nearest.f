c 1_nearest.f

c Perform the MERRA2 reprojection by selecting the nearest
c   neighbor value.

      implicit none

      integer nx,ny,nsx,nsy,i,j,kkk,irec,ii,jj

c The MERRA2 grid.
      parameter (nsx=576,nsy=161)

c The EASE grid.
      parameter (nx=361,ny=361)

      real tair2(nx,ny)
      real relh2(nx,ny)
      real wspd2(nx,ny)
      real wdir2(nx,ny)
      real prec2(nx,ny)

      real tair(nsx,nsy)
      real relh(nsx,nsy)
      real wspd(nsx,nsy)
      real wdir(nsx,nsy)
      real prec(nsx,nsy)

      real x_ipos(nx,ny)
      real y_jpos(nx,ny)

      integer maxiter_3hrly,ioffset_3hrly

c Original MERRA2 data.
      character path1*(*)
      parameter (path1=
     &  '/data2/atmos_forcing/merra2/grads_archive/3hrly/')

      character path2*(*)
      parameter (path2='/data3/lagrangian_update4/merra2/3hrly/')

      real undef

      undef = -9999.0

c Open the MERRA2 input files (on the original MERRA2 grid).
      open (51,file=path1//'tair_3hrly.gdat',
     &  form='unformatted',access='direct',recl=4*nsx*nsy)

      open (52,file=path1//'relh_3hrly.gdat',
     &  form='unformatted',access='direct',recl=4*nsx*nsy)

      open (53,file=path1//'wspd_3hrly.gdat',
     &  form='unformatted',access='direct',recl=4*nsx*nsy)

      open (54,file=path1//'wdir_3hrly.gdat',
     &  form='unformatted',access='direct',recl=4*nsx*nsy)

      open (55,file=path1//'prec_3hrly.gdat',
     &  form='unformatted',access='direct',recl=4*nsx*nsy)

c Open the MERRA2 output file (on the EASE grid).
      open (40,file=path2//'ease_met_2017-2023_3hrly.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny*5)

c These are the merra i,j positions, that are closest to each
c   SnowModel grid cell.
      open (unit=41,file='../2_get_nearest/nearest_ij.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (41,rec=1) ((x_ipos(i,j),i=1,nx),j=1,ny)
      read (41,rec=2) ((y_jpos(i,j),i=1,nx),j=1,ny)

c Read in the maxiter and ioffset information for this run.
      open (21,file=
     &  '../3_maxiter_offset/start_end_dates_maxiter_ioffset.dat')

c For a 3-hourly run.
      read (21,*)
      read (21,*)
      read (21,*)
      read (21,*)

      read (21,*)
      read (21,*)
      read (21,*)
      read (21,*)

      read (21,99) maxiter_3hrly
      read (21,99) ioffset_3hrly

   99 format (14x,i10)

c For a daily run use the read command below.  Note that you need
c   to use the mk_mm_daily program if you really want to create a
c   daily file; the calendar stuff is different.

      do kkk=1,maxiter_3hrly

        if (mod(kkk,100).eq.0.0)
     &    print *, '% done =',100.0*real(kkk)/real(maxiter_3hrly)

        irec = kkk + ioffset_3hrly

        read (51,rec=irec) ((tair(i,j),i=1,nsx),j=1,nsy)
        read (52,rec=irec) ((relh(i,j),i=1,nsx),j=1,nsy)
        read (53,rec=irec) ((wspd(i,j),i=1,nsx),j=1,nsy)
        read (54,rec=irec) ((wdir(i,j),i=1,nsx),j=1,nsy)
        read (55,rec=irec) ((prec(i,j),i=1,nsx),j=1,nsy)

c Place the nearest value in the appropriate grid cell.
        do j=1,ny
          do i=1,nx
            ii = nint(x_ipos(i,j))
            jj = nint(y_jpos(i,j))
            tair2(i,j) = tair(ii,jj)
            relh2(i,j) = relh(ii,jj)
            wspd2(i,j) = wspd(ii,jj)
            wdir2(i,j) = wdir(ii,jj)
            prec2(i,j) = prec(ii,jj)
          enddo
        enddo

c Save the data.
        write (40,rec=kkk)
     &    ((tair2(i,j),i=1,nx),j=1,ny),
     &    ((relh2(i,j),i=1,nx),j=1,ny),
     &    ((wspd2(i,j),i=1,nx),j=1,ny),
     &    ((wdir2(i,j),i=1,nx),j=1,ny),
     &    ((prec2(i,j),i=1,nx),j=1,ny)

      enddo

      end

