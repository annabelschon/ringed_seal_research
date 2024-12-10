c 1_fill_undefs.f

c This program finds 2D undefined areas, interpolates linearly
c   across the undefined patch in the x and y directions, and
c   averages the result to fill in the undefined space.

      implicit none

      integer nx,ny,i,j,ioffset,maxiter,iter

c This is the domain of interest.
      parameter (nx=304,ny=448)

c This is the input path.
      character path1*(*)
      parameter (path1=
     &  '/data3/annabel/seals/1_nsidc_0051/3_gdat_final/')

c This is the output path.
      character path2*(*)
      parameter (path2 =
     &  '/data3/annabel/seals/1_nsidc_0051/4_gdat_extracted_years/')

      real conc(nx,ny)
      real mask(nx,ny)

      real undef,undef_pole_hole

c Undefined values.
      undef = -9999.0
      undef_pole_hole = 251.0

c Define the temporal subdomain you want to extract.
      maxiter = 15705
c     maxiter = 1
      ioffset = 213

c Open the input file.
      open (unit=21,file=path1//'ice_conc_v3.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Open the output files.
      open (31,file=path2//'ice_conc_extracted_domain.gdat',
c      open (31,file='ice_conc_extracted_domain.gdat',
     &  form='unformatted',access='direct',recl=4*1*nx*ny,
     &  status='replace')

      open (41,file=path2//'ice_conc_mask.gdat',
c     open (41,file='ice_conc_mask.gdat',
     &  form='unformatted',access='direct',recl=4*1*nx*ny,
     &  status='replace')

c Run the data extraction and write procedure.
      do iter=1,maxiter

        if (mod(iter,100).eq.0) print *, 'iter =',iter

        read (21,rec=iter+ioffset) ((conc(i,j),i=1,nx),j=1,ny)

c Linearly interpolate across the polar hole.
        call fill_2D_undefs (nx,ny,conc,undef_pole_hole)

c Scale the data by 0.4 to get percentage (0-100) sea ice
c   concentration.  Also create a separate mask that identifies
c   the polar hole, land, etc.
        do j=1,ny
          do i=1,nx

c Assign mask values first to avoid setting the concentrations
c   to undef.

c Annabel, when I first ran this program, I left this test in the
c   run:
             if (conc(i,j) .eq. 251.0) then
               print *, 'something wrong, N Pole hole found',iter,i,j
             elseif (conc(i,j) .eq. 252.0) then
               print *, '252, are any of these ever used?',iter,i,j
             endif

c It came back without any prints to the screen.  So, there are
c   not any 251's anymore, and there never were any 252's.  So,
c   that means the land and coastline mask is constant over the
c   entire time period.  So I did this (made a mask with 3 values,
c   land = 1, coastline = 2, and ocean = 3), and ran it again.
             if (iter.eq.1) then
               if (conc(i,j) .eq. 254.0) then
                 mask(i,j) = 1.0
               elseif (conc(i,j) .eq. 253.0) then
                 mask(i,j) = 2.0
               else
                 mask(i,j) = 3.0
               endif
             endif

             if (conc(i,j) .le. 250.0) then
               conc(i,j) = 0.4 * conc(i,j)
             else
               conc(i,j) = undef
             endif

          enddo
        enddo

c Save the data.
*        print *, 'Writing data to mask file...'
        write (31,rec=iter)
     &    ((conc(i,j),i=1,nx),j=1,ny)
*        print *, 'Data written successfully!'
      enddo

c Save the mask (note: just one record/layer, constant with time).
      write (41,rec=1)
     &  ((mask(i,j),i=1,nx),j=1,ny)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine fill_2D_undefs (nx,ny,topo,undef)

      implicit none

      integer nx,ny,i,j

      real topo(nx,ny)
      real topo_orig(nx,ny)
      real topo_x(nx,ny)
      real topo_y(nx,ny)

      integer istart,iend,iflag,jstart,jend,jflag,ii,jj
      real x1,x2,y1,y2,xx
      real undef

c Undefs along the border will cause problems with this
c   interpolation code.  So, if there are undefs there, set them
c   to something reasonable, or at least something that will not
c   create negative numbers.  Zero works, but may not be very
c   realistic.

c Annabel, this is not needed, because you are looking at the
c   interior of your domain, not at the outside edges.  So, I
c   have commented it out.

c     print *, 'filling undef areas'
c     do j=1,ny
c       i = 1
c       if (topo(i,j).eq.undef) topo(i,j) = 0.0
c       i = 2
c       if (topo(i,j).eq.undef) topo(i,j) = 0.0

c       i = nx-1
c       if (topo(i,j).eq.undef) topo(i,j) = 0.0
c       i = nx
c       if (topo(i,j).eq.undef) topo(i,j) = 0.0
c     enddo

c     do i=1,nx
c       j = 1
c       if (topo(i,j).eq.undef) topo(i,j) = 0.0
c       j = 2
c       if (topo(i,j).eq.undef) topo(i,j) = 0.0

c       j = ny-1
c       if (topo(i,j).eq.undef) topo(i,j) = 0.0
c       j = ny
c       if (topo(i,j).eq.undef) topo(i,j) = 0.0
c     enddo

c Save a copy of the original topo array.
      do j=1,ny
        do i=1,nx
          topo_orig(i,j) = topo(i,j)
          topo_x(i,j) = undef
          topo_y(i,j) = undef
        enddo
      enddo

c Sweep through the domain, linearly interpolating across any
c   undef areas.  Do the linear interpolation in x and y and then
c   average the results to get the final topo value.

c x.
      do j=1,ny

c       if (mod(j,10000).eq.0) print *, 'j =',j

        istart = 0
        iend = 0
        iflag = 0
        do i=2,nx-1
          if (topo(i,j).ne.undef .and. topo(i+1,j).eq.undef .and.
     &        istart.eq.0) then
            istart = i
            iflag = 0
          endif
          if (topo(i,j).ne.undef .and. topo(i-1,j).eq.undef .and.
     &        istart.ne.0) then
            iend = i
            iflag = 1
          endif
          if (iflag.eq.1) then
            x1 = real(istart)
            x2 = real(iend)
c           print *, 'in x',x1,x2
            y1 = topo(istart,j)
            y2 = topo(iend,j)
            do ii=istart+1,iend-1
              xx = real(ii)
              topo_x(ii,j) = y1 + (xx - x1) * (y2 - y1)/(x2 - x1)
            enddo
            if (topo(i,j).ne.undef .and. topo(i+1,j).eq.undef) then
              istart = iend
              iend = 0
              iflag = 0
            else
              istart = 0
              iend = 0
              iflag = 0
            endif
          endif
        enddo
      enddo

c y.
      do i=1,nx

c       if (mod(i,10000).eq.0) print *, 'i =',i

        jstart = 0
        jend = 0
        jflag = 0
        do j=2,ny-1
          if (topo(i,j).ne.undef .and. topo(i,j+1).eq.undef .and.
     &        jstart.eq.0) then
            jstart = j
            jflag = 0
          endif
          if (topo(i,j).ne.undef .and. topo(i,j-1).eq.undef .and.
     &        jstart.ne.0) then
            jend = j
            jflag = 1
          endif
          if (jflag.eq.1) then
            x1 = real(jstart)
            x2 = real(jend)
c           print *, 'in y',x1,x2
            y1 = topo(i,jstart)
            y2 = topo(i,jend)
            do jj=jstart+1,jend-1
              xx = real(jj)
              topo_y(i,jj) = y1 + (xx - x1) * (y2 - y1)/(x2 - x1)
            enddo
            if (topo(i,j).ne.undef .and. topo(i,j+1).eq.undef) then
              jstart = jend
              jend = 0
              jflag = 0
            else
              jstart = 0
              jend = 0
              jflag = 0
            endif
          endif
        enddo
      enddo

c Fill in the missing data.
      do j=1,ny
        do i=1,nx
          if (topo_orig(i,j).eq.undef) then
            topo(i,j) = (topo_x(i,j) + topo_y(i,j)) / 2.0
          else
            topo(i,j) = topo_orig(i,j)
          endif
        enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

