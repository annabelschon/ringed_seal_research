c regrid.f

      parameter (nx1=3200,ny1=4800)
      parameter (nx2=800,ny2=1200)

c Define the number of grid cells I am averaging in each direction.
      parameter (ijdelta=4)

      parameter (ntypes=25)
      integer num_in_class(ntypes)

      real topo1(nx1,ny1)
      real topo2(nx2,ny2)

      real vege1(nx1,ny1)
      real vege2(nx2,ny2)

c Input file.
      open (21,file='topo_vege_25m.gdat',
     &  form='unformatted',access='direct',recl=4*nx1*ny1)

c Output file.
      open (31,file='topo_vege_100m.gdat',
     &  form='unformatted',access='direct',recl=4*nx2*ny2,
     &  status='replace')

c Read in the higher-resolution inputs.
      read (21,rec=1) ((topo1(i,j),i=1,nx1),j=1,ny1)
      read (21,rec=2) ((vege1(i,j),i=1,nx1),j=1,ny1)

c Sweep through the data, one 'ijdelta x ijdelta' block at a time,
c   extracting the dominant class in the data block.
      do j=1,ny2
        jsrt = (j - 1) * ijdelta + 1
        jend = (j - 1) * ijdelta + ijdelta

        do i=1,nx2
          isrt = (i - 1) * ijdelta + 1
          iend = (i - 1) * ijdelta + ijdelta

c DO THE VEGETATION.
c DO THE VEGETATION.
c DO THE VEGETATION.

c Initialize the counting array.
          do k=1,ntypes
             num_in_class(k) = 0
          enddo

c Sweep through this block, extracting the dominant class. If there is a
c   tie, let it take the last one (the largest class number).
          do jj=jsrt,jend
            do ii=isrt,iend
              idat = nint(vege1(ii,jj))
              num_in_class(idat) = num_in_class(idat) + 1
            enddo
          enddo

c Extract the dominant class; in the case of a tie, the last occurance
c   wins.
          imax = 0
          do k=1,ntypes
            if (num_in_class(k).ge.imax) then
              iveg = k
              imax = num_in_class(k)
            endif
          enddo

c Place that class in the new array.
          vege2(i,j) = real(iveg)

c AVERAGE THE TOPOGRAPHY.
c AVERAGE THE TOPOGRAPHY.
c AVERAGE THE TOPOGRAPHY.

          topo2(i,j) = 0.0
          do jj=jsrt,jend
            do ii=isrt,iend
              topo2(i,j) = topo2(i,j) + topo1(ii,jj)
            enddo
          enddo
          topo2(i,j) = topo2(i,j) / real(ijdelta*ijdelta)

        enddo

      enddo

c Save the data.
      write (31,rec=1) ((topo2(i,j),i=1,nx2),j=1,ny2)
      write (31,rec=2) ((vege2(i,j),i=1,nx2),j=1,ny2)

      end

