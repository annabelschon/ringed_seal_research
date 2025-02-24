c 2_time_smth_inv_index.f

c Here I am taking into account the inversion history by making
c   the current day equal to the average of the current day and the
c   previous 2 days, i.e., a 3 day running, backwards, mean.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

      integer i,j,nt,n,nx,ny

      parameter (nt=731)

c A coarse gridded version of the outputs.
      parameter (nx=2649,ny=2528)

      real zinv1(nx,ny)
      real zinv2(nx,ny)
      real zinv3(nx,ny)
      real zinv4(nx,ny)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Inputs.
      open (21,file='/data4/moose/oldcrow/process/inversion_index.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Outputs.
      open (31,file='/data4/moose/oldcrow/process/invi.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

c Deal with the first 2 days.
      do n=1,2
        read (21,rec=n) ((zinv4(i,j),i=1,nx),j=1,ny)
        write (31,rec=n) ((zinv4(i,j),i=1,nx),j=1,ny)
      enddo

      do n=3,nt

        print *, n

        read (21,rec=n-2) ((zinv1(i,j),i=1,nx),j=1,ny)
        read (21,rec=n-1) ((zinv2(i,j),i=1,nx),j=1,ny)
        read (21,rec=n) ((zinv3(i,j),i=1,nx),j=1,ny)

        do j=1,ny
          do i=1,nx
            zinv4(i,j) = (zinv1(i,j) + zinv2(i,j) + zinv3(i,j)) / 3.0
          enddo
        enddo

        write (31,rec=n) ((zinv4(i,j),i=1,nx),j=1,ny)

      enddo

      end

