c 2_count_mask_grid_cells.f

      implicit none

      integer nx,ny,ntypes,i,j,jj,idat,k

      parameter (nx=304,ny=448)

      real deltax,deltay,xmn,ymn,undef,sum,count

      real mask(nx,ny)

      parameter (ntypes=18)
      integer num_in_class(ntypes)

      undef = -9999.0

c Read in the .flt mask data.
      open (31,file='tif_flt_data/mask.flt',
     &  form='unformatted',access='direct',recl=4*nx)

c Read the data in as real numbers, and do the yrev.
      do j=1,ny
        jj = ny + 1 - j
        read (31,rec=j) (mask(i,jj),i=1,nx)
      enddo

c Count the number of grid cells in each number-class that is used.
      do k=1,ntypes
         num_in_class(k) = 0
      enddo

      do j=1,ny
        do i=1,nx
          idat = int(mask(i,j))
          num_in_class(idat) = num_in_class(idat) + 1
        enddo
      enddo

      open (51,file='mask_classes_n_grid_cells.txt')
      do k=1,ntypes
        write (51,*) k,num_in_class(k)
      enddo

c Sum the grid cells with mask values.
      sum = 0.0
      do k=1,ntypes
        sum = sum + num_in_class(k)
      enddo

      print *
      print *, sum,nx*ny,100.0*sum/(nx*ny)
      print *, sum,nx*ny,100.0*sum/(nx*ny)
      print *, sum,nx*ny,100.0*sum/(nx*ny)
      print *

c Count how many non-zero classes there are.
      count = 0.0
      do k=1,ntypes
        if (num_in_class(k).ne.0) count = count + 1.0
      enddo

      print *, 'number of classes =',count
      print *

c Save the data in a GrADS file.
      open (71,file='mask.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      write (71,rec=1) ((mask(i,j),i=1,nx),j=1,ny)

c Create a grads .ctl file for this mask.gdat file.
      call mk_mask_ctl (nx,ny)

c Read in enough information to create a _meters .ctl file.
      open (32,file='/home/aschoen/seals/2_snowmodel/sm1/topo_vege/'//
     &  'pan_arctic/SM_domain_config_info_OUTPUTS.dat')
      read (32,*)
      read (32,*)
      read (32,103) deltax
      read (32,104) deltay
      read (32,105) xmn
      read (32,106) ymn

  103 format (9x,f8.1)
  104 format (9x,f8.1)
  105 format (9x,f12.2)
  106 format (9x,f12.2)

      call mk_mask_meters_ctl (nx,ny,deltax,deltay,xmn,ymn)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_mask_meters_ctl (nx,ny,deltax,deltay,xmn,ymn)

      implicit none

      integer nx,ny
      real deltax,deltay,xmn,ymn

      open (61,file='mask_meters.ctl')

      write (61,51)
      write (61,52)
      write (61,53)
      write (61,54) nx,xmn,deltax
      write (61,55) ny,ymn,deltay
      write (61,56)
      write (61,57)
      write (61,58)
      write (61,60)
      write (61,61)

      close (61)

   51 format ('DSET ^mask.gdat')
   52 format ('TITLE LME mask for SnowModel')
   53 format ('UNDEF -9999.0')
   54 format ('XDEF ',i8,' LINEAR ',2f20.8)
   55 format ('YDEF ',i8,' LINEAR ',2f20.8)
   56 format ('ZDEF 1 LINEAR 1 1')
   57 format ('TDEF 1 LINEAR 1jan9999 1yr')
   58 format ('VARS 1')
   60 format ('mask 0 0 LME mask')
   61 format ('ENDVARS')

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mk_mask_ctl (nx,ny)

      implicit none

      integer nx,ny

      open (61,file='mask.ctl')

      write (61,51)
      write (61,52)
      write (61,53)
      write (61,54) nx
      write (61,55) ny
      write (61,56)
      write (61,57)
      write (61,58)
      write (61,60)
      write (61,61)

      close (61)

   51 format ('DSET ^mask.gdat')
   52 format ('TITLE LME mask for SnowModel')
   53 format ('UNDEF -9999.0')
   54 format ('XDEF ',i5,' LINEAR 1.0 1.0')
   55 format ('YDEF ',i5,' LINEAR 1.0 1.0')
   56 format ('ZDEF 1 LINEAR 1 1')
   57 format ('TDEF 1 LINEAR 1jan9999 1yr')
   58 format ('VARS 1')
   60 format ('mask 0 0 LME mask')
   61 format ('ENDVARS')

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

