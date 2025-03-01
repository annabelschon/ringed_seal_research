c 1_mk_vege_flt_file.f

      implicit none

      integer nx,ny,i,j,nx_max,ny_max

      parameter (nx_max=5000,ny_max=5000)

      real vege(nx_max,ny_max)

      character*4 cvege

      data cvege /'vege'/

      double precision xmn,ymn
      real deltaxy,undef

c Define the run name that will be used in the flt/tif file names.
      character*2 run_name
      parameter (run_name='SM')

c Point to where the SnowModel output data are located.
      character path_in*(*)
      parameter (path_in=
     &  '../1_mk_clean/')

c Point to where the SnowModel .flt and .hdr files should be
c   put.
      character path_out*(*)
      parameter (path_out='tif_files/')

c Read in the domain config information.
c     open (31,file=
c    &  '../1_topo/outputs/3_nxny_dxdy_xmnymn.dat')
c     read (31,91) nx
c     read (31,91) ny
c     read (31,92) deltaxy
c     read (31,92) deltaxy
c     read (31,93) xmn
c     read (31,93) ymn

      nx = 690
      ny = 990
      deltaxy = 500.0
      deltaxy = 500.0
      xmn = 398250.0
      ymn = 8490250.0

c llcorner_x = 398000.0
c llcorner_y = 8490000.0

c 91  format (9x,i8)
c 92  format (9x,f8.1)
c 93  format (9x,f12.2)

      undef = -9999.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Input data.
      open (21,file=path_in//'seals_vege_mask_500m.gdat',
     &  form='unformatted',access='direct',recl=4*nx)

      do j=1,ny
        read (21,rec=j) (vege(i,j),i=1,nx)
      enddo

c Save the data as .flt files.
      call flt_outputs(nx,ny,path_out,nx_max,ny_max,
     &  vege,cvege,xmn,ymn,deltaxy,undef,run_name)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine flt_outputs(nx,ny,path_out,nx_max,ny_max,
     &  var,cvar,xmn,ymn,deltaxy,undef,run_name)

      implicit none

      integer i,j,nx,ny,nx_max,ny_max,jj
      real var(nx_max,ny_max)
      double precision xmn,ymn
      real deltaxy,undef

      character*(*) path_out
      character*2 run_name
      character*11 f_name1
      character*11 f_name2
      character*120 f_p_name1
      character*120 f_p_name2
      character*4 cvar

c This is the file name without the path.
      f_name1 = run_name//'_'//cvar//'.flt'
      f_name2 = run_name//'_'//cvar//'.hdr'

c This is the file name with the path included.
      f_p_name1 = path_out//f_name1
      f_p_name2 = path_out//f_name2

c Open the file.
      open (71,file=f_p_name1,
     &  form='unformatted',access='direct',recl=4*nx,
     &  status='replace')

c Save the data.  Note that this has to be y-reversed.
      do j=1,ny
        jj = ny + 1 - j
        write (71,rec=j) (var(i,jj),i=1,nx)
      enddo

c Close the file so the same file number can be used next time.
      close (71)

c Now write out the .hdr file.
      open (72,file=f_p_name2)

      write (72,81) nx
      write (72,82) ny
      write (72,83) xmn
      write (72,84) ymn
      write (72,85) deltaxy
      write (72,86) undef
      write (72,87)
      write (72,88)
      write (72,89)

      close (72)

c Define the header file formats.
  81  format ('ncols        ',i10)
  82  format ('nrows        ',i10)
  83  format ('xllcenter    ',f14.3)
  84  format ('yllcenter    ',f14.3)
  85  format ('cellsize     ',f14.3)
  86  format ('NODATA_value ',f14.3)
  87  format ('nbits                32')
  88  format ('pixeltype         FLOAT')
  89  format ('byteorder      LSBFIRST')

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

