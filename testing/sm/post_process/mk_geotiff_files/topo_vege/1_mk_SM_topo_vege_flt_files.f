c 1_mk_SM_topo_vege_flt_files.f

c This requires using the gfortran compiler.

c This program reads my SnowModel GrADS archive and creates
c   corresponding daily float (.flt) files.  This is an IEEE
c   floating-point format, 32-bit signed binary file (this is
c   just a y-flipped grads file) archive with one file for
c   each day, and for each variable, with each variable going
c   in a different directory.

c Additional details of this process are described at the end
c   of this program.

      implicit none

      integer nx,ny,i,j,nvars,nx_max,ny_max

c Define the SnowModel simulation domain dimensions.
      parameter (nx_max=10000,ny_max=10000)

      real topo(nx_max,ny_max)
      real vege(nx_max,ny_max)

      double precision xmn,ymn
      real deltaxy,deltax,deltay

      real undef

c The number of variables you want to process.
      parameter (nvars=2)

c This card should not be changed.
      character*4 varnames(nvars)

      data varnames /'topo','vege'/

      character path_in*(*)
      character path_out*(*)
      character path_sm_config*(*)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c BEGIN USER INPUT.
c BEGIN USER INPUT.
c BEGIN USER INPUT.

c Define the run name that will go into the float file names.
c   This is designed to have the format: "NAME_SM_reso".  The
c   "character*12 run_name" card cannot be changed without
c   additional code modifications.
      character*12 run_name
      parameter (run_name='TEST_SM_100m')

c Point to where the SnowModel output data are located.
      parameter (path_in=
     &  '../../../topo_vege/NoAm_30m/')

c Point to where the SnowModel .flt and .hdr files should be
c   put.
      parameter (path_out='flt_files/')

c Point to where the SnowModel configuration info is located.
      parameter (path_sm_config=
     &  '../../../topo_vege/NoAm_30m/')

c END USER INPUT.
c END USER INPUT.
c END USER INPUT.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Create the directories that each variable will be written to.
c   Note that this works with the gfortran compiler.
      call execute_command_line( 'mkdir -p ' // path_out )

c Read in the SnowModel grid information from the topo-vege
c   processing file.
      open (31,file=path_sm_config//'SM_domain_config_info_OUTPUTS.dat')

      read (31,101) nx
      read (31,101) ny
      read (31,102) deltax
      read (31,102) deltay
      read (31,103) xmn
      read (31,103) ymn

  101 format (9x,i8)
  102 format (9x,f8.1)
  103 format (9x,f12.2)

c     print *, nx
c     print *, ny
c     print *, deltax
c     print *, deltay
c     print *, xmn
c     print *, ymn

c This assumes deltax = deltay.
      deltaxy = deltax

c The undef values used in the SnowModel simulation.
      undef = -9999.0

c Loop through the variables of interest.

c Input file.
      open (21,file=path_in//'topo_vege.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Read the data of interest.
      read (21,rec=1) ((topo(i,j),i=1,nx),j=1,ny)
      read (21,rec=2) ((vege(i,j),i=1,nx),j=1,ny)

c Save the data as .flt files.
      call flt_outputs (nx,ny,path_out,topo,varnames(1),
     &  xmn,ymn,deltaxy,undef,run_name,nx_max,ny_max)

      call flt_outputs (nx,ny,path_out,vege,varnames(2),
     &  xmn,ymn,deltaxy,undef,run_name,nx_max,ny_max)

      end

c Additional .gdat TO .flt TO .tif NOTES:

c To convert a .flt file to a .tif file using gdalwarp, two
c   files are required: The binary floating-point file (.flt)
c   and an ASCII header file with the same name as the .flt
c   file but with a .hdr file extension.  Details of this .hdr
c   file can be found on these web sites:

c http://resources.esri.com/help/9.3/arcgisengine/java/
c   gp_toolref/Conversion_Tools/float_to_raster_conversion_.htm

c http://resources.esri.com/help/9.3/arcgisengine/java/
c   gp_toolref/spatial_analyst_tools/floatgrid.htm

c https://www.coolutils.com/Formats/FLT

c https://pro.arcgis.com/en/pro-app/tool-reference/conversion/
c   float-to-raster.htm

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine flt_outputs (nx,ny,path_out,var,cvar,
     &  xmn,ymn,deltaxy,undef,run_name,nx_max,ny_max)

      implicit none

      integer i,j,nx,ny,nx_max,ny_max
      real var(nx_max,ny_max)
      double precision xmn,ymn
      real deltaxy,undef

      character*(*) path_out
      character*12 run_name
      character*21 f_name1
      character*21 f_name2
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
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

c Save the data.  Note that this has to be y-reversed.
      write (71,rec=1) ((var(i,j),i=1,nx),j=ny,1,-1)

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
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

