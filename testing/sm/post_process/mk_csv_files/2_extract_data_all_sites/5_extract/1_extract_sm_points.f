c extract_sm_points.f

c Read SnowModel .gdat file outputs, and output time series at
c   specific grid cells, for different variables.

      implicit none

      integer nx,ny,npts,nt,nvars

c Number of points in the SnowModel grid.
      parameter (nx=232,ny=448)

c Number of time writes.
c     parameter (nt=15705)
      parameter (nt=4748)

c Number of grid cells to extract.
      parameter (npts=73)

c Number of variables you are going to extract and save.
      parameter (nvars=5)

      real tair(nx,ny)
      real relh(nx,ny)
      real snod(nx,ny)
      real sden(nx,ny)
      real swed(nx,ny)

      real tair_gdat(npts)
      real relh_gdat(npts)
      real snod_gdat(npts)
      real sden_gdat(npts)
      real swed_gdat(npts)

      integer ii(npts),jj(npts)

      integer i,j,k,iter

c Define the path where the ij location information is located.
      character path1*(*)
      parameter (path1=
     &  '../3_mk_stn_ij/')

c Define the path where the SnowModel output data are located.
      character path2*(*) 
      parameter (path2=
     &  '/data4/space_hares/sm_4/outputs/wi_assim/')

      character path3*(*) 
      parameter (path3=
     &  '/data4/space_hares/sm_4/assim_data/')

c Open a .gdat output output file.
      open (unit=81,file='point_variables.gdat',
     &  form='unformatted',access='direct',recl=4*nvars*npts,
     &  status='replace')

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Read the grid-point coordinate data.
      open (41,file=path1//'stn_ij_coords_col.dat')
      do k=1,npts
        read (41,*) ii(k),jj(k)
      enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Open the SnowModel output files.
      open (unit=21,file=path2//'tair.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (unit=22,file=path2//'relh.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (unit=23,file=path3//'snod_assim.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (unit=24,file=path3//'sden_assim.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (unit=25,file=path3//'swed_assim.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do iter=1,nt

        if (mod(iter,100).eq.0) print *, 'working on iter',iter

c Read in the data.
        read (21,rec=iter) ((tair(i,j),i=1,nx),j=1,ny)
        read (22,rec=iter) ((relh(i,j),i=1,nx),j=1,ny)
        read (23,rec=iter) ((snod(i,j),i=1,nx),j=1,ny)
        read (24,rec=iter) ((sden(i,j),i=1,nx),j=1,ny)
        read (25,rec=iter) ((swed(i,j),i=1,nx),j=1,ny)

c Extract the point data.
        do k=1,npts
          tair_gdat(k) = tair(ii(k),jj(k))
          relh_gdat(k) = relh(ii(k),jj(k))
          snod_gdat(k) = snod(ii(k),jj(k))
          sden_gdat(k) = sden(ii(k),jj(k))
          swed_gdat(k) = swed(ii(k),jj(k))
        enddo

c Save the data.  Note that here k=1,npts is the "x" dimension
c   in the .ctl file.
        write (81,rec=iter)
     &    (tair_gdat(k),k=1,npts),
     &    (relh_gdat(k),k=1,npts),
     &    (snod_gdat(k),k=1,npts),
     &    (sden_gdat(k),k=1,npts),
     &    (swed_gdat(k),k=1,npts)

      enddo

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

