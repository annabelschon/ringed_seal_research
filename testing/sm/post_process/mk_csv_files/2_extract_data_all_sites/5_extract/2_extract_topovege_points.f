c extract_sm_points.f

c Read SnowModel .gdat file outputs, and output time series at
c   specific grid cells, for different variables.

      implicit none

      integer nx,ny,npts,nt,nvars

c Number of points in the SnowModel grid.
      parameter (nx=232,ny=448)

c Number of time writes.
      parameter (nt=1)

c Number of grid cells to extract.
      parameter (npts=73)

c Number of variables you are going to extract and save.
      parameter (nvars=2)

      real topo(nx,ny)
      real vege(nx,ny)
c     real sden(nx,ny)
c     real swed(nx,ny)

      real topo_gdat(npts)
      real vege_gdat(npts)
c     real sden_gdat(npts)
c     real swed_gdat(npts)

      integer ii(npts),jj(npts)

      integer i,j,k,iter

c Define the path where the ij location information is located.
      character path1*(*)
      parameter (path1=
     &  '../3_mk_stn_ij/')

c Define the path where the SnowModel output data are located.
      character path2*(*) 
      parameter (path2=
     &  '/home/shp/space_hare/run_2/sm_3/topo_vege/NoAm_30m/')

c Open a .gdat output output file.
      open (unit=81,file='point_topovege.gdat',
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
c     open (unit=21,file=path2//'topo_vege.gdat',
      open (unit=21,file=path2//'topovege_new.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c     open (unit=22,file=path2//'snod.gdat',
c    &  form='unformatted',access='direct',recl=4*nx*ny)

c     open (unit=23,file=path2//'sden.gdat',
c    &  form='unformatted',access='direct',recl=4*nx*ny)

c     open (unit=24,file=path2//'swed.gdat',
c    &  form='unformatted',access='direct',recl=4*nx*ny)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do iter=1,nt

        if (mod(iter,100).eq.0) print *, 'working on iter',iter

c Read in the data.
        read (21,rec=1) ((topo(i,j),i=1,nx),j=1,ny)
        read (21,rec=2) ((vege(i,j),i=1,nx),j=1,ny)
c       read (23,rec=iter) ((sden(i,j),i=1,nx),j=1,ny)
c       read (24,rec=iter) ((swed(i,j),i=1,nx),j=1,ny)

c Extract the point data.
        do k=1,npts
          topo_gdat(k) = topo(ii(k),jj(k))
          vege_gdat(k) = vege(ii(k),jj(k))
c         print *,k,'t ',topo(ii(k),jj(k))
          print *,k,'v ',vege(ii(k),jj(k))
c         sden_gdat(k) = sden(ii(k),jj(k))
c         swed_gdat(k) = swed(ii(k),jj(k))
        enddo

c Save the data.  Note that here k=1,npts is the "x" dimension
c   in the .ctl file.
        write (81,rec=iter)
     &    (topo_gdat(k),k=1,npts),
     &    (vege_gdat(k),k=1,npts)
c    &    (sden_gdat(k),k=1,npts),
c    &    (swed_gdat(k),k=1,npts)

      enddo

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

