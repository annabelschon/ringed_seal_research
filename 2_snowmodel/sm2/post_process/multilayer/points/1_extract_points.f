c 1_extract_points.f

c This just pulls out a point of interest and saves the data
c   for processing.

c This should be modified so it extracts more than one point.

      implicit none

      integer nx,ny,nz_max,i,j,k,iter,max_iter

      parameter (nx=83,ny=106)
      parameter (nz_max=26)
      parameter (max_iter=2184)

      real xKK(nx,ny,max_iter)
      real snow_depth(nx,ny,max_iter)
      real xro_snow(nx,ny,max_iter)
      real swe_depth(nx,ny,max_iter)
      real snod_layer(nx,ny,nz_max,max_iter)
      real ro_layer(nx,ny,nz_max,max_iter)
      real swed_layer(nx,ny,nz_max,max_iter)
      real diam(nx,ny,nz_max,max_iter)

      integer ii,jj
      data ii/48/
      data jj/50/

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      open (21,file='../../outputs/multilayer.gdat',
     &  form='unformatted',access='direct',
     &  recl=4*(4*nx*ny+4*nx*ny*nz_max))

      open (31,file='multilayer_pts.gdat',
     &  form='unformatted',access='direct',recl=4*(4+4*nz_max),
     &  status='replace')

c Read in the data.
      print *,'reading in the data'
      do iter=1,max_iter
        read(21,rec=iter)
     &    ((xKK(i,j,iter),i=1,nx),j=1,ny),
     &    ((snow_depth(i,j,iter),i=1,nx),j=1,ny),
     &    ((xro_snow(i,j,iter),i=1,nx),j=1,ny),
     &    ((swe_depth(i,j,iter),i=1,nx),j=1,ny),
     &    (((snod_layer(i,j,k,iter),i=1,nx),j=1,ny),k=1,nz_max),
     &    (((ro_layer(i,j,k,iter),i=1,nx),j=1,ny),k=1,nz_max),
     &    (((swed_layer(i,j,k,iter),i=1,nx),j=1,ny),k=1,nz_max),
     &    (((diam(i,j,k,iter),i=1,nx),j=1,ny),k=1,nz_max)
      enddo

c Extract and write out the point of interest.
      i = ii
      j = jj
      do iter=1,max_iter
        write (31,rec=iter)
     &    xKK(i,j,iter),
     &    snow_depth(i,j,iter),
     &    xro_snow(i,j,iter),
     &    swe_depth(i,j,iter),
     &    (snod_layer(i,j,k,iter),k=1,nz_max),
     &    (ro_layer(i,j,k,iter),k=1,nz_max),
     &    (swed_layer(i,j,k,iter),k=1,nz_max),
     &    (diam(i,j,k,iter),k=1,nz_max)
      enddo

      end

