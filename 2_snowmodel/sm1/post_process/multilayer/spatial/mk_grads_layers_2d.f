c mk_grads_layers_2d.f

c Make the snow layers for use in grads.

      implicit none

      integer nx,ny,nz_max,i,j,k,iter,max_iter

      parameter (nx=83,ny=106)
      parameter (nz_max=26)
      parameter (max_iter=2184)

      real xKK(nx,ny,max_iter)
      real snow_depth(nx,ny,max_iter)
      real xro_snow(nx,ny)
      real swe_depth(nx,ny)
      real snod_layer(nx,ny,nz_max,max_iter)
      real ro_layer(nx,ny,nz_max,max_iter)
      real swed_layer(nx,ny,nz_max)
      real diam(nx,ny,nz_max)

      real snow_wall(nx,ny,nz_max+1,max_iter)

      real undef
      integer KK

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Input the following from the snowmodel.par file.
      undef = -9999.0

c Read in the layer data and create some additional output arrays
c   for plotting purposes.

      open (21,file='snowlayers.gdat',
     &  form='unformatted',access='direct',recl=4*(nx*ny+nx*ny*nz_max))

      open (31,file='../../outputs/multilayer.gdat',
     &  form='unformatted',access='direct',
     &  recl=4*(4*nx*ny+4*nx*ny*nz_max))

      do iter=1,max_iter

c Read in the data.
        read(31,rec=iter)
     &    ((xKK(i,j,iter),i=1,nx),j=1,ny),
     &    ((snow_depth(i,j,iter),i=1,nx),j=1,ny),
     &    ((xro_snow(i,j),i=1,nx),j=1,ny),
     &    ((swe_depth(i,j),i=1,nx),j=1,ny),
     &    (((snod_layer(i,j,k,iter),i=1,nx),j=1,ny),k=1,nz_max),
     &    (((ro_layer(i,j,k,iter),i=1,nx),j=1,ny),k=1,nz_max),
     &    (((swed_layer(i,j,k),i=1,nx),j=1,ny),k=1,nz_max),
     &    (((diam(i,j,k),i=1,nx),j=1,ny),k=1,nz_max)
      enddo

c Compute the locations of the snowpack c.v. walls.
      do iter=1,max_iter
        do j=1,ny
          do i=1,nx

c Initialize the entire domain.
            do k=1,nz_max
              snow_wall(i,j,k,iter) = undef
            enddo

c Initialize the bottom of the cv.
            snow_wall(i,j,1,iter) = 0.0

c Extract the top grid cell that had snow in it.
            KK = nint(xKK(i,j,iter))

c Loop through the cv's, adding up all of the dy's until you
c   reach the top of the snowpack.
            do k=2,KK+1
              snow_wall(i,j,k,iter) = snow_wall(i,j,k-1,iter) +
     &          snod_layer(i,j,k-1,iter)
            enddo
          enddo
        enddo
      enddo

c Save the data.
      do iter=1,max_iter
        write (21,rec=iter)
     &    ((snow_depth(i,j,iter),i=1,nx),j=1,ny),
     &    (((snow_wall(i,j,k,iter),i=1,nx),j=1,ny),k=1,nz_max)
      enddo

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

