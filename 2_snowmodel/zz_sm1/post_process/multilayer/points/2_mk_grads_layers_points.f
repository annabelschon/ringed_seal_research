c 2_mk_grads_layers_points.f

c Note: This will not work perfectly if the snow column in
c   question ever had a layer reduction due to a layer being
c   thinner than dz_snow_min and due to exceeding max_layers
c   (from the .par file), during the same time step.  This is
c   usually only a problem if dz_snow_max is large.

      implicit none

      integer nz_max,k,iter,kstart,kend,kk,max_iter,nnz

      parameter (nz_max=26)
      parameter (max_iter=2184)
      parameter (nnz=nz_max*100)

      real xKK(max_iter)
      real snow_depth(max_iter)
      real xro_snow(max_iter)
      real swe_depth(max_iter)

      real snod_layer(nz_max,max_iter)
      real ro_layer(nz_max,max_iter)
      real swed_layer(nz_max,max_iter)
      real diam(nz_max,max_iter)

      real snow_wall(nz_max+1,max_iter)

      real denplot(nnz,max_iter)

      real undef
      integer KKK

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Input the following from the snowmodel.par file.
      undef = -9999.0

c Read in the layer data and create some additional output arrays
c   for plotting purposes.
      open (21,file='multilayer_pts.gdat',
     &  form='unformatted',access='direct',recl=4*(4+4*nz_max))

      open (31,file='snowlayers_pts.gdat',
     &  form='unformatted',access='direct',recl=4*(1+nz_max),
     &  status='replace')

      open (32,file='density_hires_pts.gdat',
     &  form='unformatted',access='direct',recl=4*nnz,
     &  status='replace')

      do iter=1,max_iter

c Read in the data.
        read(21,rec=iter)
     &    xKK(iter),
     &    snow_depth(iter),
     &    xro_snow(iter),
     &    swe_depth(iter),
     &    (snod_layer(k,iter),k=1,nz_max),
     &    (ro_layer(k,iter),k=1,nz_max),
     &    (swed_layer(k,iter),k=1,nz_max),
     &    (diam(k,iter),k=1,nz_max)
      enddo

c Compute the locations of the snowpack c.v. walls.
      do iter=1,max_iter

c Initialize the grid-cell walls.
        do k=1,nz_max+1
          snow_wall(k,iter) = undef
        enddo

c Initialize the bottom of the cv.
        snow_wall(1,iter) = 0.0

c Extract the top grid cell that had snow in it.
        KKK = nint(xKK(iter))

c Loop through the cv's, adding up all of the dy's until you
c   reach the top of the snowpack.
        do k=2,KKK+1
          snow_wall(k,iter) = snow_wall(k-1,iter) +
     &      snod_layer(k-1,iter)
        enddo
      enddo

c Save the data.
      do iter=1,max_iter
        write (31,rec=iter)
     &    snow_depth(iter),
     &    (snow_wall(k,iter),k=2,nz_max+1)
      enddo

c Create some arrays that allow plotting things like snow temperature
c   and density.
      do iter=1,max_iter
        do k=1,nnz
          denplot(k,iter) = undef
        enddo
        KKK = nint(xKK(iter))
        do k=1,KKK
          kstart = 1 + nint(100.0 * snow_wall(k,iter))
          kend = nint(100.0 * snow_wall(k+1,iter))
          do kk=kstart,kend
            denplot(kk,iter) = ro_layer(k,iter)
          enddo
        enddo
      enddo

c Save the data.
      do iter=1,max_iter
        write (32,rec=iter)
     &    (denplot(k,iter),k=1,nnz)
      enddo

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

