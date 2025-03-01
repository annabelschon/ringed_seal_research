c 3_den_assim_layers_points.f

c Here I am taking the density assimilation adjustment ratio and
c   applying it to the multilayer snowpack profile.

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

      real undef,ratio_density
      integer KKK

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Input the following from the snowmodel.par file.
      undef = -9999.0

c Read in the layer data and create some additional output arrays
c   for plotting purposes.

c Input file.
      open (21,file='multilayer_pts.gdat',
     &  form='unformatted',access='direct',recl=4*(4+4*nz_max))

c Output files.
      open (31,file='snowlayers_assim_pts.gdat',
     &  form='unformatted',access='direct',recl=4*(1+nz_max),
     &  status='replace')

      open (32,file='density_hires_assim_pts.gdat',
     &  form='unformatted',access='direct',recl=4*nnz,
     &  status='replace')

c Read in the density assimilation adjustment factor (ratio).  If
c   there was not density assimilation, then there is not adjustment
c   (ratio_density = 1.0).
      ratio_density = 1.0
      open (22,file='../../density_assim/data/density_data_notes.txt')
      read (22,*)
      read (22,*)
      read (22,*)
      read (22,99) ratio_density
c     print *, ratio_density
 99   format (22x,f10.6)

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

c Adjust the top snow depth by the snow density assimilation
c   ratio.
        snow_depth(iter) = snow_depth(iter) / ratio_density

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
     &      snod_layer(k-1,iter) / ratio_density
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
            denplot(kk,iter) = ratio_density * ro_layer(k,iter)
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

