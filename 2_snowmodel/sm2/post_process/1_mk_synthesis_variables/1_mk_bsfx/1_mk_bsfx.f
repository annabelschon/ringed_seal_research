c 1_mk_bsfx.f
c This version is adapted from 1_merge_bs_fluxes.f, which merges var3
c      var4 to create the blowing snow flux in a post-processing step.
c      This SM run already merged var3 and var4 during the run, so only
c      the new bsfx.gdat is created here.

      implicit none

      integer nx,ny,i,j,maxiter,iter

      parameter (nx=304,ny=448)

      parameter (maxiter=125640)

      real var3(nx,ny),bsfx(nx,ny)

      real sec_in_day,ro_snow,undef

      character path*(*) 
      parameter (path =
c    &  '/home/gliston/zdata6/seals/sm/outputs/')
     &  '/data3/annabel/seals/3_outputs/wo_assim/')

c Constants.
      sec_in_day = 86400.0
      ro_snow = 330.0
      undef = -9999.0

c Input files.
      open (21,file=path//'var3.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c     open (22,file=path//'var4.gdat',
c    &  form='unformatted',access='direct',recl=4*nx*ny)

c Output files.
      open (31,file=path//'bsfx.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

c Merge the salt and susp data and convert from kg m-1 s-1, to
c   volume per unit width.
      do iter=1,maxiter

        read (21,rec=iter) ((var3(i,j),i=1,nx),j=1,ny)
c       read (22,rec=iter) ((var4(i,j),i=1,nx),j=1,ny)

        do j=1,ny
          do i=1,nx
            if (var3(i,j).eq.undef) then
              bsfx(i,j) = undef
c           else
c             bsfx(i,j) = var3(i,j) + var4(i,j)
c             bsfx(i,j) = sec_in_day * bsfx(i,j) / ro_snow
            endif
          enddo
        enddo

        write (31,rec=iter) ((bsfx(i,j),i=1,nx),j=1,ny)

      enddo

      end

