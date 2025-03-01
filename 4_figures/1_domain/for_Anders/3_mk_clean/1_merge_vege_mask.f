c 1_merge_vege_mask.f

      implicit none

      integer nx,ny,i,j,ny2
      parameter (nx=375,ny=275,ny2=236)

      real vege1(nx,ny)
      real vege2(nx,ny)
      real vege3(nx,ny)
      real vege4(nx,ny2)
      real glfm(nx,ny)

      real undef

      undef = -9999.0

      open (21,file=
     &  '../../../../../2020_work/1_topo_vege/6_final_topo_vege/'//
     &  'seals_topo_vege_100m.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      read (21,rec=2) ((vege1(i,j),i=1,nx),j=1,ny)

      open (22,file=
     &  '../../../../../2020_work/1_topo_vege/8_mk_glac_front_mask/'//
     &  'seals_gfront_mask_3km.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      read (22,rec=1) ((glfm(i,j),i=1,nx),j=1,ny)

c Merge the land cover classes to just have: land, glacier, ocean.
      do j=1,ny
        do i=1,nx
          vege2(i,j) = undef

          if (vege1(i,j).eq.14.0) then
            vege2(i,j) = 18.0

          elseif (vege1(i,j).eq.15.0) then
            vege2(i,j) = 18.0

          elseif (vege1(i,j).eq.17.0) then
            vege2(i,j) = 18.0

          elseif (vege1(i,j).eq.18.0) then
            vege2(i,j) = 18.0

          elseif (vege1(i,j).eq.19.0) then
            vege2(i,j) = 18.0

          elseif (vege1(i,j).eq.20.0) then
            vege2(i,j) = 20.0

          elseif (vege1(i,j).eq.24.0) then
            vege2(i,j) = 24.0

          elseif (vege1(i,j).eq.25.0) then
            vege2(i,j) = 18.0

          elseif (vege1(i,j).eq.26.0) then
            vege2(i,j) = 18.0

          else
            print *,'extra veg value found  ',i,j,vege1(i,j)
          endif

        enddo
      enddo

c Merge the land cover classes to just have: land, glacier, ocean,
c   and glfm.
      do j=1,ny
        do i=1,nx
          vege3(i,j) = vege2(i,j)
          if (glfm(i,j).eq.1.0) vege3(i,j) = 1.0
        enddo
      enddo

c Extract the plotted domain.
      do j=1,ny2
        do i=1,nx
          vege4(i,j) = vege3(i,j+29)
        enddo
      enddo

c Save the data.
      open (41,file='seals_vege_mask_100m.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny2,
     &  status='replace')

      write (41,rec=1) ((vege4(i,j),i=1,nx),j=1,ny2)

      end

