c 1_merge_vege_mask.f

      implicit none

      integer nx,ny,i,j
      parameter (nx=690,ny=990)

      real vege1(nx,ny)
      real vege2(nx,ny)
      real glfm(nx,ny)

      real undef

      undef = -9999.0

      open (21,file='../../../../1_topo_vege/5_mk_final_topo_vege/'//
     &  'seals_topo_vege_500m.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      read (21,rec=2) ((vege1(i,j),i=1,nx),j=1,ny)

      open (22,file='../../../../1_topo_vege/6_mk_glac_front_mask/'//
     &  'glacier_front_mask_500m_6km.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      read (22,rec=1) ((glfm(i,j),i=1,nx),j=1,ny)

c Merge the land cover classes to just have: land, glacier, ocean,
c   and glfm.
      do j=1,ny
        do i=1,nx
          vege2(i,j) = vege1(i,j)
          if (glfm(i,j).eq.1.0) vege2(i,j) = 1.0
        enddo
      enddo

c Save the data.
      open (41,file='seals_vege_mask_500m.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      write (41,rec=1) ((vege2(i,j),i=1,nx),j=1,ny)

      end

