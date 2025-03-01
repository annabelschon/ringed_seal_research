c 1_mk_pan_arctic_topo_vege.f

      implicit none

      integer nx,ny,i,j

c This is the domain of interest.
      parameter (nx=304,ny=448)

c This is the input path.
      character path2*(*)
      parameter (path2 =
     &  '/data3/annabel/seals/1_nsidc_0051/4_gdat_extracted_years/')

      real mask(nx,ny)
      real vege(nx,ny)
      real topo(nx,ny)

      open (21,file=path2//'ice_conc_mask.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c mask  0  0   Mask values: 1 = Land, 2 = Coast, 3 = Ocean
      read (21,rec=1) ((mask(i,j),i=1,nx),j=1,ny)

c Make this into a SnowModel topo_vege.gdat file:
c   18 = Land, 24 = Ocean.
      do j=1,ny
        do i=1,nx

          if (mask(i,j) .eq. 1.0) then
            vege(i,j) = 18.0
          elseif (mask(i,j) .eq. 2.0) then
            vege(i,j) = 24.0
          elseif (mask(i,j) .eq. 3.0) then
            vege(i,j) = 24.0
          else
            print *, 'strange value found',i,j,mask(i,j)
          endif

          if (vege(i,j).eq.18.0) then
            topo(i,j) = 1.0
          else
            topo(i,j) = 0.0
          endif

        enddo
      enddo

      open (41,file='topo_vege.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      write (41,rec=1) ((topo(i,j),i=1,nx),j=1,ny)
      write (41,rec=2) ((vege(i,j),i=1,nx),j=1,ny)

      end

