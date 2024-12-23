c 1_extract_years_of_interest.f

      implicit none

      integer nx,ny,maxiter,i,j,ioffset,iter

      parameter (nx=304,ny=448)

      real conc(nx,ny)

c Input path.
      character path1*(*)
      parameter (path1 =
     &  '/data3/annabel/seals/1_nsidc_0051/3_gdat_final/')

c Output path.
      character path2*(*)
      parameter (path2 =
     &  '/data3/annabel/seals/1_nsidc_0051/4_gdat_extracted_years/')

c Define the temporal subdomain you want to extract.
      maxiter = 15705
      ioffset = 213

c Open the input files.
      open (unit=21,file=path1//'ice_conc_v3.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)


c Open the output file.
      open (31,file=path2//'ice_conc_extracted_domain.gdat',
     &  form='unformatted',access='direct',recl=4*1*nx*ny,
     &  status='replace')

c Run the data extraction and write procedure.
      do iter=1,maxiter

        read (21,rec=iter+ioffset) ((conc(i,j),i=1,nx),j=1,ny)

c Scale the data by 0.4 to get percentage (0-100) sea ice concentration
        do j=1,ny
          do i=1, nx
c            if (conc(i,j) .lt. 251.0) then
              conc(i,j)=conc(i,j) * 0.4
c            else
c              conc(i,j) = conc(i,j)
c            endif
          enddo
        enddo
        
        write (31,rec=iter)
     &    ((conc(i,j),i=1,nx),j=1,ny)
      
      enddo

      end

