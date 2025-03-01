c 1_mk_ij_obs_coords.f

c Calculate the i,j coords in the SnowModel simulation domain.

      implicit none

      integer nstns,k

      parameter (nstns=73)

      integer ii(nstns),jj(nstns)

      double precision xstn,ystn,xmn,ymn,deltax,deltay

c Define the path where the topo-vege grid information is located.
      character path*(*)
      parameter (path='../../sm_2/topo_vege/NoAm_30m/')

c Read in the SnowModel grid information from the topo-vege
c   processing file.
      open (31,file=path//'SM_domain_config_info_OUTPUTS.dat')

      read (31,101)
      read (31,101)
      read (31,102) deltax
      read (31,102) deltay
      read (31,103) xmn
      read (31,103) ymn

  101 format (9x,i8)
  102 format (9x,f8.1)
  103 format (9x,f12.2)

c     print *,deltax
c     print *,deltay
c     print *,xmn
c     print *,ymn

c Save the (i,j) station info.
      open (41,file='stn_ij_coords_col.dat')

c Read in the station data and calculate the i,j coords.
      open (21,file=
     &  '../2_ll_to_proj/stns_xy_proj.dat')

      do k=1,nstns
        read (21,*) xstn,ystn

c Convert the x and y locations to (ii,jj) locations.
        ii(k) = 1 + nint((xstn - xmn) / deltax)
        jj(k) = 1 + nint((ystn - ymn) / deltay)
      enddo

c Write out in column format.
      do k=1,nstns
        write (41,91) ii(k),jj(k)
      enddo

  91  format (2i6)

      end

