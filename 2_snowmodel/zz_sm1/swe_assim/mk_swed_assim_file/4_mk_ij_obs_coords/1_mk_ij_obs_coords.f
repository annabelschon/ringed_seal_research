c 1_mk_ij_obs_coords.f

c Calculate the i,j coords in the SnowModel simulation domain.

      implicit none

      integer id1,id2,id3,nstns,k,nstns_max
      
      double precision xstn,ystn,xmn,ymn,deltax,deltay
      real elev,undef

      parameter (nstns_max=5000)

      integer ii(nstns_max),jj(nstns_max)
      integer iii(nstns_max),jjj(nstns_max)

      integer number(nstns_max)

      character*4 state
      character*8 id(nstns_max)
      character*40 form

c Define the path where the topo-vege grid information is located.
      character path*(*)
      parameter (path='../../../topo_vege/NoAm_30m/')

      undef = -9999.0

c Read in the number of stations in your SnowModel domain.
      open (71,file='../2_extract_space/n_stations.dat')
      read (71,*) nstns

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
      open (41,file='stn_ij_coords_row.dat')
      open (42,file='stn_ij_coords_col.dat')

c Read in the station data and calculate the i,j coords.
      open (21,file=
     &  '../2_extract_space/stations_in_your_sm_domain.dat')

      do k=1,nstns
        read (21,91) id1,id2,id3,state,id(k),xstn,ystn,elev

c Convert the x and y locations to (ii,jj) locations.
        ii(k) = 1 + nint((xstn - xmn) / deltax)
        jj(k) = 1 + nint((ystn - ymn) / deltay)

      enddo

c Now build (i,j) arrays that identify stations with valid data
c   in them for this model runs time and space domains.
      open (51,file='../3_extract_time/stations_w-valid_data.dat')

      do k=1,nstns
        read (51,*) number(k)
      enddo

      do k=1,nstns
        if (number(k).ne.nint(undef)) then
          iii(k) = ii(number(k))
          jjj(k) = jj(number(k))
        else
          iii(k) = nint(undef)
          jjj(k) = nint(undef)
        endif
      enddo

      write (form,93) nstns

      write (41,form) (iii(k),k=1,nstns)
      write (41,form) (jjj(k),k=1,nstns)

  91  format (3i6,4x,a4,a8,2f12.1,f10.1)
  93  format ('(',i5,'i6)')

c Write out in column format.
      do k=1,nstns
        write (42,92) k,iii(k),jjj(k),id(k)
      enddo

  92  format (3i6,a8)

      end

