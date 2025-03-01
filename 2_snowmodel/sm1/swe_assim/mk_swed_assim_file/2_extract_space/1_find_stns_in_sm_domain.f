c 1_find_stns_in_sm_domain.f

c This program is looking for stations within your SnowModel
c   domain.

      implicit none

      integer nstns,nx,ny,nfiles,k,icount,nfiles_max,nsnotel

      parameter (nfiles_max=5000)

      double precision xmin,xmax,ymin,ymax,deltax,deltay,xmn,ymn
      real glat,glon

      double precision x(nfiles_max),y(nfiles_max)
      real elev(nfiles_max)

      integer iflag(nfiles_max),id1(nfiles_max),id2(nfiles_max),
     &  id3(nfiles_max)

      character*4 state(nfiles_max)
      character*8 id(nfiles_max)

      character*120 path2
      integer trailing_blanks,i_len

c Define the path where the topo-vege grid information is located.
      character path1*(*)
      parameter (path1='../../../topo_vege/NoAm_30m/')

c Read in the SNOTEL path.
      open (21,file='../0_define_run_info/snotel_archive_path.dat')
      read (21,94) path2
   94 format (a120)
      i_len = 120 - trailing_blanks(path2)

c Read in the number of stations.
      open (61,file=
     &  path2(1:i_len)//'6_stn_table/total_number_of_stations.dat')
      read (61,*) nfiles

c Read in the station info data.
      open (41,file=
     &  path2(1:i_len)//'6_stn_table/stn_info_table.dat')
      do k=1,nfiles
        read (41,91)
     &    id1(k),id2(k),id3(k),state(k),id(k),glat,glon,elev(k)
      enddo

  91  format (3i6,4x,a4,a8,2f12.5,f9.1)

c Read in the SnowModel grid information from the topo-vege
c   processing file.
      open (31,file=path1//'SM_domain_config_info_OUTPUTS.dat')

      read (31,101) nx
      read (31,101) ny
      read (31,102) deltax
      read (31,102) deltay
      read (31,103) xmn
      read (31,103) ymn

  101 format (9x,i8)
  102 format (9x,f8.1)
  103 format (9x,f12.2)

c     print *,nx
c     print *,ny
c     print *,deltax
c     print *,deltay
c     print *,xmn
c     print *,ymn

c Calculate the SM domain boundaries.
      xmin = xmn - 0.5 * deltax
      xmax = xmin + real(nx) * deltax
      ymin = ymn - 0.5 * deltay
      ymax = ymin + real(ny) * deltay

c Read in the projected coordinate data.
      open (21,file='../1_sm_projection/snotel_stns_proj.dat')
      do k=1,nfiles
        read (21,*) x(k),y(k)
      enddo

c Loop through the station coords and flag the stations that are
c   within your SnowModel spatial domain.
      nstns = 0
      do k=1,nfiles
        iflag(k) = 0
        if (x(k).le.xmax .and. x(k).ge.xmin .and. y(k).le.ymax .and.
     &    y(k).ge.ymin) then
          nstns = nstns + 1
          iflag(k) = 1
        endif
      enddo

      print *
      print *, 'nstns found = ', nstns
      print *

c Save the data.
      open (51,file='stations_in_your_sm_domain.dat')
      icount = 0
      nsnotel = 0
      do k=1,nfiles
        if (iflag(k).eq.1) then
          icount = icount + 1
c         print *, icount,k,id1(k),id2(k),id3(k),state(k),id(k)
          if (id3(k).eq.1) nsnotel = nsnotel + 1
          write (51,92)
     &      id1(k),id2(k),id3(k),state(k),id(k),x(k),y(k),elev(k)
        endif
      enddo

  92  format (3i6,4x,a4,a8,2f12.1,f10.1)

      open (52,file='n_stations.dat')
      write (52,93) nstns

      open (53,file='n_snotel_stations.dat')
      write (53,93) nsnotel

  93  format (i8)

      open (54,file='SM_info.dat')
      write (54,93) nx
      write (54,93) ny
      write (54,95) deltax
      write (54,95) deltay
      write (54,95) xmn
      write (54,95) ymn

  95  format (f12.2)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer function trailing_blanks(input_string)

      implicit none

      integer k
      character*(*) input_string

c Count the number of blanks following the last non-blank
c   character.
      trailing_blanks = 0
      do k=len(input_string),1,-1
        if (input_string(k:k).eq.' ') then
          trailing_blanks = trailing_blanks + 1
        else
          return
        endif
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

