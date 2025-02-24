c 1_mk_stn_ll.f

      implicit none

      integer nstns,k,isite,id

      parameter (nstns=73)

      real glat,glon

      open (21,file=
c    &  '../1_orig/Olnes_2018_sites_latlon_clean_final.csv')
     &  '../1_orig/spacehares_all_wiseman_plot_coords_clean.csv')

c read past header.
      read (21,*)

c Output files.
      open (51,file='stn_ll.dat')

      do k=1,nstns
c       read (21,*) isite,id,glon,glat
        read (21,*) glon,glat
c       print *,k,isite,id,glon,glat

c Also save a file of just the lon-lat values.
        write (51,91) glon,glat
      enddo

  91  format (2f16.8)

      end

