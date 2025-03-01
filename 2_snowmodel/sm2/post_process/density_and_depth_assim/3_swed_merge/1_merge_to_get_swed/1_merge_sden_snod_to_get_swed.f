c 1_merge_sden_snod_to_get_swed.f

      implicit none

      integer nx,ny,i,j,iter,nx_max,ny_max

      parameter (nx_max=10000,ny_max=10000)

      real sden(nx_max,ny_max)
      real snod(nx_max,ny_max)
      real swed(nx_max,ny_max)

      integer iyear_init,imonth_init,iday_init,ndays,max_iter
      real undef,ro_water,print_inc

      character*120 path
      integer trailing_blanks,i_len

      undef = -9999.0
      ro_water = 1000.0

c Provide the required input information.  This information comes
c   out of the snowmodel.par file.
      open (61,file=
     &  '../../1_sden_assim/2_sden_assim/snowmodel_info.dat')

      read (61,*) nx
      read (61,*) ny
      read (61,*)
      read (61,*)
      read (61,*)
      read (61,*)
      read (61,*)
      read (61,*) iyear_init
      read (61,*) imonth_init
      read (61,*) iday_init
      read (61,*) max_iter
      read (61,*)
      read (61,*) print_inc

c Calculate the number of records (ndays, in this case) in the
c   data files.
      ndays = max_iter / nint(print_inc)

c Define the path where the assimilated output data are located.
      open (71,file=
     &  '../../1_sden_assim/2_sden_assim/0_input_output_paths.dat')
      read (71,*)
      read (71,*)
      read (71,98) path
   98 format (a120)
      i_len = 120 - trailing_blanks(path)

c This is the clipped path.
c     path(1:i_len)

c Open the input files.
      open (21,file=path(1:i_len)//'snod_assim.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

      open (22,file=path(1:i_len)//'sden_assim.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Open the output file.
      open (31,file=path(1:i_len)//'swed_assim.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny,
     &  status='replace')

      do iter=1,ndays

        if (mod(iter,100).eq.0) print *, 'iter =',iter

        read (21,rec=iter) ((snod(i,j),i=1,nx),j=1,ny)
        read (22,rec=iter) ((sden(i,j),i=1,nx),j=1,ny)

c Use the new snod to create new swed from the new snod data.
        do j=1,ny
          do i=1,nx
            if (sden(i,j).ne.undef) then
              swed(i,j) = sden(i,j) / ro_water * snod(i,j)
            else
              swed(i,j) = 0.0
            endif
          enddo
        enddo

c Save the data.
        write (31,rec=iter) ((swed(i,j),i=1,nx),j=1,ny)

      enddo

c Make the corresponding .ctl file.  This is assuming daily data
c   starting on 1 Sep.
      call make_swed_ctl_file (nx,ny,ndays,iyear_init,
     &  path(1:i_len))

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_swed_ctl_file (nx,ny,ndays,iyr_start,
     &  path)

      implicit none

      integer nx,ny,ndays,iyr_start,nvars,nvar

      parameter (nvars=1)

      character*60 var_string(nvars)
      character*(*) path

c Define the variable names that correspond to the .gdat file
c   writes.  I list them in a column here so it better corresponds
c   to the .gdat writes listed below; these must be listed in the
c   same order as the data writes, and they must be in single
c   quotes, and they cannot be more than 15 characters long.
      data var_string /
     &  'swed     0  0 swed depth (m)'/

      open (71,file='swed_assim.ctl')

      write (71,51) path
      write (71,52)
      write (71,53)
      write (71,54) nx
      write (71,55) ny

      write (71,56)
      write (71,57) ndays,iyr_start
      write (71,58) nvars

      do nvar=1,nvars
        write (71,100) var_string(nvar)
      enddo

      write (71,68)

      close (71)

   51 format ('DSET ',a,'swed_assim.gdat')
   52 format ('TITLE xxxxxxxxxxxxxxxxxxxxxxxxxx')
   53 format ('UNDEF -9999.0')

   54 format ('XDEF ',i8,' LINEAR  1.0  1.0')
   55 format ('YDEF ',i8,' LINEAR  1.0  1.0')
   56 format ('ZDEF        1 LINEAR  1.0  1.0')

   57 format ('TDEF ',i8,' LINEAR  01SEP',i4,' 1dy')

   58 format ('VARS     ',i4)

  100 format (a)

   68 format ('ENDVARS')

      return
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

