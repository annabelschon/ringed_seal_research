c 2_adjust_prec.f

c Here I am iteratively adjusting a precipitation correction
c   factor (or parameter), called cf_precip, until the domain-
c   average snow depth equals 1.0 m on 1 April.

      implicit none

      integer nx,ny

      parameter (nx=60,ny=41)

      real snod(nx,ny)

      integer i,j,irec

      real snod_ave_obs
      real snod_ave_mod

      real snod_ratio,snod_ratio_old
      real cf_precip
      real percent_diff
      integer iprec_flag

      real wt,ratio

      print *,'CALCULATING CONVERGENCE'
      print *

c Define the snow depth (snod) target (domain average on 1 Apr).
      snod_ave_obs = 1.0

c Read in the SnowModel snod data.
      open (21,file='../outputs/wo_assim/snod.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c I happen to know that 1 Apr is daily time record 213 (otherwise
c   we could use the calndr program to calculate it).
      irec = 213
      read (21,rec=irec) ((snod(i,j),i=1,nx),j=1,ny)

c Calculate the domain average snod.
      snod_ave_mod = 0.0
      do j=1,ny
        do i=1,nx
          snod_ave_mod = snod_ave_mod + snod(i,j)
        enddo
      enddo
      snod_ave_mod = snod_ave_mod / real(nx * ny)

c Read in the old precipitation correction values.
      open (51,file='../precip_cf/cf_precip.dat')
      open (52,file='snod_ratio_old.dat')

      read (51,*) cf_precip
      read (52,*) snod_ratio_old

c These are closed so the next time you open and write to them
c   you will write and read at the first line in the file.
      close (51)
      close (52)

c Calculate the new corrections.

c When wt = 1.0, you get the new ratio.  The smaller wt is, the
c   more relaxation you are getting (you are weighting the old
c   value more, and the system converges slower).  Some systems
c   require this "under-relaxation", some don't.
      wt = 0.75
c     wt = 1.00

      snod_ratio = snod_ave_obs / snod_ave_mod

      ratio = wt * snod_ratio + (1.0 - wt) * snod_ratio_old

      cf_precip = cf_precip * ratio

c This is the % difference between obs and mod snod_ave.
      percent_diff = 100.0 *
     &  (snod_ave_mod - snod_ave_obs) / snod_ave_obs

      if (abs(percent_diff).gt.1.0) then
        iprec_flag = 0
      else
        iprec_flag = 1
      endif

c Print the calculation results to the screen.
      write (*,91) snod_ave_obs,snod_ave_mod,
     &  snod_ratio,snod_ratio_old,ratio,percent_diff,iprec_flag

  91  format (5f8.3,f10.3,i4)

c Write the calculation results to a file.  Notice that I open
c   this file in "append" mode, so I can add to it in each model
c   run loop.  And also I zero it out at the start of the looping
c   script, so the file starts empty at the begining of a new
c   run.
      open (71,file='prec_snod_convergence_calcs.dat',access='append')
      write (71,91) snod_ave_obs,snod_ave_mod,
     &  snod_ratio,snod_ratio_old,ratio,percent_diff,iprec_flag

c Write out the new correction values.
      open (51,file='../precip_cf/cf_precip.dat')
      open (52,file='snod_ratio_old.dat')
      open (53,file='prec_flag.dat')

      write (51,*) cf_precip
      write (52,*) snod_ratio
      write (53,*) iprec_flag

c These are closed so you can open them again in the next loop.
      close (51)
      close (52)
      close (53)

      print *
      print *
      print *

      end

