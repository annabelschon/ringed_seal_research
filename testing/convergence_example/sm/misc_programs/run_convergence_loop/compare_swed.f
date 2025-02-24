c compare_swed.f

      implicit none

      integer nx,ny,i,j,iter,ii,jj

      parameter (nx=3,ny=3)

      real swed(nx,ny)
      real swed_obs,swed_mod,ratio,cf_precip,percent_diff

      real swed_obs1,swed_obs2,swed_obs3
      real swed_mod1,swed_mod2,swed_mod3

      print *,'IN ASSIM LOOP'
      print *,'IN ASSIM LOOP'
      print *,'IN ASSIM LOOP'

c Open the SnowModel input file.
      open (21,file='../outputs/wo_assim/swed.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Read in the 2010 3 5 SnowModel swed data.
      iter = 5928
      read (21,rec=iter) ((swed(i,j),i=1,nx),j=1,ny)
      swed_mod1 = swed(2,2)

c Read in the 2010 3 24 SnowModel swed data.
      iter = 6384
      read (21,rec=iter) ((swed(i,j),i=1,nx),j=1,ny)
      swed_mod2 = swed(2,2)

c Read in the 2010 4 2 SnowModel swed data.
      iter = 6600
      read (21,rec=iter) ((swed(i,j),i=1,nx),j=1,ny)
      swed_mod3 = swed(2,2)

c Define the obs of interest.
      swed_obs1 = 0.097
      swed_obs2 = 0.111
      swed_obs3 = 0.113

      print *,swed_obs1,swed_obs2,swed_obs3
      print *,swed_mod1,swed_mod2,swed_mod3

c Average these values.
      swed_obs = (swed_obs1 + swed_obs2 + swed_obs3) / 3.0
      swed_mod = (swed_mod1 + swed_mod2 + swed_mod3) / 3.0

      print *
      print *,swed_obs,swed_mod
      print *

c Read in the old cf_precip value.
      open (61,file='../precip_cf/cf_precip.dat')
      read (61,*) cf_precip
      close (61)

c Calculate what the new cf_precip value should be.
      ratio = swed_obs / swed_mod
      cf_precip = cf_precip * ratio
      percent_diff = abs((swed_obs-swed_mod)/swed_mod)*100.0
      print *,cf_precip

c Save a record of what was calculated and done.
      open (71,file='cf_precip_info.dat',access='append')
      write (71,*) swed_obs,swed_mod,ratio,cf_precip,percent_diff
      close (71)

c Write out the new cf_precip value.
      open (61,file='../precip_cf/cf_precip.dat')
      write (61,*) cf_precip
      close (61)

      end

