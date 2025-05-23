c snowmodel.inc

      integer nx_max,ny_max,nstns_max,nvegtypes,max_time_steps,
     &  max_obs_dates,nz_max,n_print_vars

c THESE FIRST THREE PARAMETER VALUES OFTEN NEED TO BE CHANGED
c   FOR BIG AND LONG MODEL SIMULATIONS.

c nx_max and ny_max define the maximum nx and ny grid points
c   that you can have in your model run.  If you want to run a
c   larger domain, then you have to change these numbers and
c   recompile the code.
      parameter (nx_max=1001,ny_max=1001)

c max_time_steps defines the maximum number of time steps that
c   will be used in the current compliled version of the code.
c   If you want to run a longer time domain, then you have to
c   change this number and recompile the code.
      parameter (max_time_steps=125650)

c nstns_max is the maximum number of stations that can be used
c   in the data assimilation routines.
      parameter (nstns_max=30000)

c max_obs_dates is used in the data assimilation routines.  It
c   must be greater than or equal to the number of observation
c   dates in the entire simulation + (plus) the number of years
c   in the simulation.  For example, for a 6-year simulation with
c   2 observation dates in each year, you would set max_obs_dates
c   to be = 2obs*6yrs+6yrs = 18 or greater.  For a 6-year run with
c   4 observation dates in 2 of the years, and 0 observation dates
c   in the other 4 years, max_obs_dates = 8obs+6yrs = 14 or
c   greater.
      parameter (max_obs_dates=12)

c If you are running the multi-layer snow model (even with a single
c   layer) nz_max must be at least one greater than max_layers in
c   snowmodel.par.  This is because the model will build a new layer
c   with the new snowfall and then it is merged with the layer below
c   if you only want a single snow layer.  If you are running
c   SnowModel's original single layer model, nz_max can be 1 (but if
c   nz_max=2 it will avoid a warning message if you are compiling
c   the code with gfortran, and some compilers require you to set
c   nz_max=3 to avoid these warning messages).  So, even if you are
c   doing a single layer run, it is fine to just leave nz_max=3.
c     parameter (nz_max=25)
      parameter (nz_max=3)

c This is the number of print variables that are controlled by
c   the variable list in snowmodel.par.
      parameter (n_print_vars=30)

c nvegtypes is the number of vegetation types used in the model
c   run.  If you change this then you have made some big changes
c   in the codes' vegetation description.
      parameter (nvegtypes=30)

