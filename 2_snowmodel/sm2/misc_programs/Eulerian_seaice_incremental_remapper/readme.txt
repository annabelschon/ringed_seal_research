This directory contains the code required to run SnowModel's
Eulerian snow-on-sea-ice configuration (seaice_run = 3.0 in
the .par file).

It is my implementation of a code that came from Los Alamos
National Laboratory that implements the incremental remapping
procedures described in Lipscomb and Hunke (2004).  I used
it for a Eulerian snow-on-sea-ice project I had.  I now have
a paper in review that uses a Lagrangian approach and says
(basically) that you can never get the right answer using a
Eulerian approach for snow-on-sea-ice applications.

The code is provided here in case someone wants to come back to
it and use, test, and improve it for some application.  In its
current state, I don’t recommend that anyone else uses it; it
was never tested enough to publish a paper out of the results,
it is really just a ‘development exploration’ effort, and
there is NO documentation about what is does or why.  If you
set seaice_run = 3.0 in the .par file, it will give you a
message that says what you must do to use the remapping codes.

Note that there is also a different ‘compile.script’ if
you want to do a remapping run.  So, you will have to do some
work to use this option.

Other important information includes: A sea-ice run with
incremental remapping to handle the ice motion (seaice_run =
3.0) requires compiling with a Fortran 90 compiler, see the
"compile_snowmodel_w-remapper.script", and it also requires
nx = nx_max and ny = ny_max in snowmodel.inc.

Lipscomb, W. H., and E. C. Hunke (2004), Modeling sea ice
transport using incremental remapping. Mon. Weather Rev., 132,
1341-1354.
https://doi.org/10.1175/1520-0493(2004)132<1341:MSITUI>2.0.CO;2


To get this working again, you (at least) have to:

1) Move the '/remapper/' directory to the /code/ directory,
so it looks like: /code/remapper/

2) Move the 'compile_snowmodel_w-remapper.script' to the /code/
directory and use it to compile SnowModel.

3) Set seaice_run = 3.0 in the .par file.

4) Make the following code changes:

  a) Uncomment the following in snowmodel_main.f:

c If this is a sea ice run with incremental remapping, perform the
c   remapping here, before any data is written out for this time step.
c   Note that the incremental remapping programs must be compiled with
c   pgf90 or gfortran.
c         if (seaice_run.eq.3.0) then
c           call remapper_main(iter,nx,ny,seaice_conc,swe_depth,dt,
c    &        deltax,deltay)
c         endif

  b) Comment out the following in preprocess_code.f:

c seaice_run = 3.0 is no longer supported.  If seaice_run has been
c   set to 3.0 in the .par file, send a message describing your
c   options.
      if (seaice_run.eq.3.0) then
        print *, 'Eulerian sea ice runs are not longer supported'
        print *, '(seaice_run = 3.0).  If you want to restart this'
        print *, 'option, see the notes in here:'
        print *, '/sm/misc_programs/Eulerian_incremental_remapper/'
        stop
      endif

