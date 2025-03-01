
This collection of SnowModel scripts and codes assume you
have the following programs installed on your workstation:

A Fortran 77, 90, or 95 compiler.  The code has been tested
with the Portland Group compiler (pgf77 and pgf90) and the
GNU compiler (gfortran).  We have tried to make sure there is
nothing in the code that requires any special considerations.

All of the projection steps are performed with gdal
(https://gdal.org/).  The sub-programs that are used include:
  gdaltransform
  gdalwarp
  gdal_translate
All of these calls are made from scripts in the /sm/topo_vege/
directory.

The SnowModel code, and the inputs and outputs generally
follow GrADS conventions (http://cola.gmu.edu/grads/).
GrADS is not required to run SnowModel, but it is convenient
to look at the inputs and outputs.

