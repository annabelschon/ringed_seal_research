This document provides a step-by-step recipe for running
SnowModel.

In general, each step points you to a directory that contains
information that must be addressed. And the required details
of what must be done within that directory are described there.

This document outlines the steps to perform a test simulation
over a domain in the Fraser Experimental Forest in Colorado.
Reproducing this simulation is a logical place to start.
After performing this test simulation you will have been
introduced to all the steps required to do a basic SnowModel
simulation.


1) copy_to_new_application.script

I usually edit this file to define the location of my new
SnowModel run and run this script whenever I want to run a new
model simulation. This preserves a record of the projects for
which this version of the model was used within the directory
where I originally unzipped it.


2) dependencies

You should first read readme_docs/1_readme_dependencies.txt
to understand what must be installed on your linux workstation
in order to perform the steps listed below.


3) topo_vege/

This directory contains all of the processing steps for the
topography and vegetation inputs that SnowModel requires. The
contents of this folder include:

   a) asc_to_grads/

   b) NoAm_30m/

   c) readme.txt

The first directory (a in the above list) provides information
on how to convert .asc formatted raster files of topography
and vegetation data into a GrADS binary file.

The readme.txt file (c in the above list) provides further
information.

The topography and vegetation processing is done in the
NoAm_30m/ directory (b in the above list). Here we provide
instruction on using the files within that directory:

  NoAm_30m/

First, read the 1-5 readme files and follow those instructions
to create your topo and vege distributions over your simulation
domain of interest. This generally includes progressing in
this order:

    a) Follow instructions in the data_download/ directory
       to get Glen's topography and vegetation datasets

    b) Modify the SM_dxdy_cornerll_proj_INPUTS.dat file to meet
       your domain specifications

    c) Modify the files within the input_files_info/
       directory as needed

    d) Execute the process_topo_vege.script file

After these steps are all completed, the information contained
in the SM_domain_config_info_OUTPUTS.dat file should define
your simulation domain, and the resulting topo_vege.gdat file
will define your topographic and landcover distributions.

Alternatively, you can generate the dem and landcover
distributions some other way, and/or using other
datasets. Some options for this are described in the
6_readme_other_topo_vege_datasets.txt file.


4) met/

This directory contains all of the processing steps for the
generating the meteorological forcing inputs that SnowModel
requires. The contents of this directory include:

   a) data_download/

   b) instructions specific to several reanalysis datasets
      including:

      i.   era5/   (This covers 10-90 N latitude on a 0.25
           degree grid, and 1 Jan 1979 to present (within a
           few months)

      ii.  merra2/ (This covers 10-90 N latitude on a ~0.5
           degree grid, and 1 Jan 1980 to present (within a
           few months)

      iii. nldas2/ (This covers the lower 48 states on a
           0.125 degree grid, and 1 Jan 1979 to present
           (within a few months)

      iv.  nora10/ (This covers Scandinavia on a 0.1 degree
           grid, and 1 Sep 1957 to present (within a few
           months)

      Each of these archives include 3-hourly and daily
      forcing data required to drive SnowModel (e.g., air
      temperature, relative humidiy, wind speed, wind direction,
      and precipitation)

   c) readme.txt

   d) stations/

The data_download/ directory (a in the above list) provides
instructions for downloading your desired met forcing dataset
from Glen's archives. This is done using a wget script.

The readme.txt file (c in the above list) explains the contents
of this met/ directory in more detail.

The stations/ directory (d in the above list) includes examples
of how to process met forcing data from stations, rather than
from reanalysis datasets. The steps on how this is done can
be found within each individual reanalysis dataset directory
(i through iv above).

These processing steps within each reanalysis dataset directory
read the spatial domain information that you generated in
processing your topography and vegetation information (step
1 above), and create the meteorological forcing required to
do your SnowModel simulation. The readme.txt file in each
reanalysis dataset directory outlines the required steps,
working through sub-directories 1_* through 7_*.

Alternatively, you can generate the met forcing some other
way, and/or using other datasets.


5) code/

The code must be compiled for your machine. You can do this
by editing and running the compile_snowmodel.script in the
code/ directory. The resulting executable (called 'snowmodel') 
is placed in the base sm/ directory.

In general, compiling the code does not have to be done again
unless you make a change to one of the files in this code/
directory; modifications to the snowmodel.par file (discussed
below) do not require recompiling the code.


6) snowmodel.par

This parameter file controls the SnowModel simulation
and allows you to tailor the model specifications and
outputs to fit your specific, unique application. There are
extensive discussions in this .par file that describe what
the options are and what they control. Key changes from the
base configuration that you will need to make include:

   a) defining the path to your topo_vege and met input files
      (see steps 1 and 2 above)

   b) defining the grid configuration of your SnowModel domain,
      which is detailed and easily accesible in the
      SM_domain_config_info_OUTPUTS.dat file you produced in
      step 1.

You must edit and save this .par file; it gets read when
SnowModel is run in step 6 and allows you to ensure the model
is doing what you want it to do.


7) run_snowmodel.script

Edit this file to change the email address you want the
"The SnowModel Run Has Finished" message to be sent to once
your simulation is finished (or comment it out, if you don't
want such a message sent). For this email to be sent, 'mail'
or 'sendmail' must be running on your workstation. It can
also be helpful to modify the email subject to provide more
information, such as "Subject: the 1999-2010 Oregon SnowModel
simulation has finished". It is crucial that the email address
in this script is edited - otherwise you will not receive an
email saying the SnowModel simulation is finished.


8) run SnowModel

The run_snowmodel.script is an executable file that runs the
SnowModel code. To execute it, just type run_snowmodel.script
and hit 'enter'. Running SnowModel this way will create
a snowmodel.err file that will include any errors that
occurred during the model run and will also include the time
it took to do the run. When conducting simulations requiring
lots of CPU time, it can be helpful to run a single year of
simulations, the same domain on a coarser grid, or a subset of
your domain to determine, based on the time it took to do the
pared down run, how long it will likely take to run your full
simulation. This script will also create a snowmodel.list file
that will include anything that the code would normally print
to the screen, and allows you to see the progress of your
simulation. Alternatively, you can also just type snowmodel
and hit 'enter'. This will not record the time it took to
complete your run, and anything printed to the screen will
not be saved in the .list file.

As another run option, you can also run snowmodel with a
.par file that is called something other than snowmodel.par,
and/or with the .par file in another location. This is done by
typing "snowmodel parpath/parname.par", without the quotes,
and hitting enter. You could also modify the /sm/code/utility/
running_SnowModel.script file to do something similar, if you
want to output the .err and .list files for your run with the
different .par name and/or location.


9) example simulation

The example simulation contained in this SnowModel distribution
will output GrADS data files to the sm/outputs/wo_assim/
directory.  The code also writes GrADS control (.ctl) files to
the ctl_files/ directory. The figures/test_example/ directory
contains GrADS plotting scripts and the resulting plots for
this example simulation.  These can be used to compare the
results of your example simulation with our example simulation
results.


10) additional directories, etc.

Other directories that are used for other, often more complex,
SnowModel applications are included in this model distribution.
These include:

   a) extra_met/

   b) hrestart/

   c) post_process/

   d) precip_cf/

   e) readme_docs/

   f) seaice/

   g) swe_assim/

   h) misc_programs/

These directories are not required for this example simulation,
but here is some more information regarding them:

   a) extra_met/

      This directory is used if your domain is large enough
      in the north-south and/or east-west directions to
      require information about the latitude and longitude
      positions of each SnowModel grid cell (domain sizes
      generally greater than 500 km). This is also used
      if you are running SnowModel in 'line' mode (see
      readme_docs/readme_snowmodel_line.txt).

   b) hrestart/

      This directory will contain any history restart files you
      have elected to generate (see the snowmodel.par options).

   c) post_process/

      This directory includes programs that perform a wide
      variety of post-processing steps you might be interested
      in. Including:

         i.   Taking daily SnowModel outputs and creating
              year-specific information, such as maximum
              yearly swe, snow-onset date, snow-free date,
              number of snow days in the year, etc.

         ii.  Assimilating snow density observations.

         iii. Converting from .gdat output files to ascii
              text files or geotif files.

         iv.  Processing multilayer output files.

   d) precip_cf/

      This directory contains any precipitation corrections
      you want to impose on your model simulations. See the
      cf_precip_flag discussion in the snowmodel.par file
      for additional information.

   e) readme_docs/

      This directory contains additional information that
      may be useful for different applications.

   f) seaice/ 

      This directory is where you put your sea-ice
      concentration data file if you are doing a run with sea
      ice (e.g., seaice_run = 1-4 in the snowmodel.par file).

   g) swe_assim/ 

      This directory contains snow water equivalent (swe)
      assimilation input and output files if you have set
      irun_data_assim = 1 in the snowmodel.par file.

   h) misc_programs/

      This directory contains a collection of SnowModel
      simulation support programs that I find I use over
      and over in different applications.  They include:
      Offline versions of the Barnes interpolation program
      used in MicroMet, SnowModel, and SnowAssim.  The
      calendar program used in SnowModel, etc.  The nine-
      point smoother program used in SnowModel.  And an
      example convergence loop program that can be used to
      iteratively converge on some requirement you impose
      on the SnowModel simulation.



