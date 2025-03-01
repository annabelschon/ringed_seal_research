
Here we are creating two files:

=============================================================
=============================================================
=============================================================

First, create a file that points to the SNOTEL archive by
copying either:

snotel_archive_path.dat.adele
snotel_archive_path.dat.glen

to:

snotel_archive_path.dat

=============================================================
=============================================================
=============================================================

Second, create a file that defines the SM temporal domain.

Here we are defining the SnowModel temporal simulation domain
by listing the start and end dates in the file:

sm_temporal_domain.dat

The required information is:

c This is the beginning date of the SnowModel simulation.
      iyr_start = 1980
      imo_start = 9
      idy_start = 1

c This is the end date of the SnowModel simulation.
      iyr_end = 2020
      imo_end = 8
      idy_end = 31

The format of the file should look like (without the "***"s):

************************************************************
c This is the beginning date of the SnowModel simulation.
1980
9
1

c This is the end date of the SnowModel simulation.
2020
8
31
************************************************************

