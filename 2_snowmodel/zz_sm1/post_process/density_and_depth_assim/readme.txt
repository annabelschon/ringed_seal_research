The general vision here is that we have done/are doing the
following:

1) swed assim as part of the SnowModel simulation.

2) Assimilate the density (sden) at locations where we have
sden observations and/or the ability to calculate sden from
swed and snod observations.

3) Use the simulated swed and updated sden to calculate a
new depth distribution.

4) Assimilate observed snow depths by calculating snow depth
correction factors, and applying those to the simulated snow
depths.  This takes advantage of snow depth observations that
don't have corresponding sden or swed observations.

5) Use the updated snow density and snow depths to update
the swed distributions, so the three variables are interally
consistent.


Note that these later steps assume there is only one date that
is assimilated (like 1 April).  For an example of assimilating
multiple density observation dates in a year, see
  /hightsi/mosaic_ship/1_sm/19_density_assim/

