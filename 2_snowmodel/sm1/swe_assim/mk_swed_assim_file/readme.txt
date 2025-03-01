The SNOTEL and SNOW COURSE archive is updated here:

/snotel_snocrs_archive/

You can look in:

/snotel_snocrs_archive/readme.txt

to see the archive time span.


Steps 0-9 in this directory must be run for your particular
SnowModel simulation.


CRITICAL EDITS MUST BE MADE TO:

/0_define_sm_temporal_domain/sm_temporal_domain.dat

/6_clean_data/1_clean_swe_obs.f

/8_extract_1st_mo_swe_obs/1_monthly_snotel_snocourse_swe_obs.f

Otherwise, everthing just works without any intervention.


NOTE FOR SIMULATIONS WITH A PARTIAL FINAL YEAR:

This code should be general enough to handle the case of a
partial year for the last year.

