c 4_find_outside_domain.f

c This is just here if you want to see if you have any GPS points
c   that are outside the simulation domain in space and or time.

c You could read in this to get the domain coordinates:

c /sm/topo_vege/NoAm_30m/process_data/1_topo/outputs/
c   3_cropping_coords.dat

c And this to get the time domain:

c /sm/met/merra2/4_maxiter_offset/
c   start_end_dates_maxiter_ioffset.dat

c And then compare the locations with these:

c /sm/figures/movie_w-GPS_locations/1_mk_gps_data/2_reproject/
c   proj_points.dat

c And the dates with these:

c /sm/figures/movie_w-GPS_locations/1_mk_gps_data/1_orig_gps_data/
c   AMRO_GPS_2018_orig.dat


      end

