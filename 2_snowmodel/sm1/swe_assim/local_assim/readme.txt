The local cf_mask is created with:

barnes_interp_local_cf_mask.f

You will want to look at the outputs and adjust beta to define
how much area you to be set to undef.

The undef area will then have correction factors set to 1.0 in
dataassim_user.f.

For this to work, in dataassim_user.f, you will have to set:

local_assim_flag = 1

and define:

fname_sweobs_barnes_mask = '../swe_obs/2014/barnes/obs.gridded.gdat'

or something.

If you look in the dataassim_user.f code you will see that it
creates cf=1.0 data points every 100 grid cells.  If this doesn't
work for your simulation, you will have to change this.

