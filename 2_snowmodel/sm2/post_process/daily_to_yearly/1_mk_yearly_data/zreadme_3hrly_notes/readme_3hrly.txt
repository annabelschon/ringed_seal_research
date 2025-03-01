This provides a way to deal with variables like swed,
snod, and sden, where you are extracting variables at
the end of the day (not averaging or summing over the
day).  To do this, look at the following in this file:
1_mk_yearly_variables_from_3hrly_data.f

c If the SnowModel run output 3-hourly data, then daily data
c   must be produced for input into the following subroutines
c   (they assume daily input data).  Define the information
c   required to make daily data out of 3-hourly data.
      dt = 10800.0
      print_inc = 1.0

